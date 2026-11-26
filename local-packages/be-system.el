;;; be-system.el --- Portable executable installation -*- lexical-binding: t; -*-
;;; Commentary:
;; Keep executable names at call sites; distribution package names belong here.
;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'be-toolchains)

(defun be/tool-paths ()
  "Expose user-local tools while preserving project executables' precedence."
  (dolist (directory '("~/.local/bin" "~/.cargo/bin" "~/.ghcup/bin" "~/.cabal/bin" "~/go/bin"))
    (let ((path (expand-file-name directory)))
      (unless (member path exec-path) (setq exec-path (append exec-path (list path))))
      (unless (member path (split-string (or (getenv "PATH") "") path-separator))
        (setenv "PATH" (concat (getenv "PATH") path-separator path))))))

(defvar be/allow-tool-installation t
  "Dynamically bound to nil while configuring a feature from a remote buffer.")
(defvar be/tool-extra-requirements (make-hash-table :test 'equal))

(defun be/with-build-limits (function &rest args)
  "Call FUNCTION with ARGS and bounded subprocess build concurrency."
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "MAKEFLAGS" "-j2")
    (setenv "CMAKE_BUILD_PARALLEL_LEVEL" "2")
    (setenv "CARGO_BUILD_JOBS" "2")
    (setenv "GOMAXPROCS" "2")
    (apply function args)))

(defun be/install-user-script (url &rest args)
  "Download upstream installer URL to a temporary file and run it with ARGS."
  (require 'url-handlers)
  (let ((script (make-temp-file "emacs-installer-")))
    (unwind-protect
        (progn
          (url-copy-file url script t)
          (be/run-installer (append (list "bash" script) args)))
      (delete-file script))))

(defun be/ensure-uv ()
  "Install uv in the user account using its upstream installer."
  (or (executable-find "uv")
      (when (and be/allow-tool-installation (not (file-remote-p default-directory)))
        (be/install-user-script "https://astral.sh/uv/install.sh")
        (executable-find "uv"))))

(defun be/tool-installer (name)
  "Return a user-local installer command for NAME, when appropriate."
  (pcase name
    ((or "ruff" "isort" "black" "sqlfluff" "cmake-format")
     (unless (be/ensure-uv) (error "uv is unavailable"))
     (list "uv" "tool" "install"
           name))
    ("terraform-ls"
     (unless (be/ensure-tool 'go) (error "Go is unavailable"))
     '("go" "install" "github.com/hashicorp/terraform-ls@latest"))
    ((or "rust-analyzer" "rustfmt")
     (unless (be/ensure-tool 'rustup) (error "rustup is unavailable"))
     (list "rustup" "component" "add" name))
    ("goimports"
     '("go" "install" "golang.org/x/tools/cmd/goimports@latest"))
    ("aider"
     '("uv" "tool" "install" "--force" "--with-pip" "aider-chat@latest"))
    (_ nil)))

(defun be/system-manager ()
  "Return the available native package manager."
  (cond ((eq system-type 'darwin)
         (and (executable-find "brew") 'brew))
        ((executable-find "apt-get") 'apt)
        ((executable-find "dnf") 'dnf)))

(defconst be/system-packages
  '((rg "ripgrep" "ripgrep" "ripgrep")
    (gpg "gnupg" "gnupg2" "gnupg")
    (cc "build-essential" "gcc" "llvm")
    (c++ "g++" "gcc-c++" "llvm")
    (make "make" "make" "make")
    (cmake "cmake" "cmake" "cmake")
    (libtool "libtool-bin" "libtool" "libtool")
    (node "nodejs" "nodejs" "node")
    (npm "npm" "npm" "node")
    (go "golang-go" "golang" "go")
    (gofmt "golang-go" "golang" "go")
    (gopls "gopls" "golang-x-tools-gopls" "gopls")
    (sbcl "sbcl" "sbcl" "sbcl")
    (shfmt "shfmt" "shfmt" "shfmt")
    (python3 "python3" "python3" "python@3.13")
    (racket "racket" "racket" "racket")
    (dot "graphviz" "graphviz" "graphviz")
    (psql "postgresql-client" "postgresql" "libpq")
    (unzip "unzip" "unzip" "unzip")
    (xz "xz-utils" "xz" "xz")
    (bzip2 "bzip2" "bzip2" "bzip2")
    (clang-format "clang-format" "clang-tools-extra" "clang-format")
    (wl-copy "wl-clipboard" "wl-clipboard" nil))
  "Executable and package names for apt, dnf, and Homebrew respectively.")

(defun be/system-install-command (executable &optional package)
  "Return an argument list to install EXECUTABLE or explicit PACKAGE."
  (let* ((manager (be/system-manager))
         (entry (assq (intern executable) be/system-packages))
         (name (or package (nth (pcase manager ('apt 1) ('dnf 2) (_ 3)) entry)
                   executable)))
    (pcase manager
      ('brew (append '("brew" "install") (ensure-list name)))
      ('apt (append (unless (zerop (user-uid)) '("sudo")) '("apt-get" "install" "-y") (ensure-list name)))
      ('dnf (append (unless (zerop (user-uid)) '("sudo")) '("dnf" "install" "-y") (ensure-list name)))
      (_ (error "Install %s and a supported manager (brew, apt-get, dnf)" executable)))))

(defun be/run-installer (command)
  "Run COMMAND synchronously, preserving diagnostic output on failure."
  (let ((buffer (get-buffer-create "*Emacs dependency installation*"))
        (default-directory (expand-file-name "~")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (format "\n$ %s\n" (mapconcat #'shell-quote-argument command " "))))
    ;; TRAMP handles the sudo password; never put it in command text or argv.
    (when (equal (car command) "sudo")
      (setq default-directory "/sudo::/" command (cdr command)))
    (unless (zerop (apply #'be/with-build-limits #'process-file (car command) nil buffer t (cdr command)))
      (display-buffer buffer)
      (error "Installation failed; see %s" (buffer-name buffer)))))

(defun be/ensure-tool (executable &optional installer callback)
  "Ensure EXECUTABLE exists, using INSTALLER when supplied.
INSTALLER is a package symbol or shell command string.  Return nil with a
warning when installation fails, so ordinary editing remains available.
In interactive Emacs, enqueue missing tools and return nil immediately;
call CALLBACK without arguments after the tool is ready.  Batch installers
and validation run synchronously so their exit status reports failures."
  (let ((name (if (symbolp executable) (symbol-name executable) executable)))
    (be/tool-paths)
    (or (be/tool-present-p name)
        (when (and be/allow-tool-installation (not (file-remote-p default-directory)))
          (if (not noninteractive)
              (progn
                (require 'be-install)
                (be/queue-tool name installer callback)
                nil)
          (condition-case err
              (progn
                (dolist (dependency (be/tool-requirements name))
                  (unless (be/ensure-tool dependency)
                    (error "Prerequisite %s is unavailable for %s" dependency name)))
                (cond
                 ((string-prefix-p "grammar:" name)
                  (require 'be-git) (require 'be-assets)
                  (unless (be/ensure-grammar (intern (substring name 8)))
                    (error "Grammar installation failed: %s" name)))
                 ((string-prefix-p "@" name)
                  (be/run-installer (be/tool-library-command name))
                  (make-directory (file-name-directory (be/tool-marker name)) t)
                  (write-region "installed\n" nil (be/tool-marker name) nil 'silent))
                 ((equal name "uv") (be/ensure-uv))
                 ((member name '("rustup" "cargo"))
                  (be/install-user-script "https://sh.rustup.rs" "-y" "--no-modify-path" "--profile" "minimal"))
                 ((and (null installer) (member name be/managed-tools)) (be/install-managed-tool name))
                 (t (be/run-installer
                     (cond ((stringp installer)
                            (list shell-file-name shell-command-switch installer))
                           ((null installer)
                            (or (be/tool-installer name) (be/system-install-command name)))
                           (t (be/system-install-command name (symbol-name installer)))))))
                (or (be/tool-present-p name)
                    (error "%s still missing from PATH after installation" name)))
            (error
             (display-warning 'be-system (error-message-string err) :warning)
             nil)))))))

(defconst be/tool-dependencies
  '(("jdtls" "python3" "tar") ("tinymist" "tar") ("typst" "tar" "xz")
    ("fourmolu" "unzip") ("omnisharp" "dotnet11" "tar")
    ("dotnet" "curl" "@dotnet") ("cs" "gzip")
    ("dotnet11" "curl" "@dotnet")
    ("csharpier" "dotnet11") ("google-java-format" "tar")
    ("dhall" "tar" "bzip2")
    ("metals" "cs") ("sbt" "cs") ("scalafmt" "cs")
    ("ghcup" "curl" "@ghcup") ("ghc" "ghcup") ("cabal" "ghcup")
    ("stack" "ghcup") ("haskell-language-server-wrapper" "ghcup" "ghc")
    ("ruff" "uv") ("isort" "uv") ("black" "uv") ("sqlfluff" "uv")
    ("cmake-format" "uv") ("pyright-langserver" "tar") ("aider" "uv")
    ("node" "tar") ("vterm-module" "cmake" "libtool" "cc" "make")
    ("typescript-language-server" "tar") ("yaml-language-server" "tar")
    ("elm-language-server" "tar") ("elm" "tar")
    ("elm-format" "tar") ("prettier" "tar") ("mmdc" "tar" "@mermaid")
    ("biome" "tar")
    ("dprint" "tar")
    ("terraform-ls" "go") ("goimports" "go") ("gofmt" "go")
    ("rust-analyzer" "rustup") ("rustfmt" "rustup")))

(defconst be/tool-libraries
  '(("@dotnet" ("libicu-dev") ("libicu") nil)
    ("@mermaid" ("libasound2-dev" "libatk-bridge2.0-0" "libatk1.0-0" "libcups2"
                  "libdbus-1-3" "libgbm1" "libgtk-3-0" "libnss3" "libx11-xcb1"
                  "libxcomposite1" "libxdamage1" "libxrandr2" "fonts-liberation")
     ("alsa-lib" "atk" "at-spi2-atk" "cups-libs" "dbus-libs" "mesa-libgbm"
      "gtk3" "nss" "libX11" "libXcomposite" "libXdamage" "libXrandr" "liberation-fonts") nil)
    ("@ghcup" ("build-essential" "libgmp-dev" "libffi-dev" "libncurses-dev" "libnuma-dev" "xz-utils" "pkg-config")
     ("gcc" "gcc-c++" "make" "gmp-devel" "libffi-devel" "ncurses-devel" "numactl-devel" "xz" "pkgconf-pkg-config")
     ("gmp" "libffi" "ncurses" "xz" "pkgconf")))
  "Native shared libraries needed by private SDK distributions.
Private completion markers avoid reinstalling a package bundle at every startup.")

(defun be/tool-requirements (name)
  "Return installation prerequisites for executable NAME."
  (append (when (string-prefix-p "grammar:" name) '("git" "cc"))
          (cdr (assoc name be/tool-dependencies))
          (gethash name be/tool-extra-requirements)
          (when (and (equal name "dhall") (eq system-type 'gnu/linux)
                     (string-match-p "\\`\\(?:aarch64\\|arm64\\)" system-configuration))
            '("cabal" "ghc"))))

(defun be/tool-marker (name)
  "Return the private marker for a native dependency bundle NAME."
  (expand-file-name (format "libraries-%s-%s" name (be/system-manager))
                    (be/tool-directory "state")))

(defun be/tool-present-p (name)
  "Test executable NAME or an internal native-library bundle."
  (cond ((string-prefix-p "@" name) (file-exists-p (be/tool-marker name)))
        ((string-prefix-p "grammar:" name)
         (require 'be-assets)
         (be/grammar-ready-p (intern (substring name 8))))
        ((equal name "vterm-module")
         (when-let ((library (locate-library "vterm")))
           (and module-file-suffix
                (file-exists-p (expand-file-name (concat "vterm-module" module-file-suffix)
                                                (file-name-directory library))))))
        ((equal name "dotnet11")
         (let ((private (expand-file-name "dotnet" (be/tool-directory "dotnet"))))
           (cl-find-if
            (lambda (dotnet)
              (when (and dotnet (file-executable-p dotnet))
                (let ((sdks (expand-file-name "sdk" (file-name-directory (file-truename dotnet)))))
                  (and (file-directory-p sdks)
                       (cl-some (lambda (version) (>= (string-to-number version) 11))
                                (directory-files sdks nil "\\`[0-9]+\\.[0-9]"))))))
            (list private (executable-find "dotnet")))))
        (t (executable-find name))))

(defun be/tool-library-command (name)
  "Return the native installation command for library bundle NAME."
  (let* ((manager (be/system-manager))
         (packages (nth (pcase manager ('apt 1) ('dnf 2) ('brew 3))
                        (assoc name be/tool-libraries))))
    (unless manager (error "No native manager available for %s" name))
    (if packages (be/system-install-command name packages) '("true"))))

(autoload 'be/install-tool "be-install" nil t)

(defun be/command-tools (command tools)
  "Run COMMAND after TOOLS are ready, without holding its calling buffer.
If the buffer changes during installation, ask the user to invoke the command
again rather than replaying an old action against newer contents."
  (advice-add
   command :around
   (lambda (function &rest args)
     (if (or (file-remote-p default-directory) (not be/allow-tool-installation))
         (apply function args)
       (let ((buffer (current-buffer)) (tick (buffer-chars-modified-tick)) done)
         (cl-labels ((resume ()
                       (when (and (not done) (buffer-live-p buffer)
                                  (cl-every #'be/tool-present-p tools))
                         (setq done t)
                         (with-current-buffer buffer
                           (if (= tick (buffer-chars-modified-tick))
                               (apply function args)
                             (message "Tools ready; invoke %s again in the updated buffer" command))))))
           (dolist (tool tools) (be/ensure-tool tool nil #'resume))
           (if (cl-every #'be/tool-present-p tools)
               (progn (setq done t) (apply function args))
             (message "%s will be available after tool installation" command))))))))

(defun be/use-package-system-handler (name _keyword requirements rest state)
  "Expand normalized system REQUIREMENTS for use-package NAME."
  (use-package-concat
   (mapcar (lambda (requirement)
             (let* ((check (car requirement))
                    (form (cdr requirement))
                    (installer (cadr form)))
               `(be/ensure-tool ',check
                                ,(if (eq (car form) 'system-packages-install)
                                     (unless (equal installer (if (symbolp check) (symbol-name check) check))
                                       `',(intern installer))
                                   installer))))
           requirements)
   (use-package-process-keywords name rest state)))

(defun be/system-setup ()
  "Enable portable :ensure-system-package support with interactive background jobs."
  (require 'use-package-ensure-system-package)
  (advice-add 'use-package-handler/:ensure-system-package :override
              #'be/use-package-system-handler))

(provide 'be-system)
;;; be-system.el ends here
