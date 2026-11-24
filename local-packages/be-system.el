;;; be-system.el --- Portable executable installation -*- lexical-binding: t; -*-
;;; Commentary:
;; Keep executable names at call sites; distribution package names belong here.
;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defvar be/allow-tool-installation t
  "Dynamically bound to nil while configuring a feature from a remote buffer.")

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
          (be/run-installer (append (list "sh" script) args)))
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
    ((or "ruff" "isort" "pyright-langserver")
     (unless (be/ensure-uv) (error "uv is unavailable"))
     (list "uv" "tool" "install"
           (pcase name ("pyright-langserver" "pyright") (_ name))))
    ("typescript-language-server"
     (unless (and (be/ensure-tool 'node) (be/ensure-tool 'npm))
       (error "Node and npm are unavailable"))
     (list "npm" "install" "--global" "--prefix" (expand-file-name "~/.local")
           "typescript-language-server" "typescript"))
    ("helm_ls"
     (error "Install helm-ls from https://github.com/mrjosh/helm-ls/releases and put helm_ls on PATH"))
    ("terraform-ls"
     (unless (be/ensure-tool 'go) (error "Go is unavailable"))
     '("go" "install" "github.com/hashicorp/terraform-ls@latest"))
    ("rust-analyzer"
     (unless (be/ensure-tool 'rustup) (error "rustup is unavailable"))
     '("rustup" "component" "add" "rust-analyzer"))
    ("tinymist"
     (unless (be/ensure-tool 'cargo) (error "cargo is unavailable"))
     '("cargo" "install" "--locked" "tinymist-cli"))
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
    (gopls "gopls" "golang-x-tools-gopls" "gopls")
    (sbcl "sbcl" "sbcl" "sbcl")
    (shfmt "shfmt" "shfmt" "shfmt")
    (wl-copy "wl-clipboard" "wl-clipboard" nil))
  "Executable and package names for apt, dnf, and Homebrew respectively.")

(defun be/system-install-command (executable &optional package)
  "Return an argument list to install EXECUTABLE or explicit PACKAGE."
  (let* ((manager (be/system-manager))
         (entry (assq (intern executable) be/system-packages))
         (name (or package (nth (pcase manager ('apt 1) ('dnf 2) (_ 3)) entry)
                   executable)))
    (pcase manager
      ('brew (list "brew" "install" name))
      ('apt (append (unless (zerop (user-uid)) '("sudo")) (list "apt-get" "install" "-y" name)))
      ('dnf (append (unless (zerop (user-uid)) '("sudo")) (list "dnf" "install" "-y" name)))
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

(defun be/ensure-tool (executable &optional installer)
  "Ensure EXECUTABLE exists, using INSTALLER when supplied.
INSTALLER is a package symbol or shell command string.  Return nil with a
warning when installation fails, so ordinary editing remains available."
  (let ((name (if (symbolp executable) (symbol-name executable) executable)))
    (or (executable-find name)
        (when (and be/allow-tool-installation (not (file-remote-p default-directory)))
          (condition-case err
              (progn
                (cond
                 ((equal name "uv") (be/ensure-uv))
                 ((member name '("rustup" "cargo"))
                  (be/install-user-script "https://sh.rustup.rs" "-y" "--no-modify-path" "--profile" "minimal"))
                 (t (be/run-installer
                     (cond ((stringp installer)
                            (list shell-file-name shell-command-switch installer))
                           ((null installer)
                            (or (be/tool-installer name) (be/system-install-command name)))
                           (t (be/system-install-command name (symbol-name installer)))))))
                (or (executable-find name)
                    (error "%s still missing from PATH after installation" name)))
            (error
             (display-warning 'be-system (error-message-string err) :warning)
             nil))))))

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
  "Enable portable, synchronous :ensure-system-package support."
  (require 'use-package-ensure-system-package)
  (advice-add 'use-package-handler/:ensure-system-package :override
              #'be/use-package-system-handler))

(provide 'be-system)
;;; be-system.el ends here
