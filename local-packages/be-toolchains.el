;;; be-toolchains.el --- User-local upstream CLI distributions -*- lexical-binding: t; -*-
;;; Commentary:
;; These installers run in a bounded background Emacs, never the editor thread.
;;; Code:
(defvar url-request-extra-headers)
(defvar url-http-response-status)
(defvar vterm-always-compile-module)

(defconst be/npm-tools
  '(("typescript-language-server" "typescript-language-server" "typescript")
    ("yaml-language-server" "yaml-language-server")
    ("elm-language-server" "@elm-tooling/elm-language-server" "elm" "elm-format")
    ("elm" "elm") ("elm-format" "elm-format") ("prettier" "prettier")
    ("dprint" "dprint") ("mmdc" "@mermaid-js/mermaid-cli")
    ("biome" "@biomejs/biome")
    ("pyright-langserver" "pyright")))

(defconst be/managed-tools
  (append '("tinymist" "typst" "helm_ls" "jdtls" "omnisharp" "dotnet" "dotnet11" "cs" "node"
    "metals" "sbt" "scalafmt" "ghcup" "ghc" "cabal" "stack"
    "haskell-language-server-wrapper" "fourmolu" "google-java-format" "csharpier" "typstyle" "dhall" "vterm-module")
          (mapcar #'car be/npm-tools)))

(defun be/tool-directory (name)
  "Return the private installation directory for NAME."
  (expand-file-name (concat "~/.local/share/emacs-tools/" name "/")))

(defun be/tool-platform ()
  "Return the upstream OS and architecture names for this machine."
  (list (pcase system-type ('darwin "darwin") ('gnu/linux "linux")
               (_ (error "Unsupported CLI platform: %s" system-type)))
        (cond ((string-match-p "\\`\\(?:aarch64\\|arm64\\)" system-configuration) "aarch64")
              ((string-prefix-p "x86_64" system-configuration) "x86_64")
              (t (error "Unsupported CLI architecture: %s" system-configuration)))))

(defun be/tool-wrapper (name command)
  "Expose COMMAND as NAME without changing global SDK environment variables."
  (let ((file (expand-file-name (concat "~/.local/bin/" name))) temporary)
    (make-directory (file-name-directory file) t)
    (setq temporary (make-temp-file (expand-file-name ".wrapper-" (file-name-directory file))))
    (with-temp-file temporary
      (insert "#!/usr/bin/env bash\nset -euo pipefail\nexec "
              (mapconcat #'shell-quote-argument command " ") " \"$@\"\n"))
    (set-file-modes temporary #o755)
    (rename-file temporary file t)
    file))

(defun be/upstream-text (url)
  "Retrieve a small upstream version manifest from URL."
  (require 'url)
  (let ((buffer (url-retrieve-synchronously url t t 30)))
    (unless buffer (error "Cannot retrieve %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (unless (= url-http-response-status 200) (error "HTTP %s from %s" url-http-response-status url))
          (goto-char (point-min)) (re-search-forward "\r?\n\r?\n")
          (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer buffer))))

(defun be/node-runtime ()
  "Return a private Node 24 runtime for editor tools, preserving project Node."
  (or (when (file-directory-p (be/tool-directory "node-24"))
        (car (directory-files-recursively (be/tool-directory "node-24") "/bin/node\\'")))
      (pcase-let* ((`(,os ,arch) (be/tool-platform))
                   (base "https://nodejs.org/dist/latest-v24.x/")
                   (manifest (be/upstream-text (concat base "SHASUMS256.txt")))
                   (regexp (format "\\([0-9a-f]+\\) +\\(node-v[0-9.]+-%s-%s\\.tar\\.gz\\)"
                                   os (if (equal arch "x86_64") "x64" "arm64"))))
        (unless (string-match regexp manifest) (error "No Node 24 distribution for %s/%s" os arch))
        (let ((digest (match-string 1 manifest)) (filename (match-string 2 manifest)))
          (be/extract-tool "node-24" (concat base filename) filename "node" (concat "sha256:" digest))))))

(defun be/install-npm-tool (name)
  "Install npm tool NAME with private Node and expose a version-safe launcher."
  (let* ((node (be/node-runtime))
         (npm (expand-file-name "../lib/node_modules/npm/bin/npm-cli.js" (file-name-directory node)))
         (prefix (be/tool-directory "npm"))
         (process-environment (copy-sequence process-environment)))
    (setenv "PATH" (concat (file-name-directory node) path-separator (getenv "PATH")))
    (be/run-installer (append (list node npm "install" "--global" "--prefix" prefix)
                              (cdr (assoc name be/npm-tools))))
    (let ((programs (pcase name
                      ("typescript-language-server" '("typescript-language-server" "tsc" "tsserver"))
                      ("elm-language-server" '("elm-language-server" "elm" "elm-format"))
                      ("pyright-langserver" '("pyright" "pyright-langserver"))
                      (_ (list name)))))
      (dolist (program programs)
        (let ((binary (expand-file-name (concat "bin/" program) prefix)))
          (unless (file-exists-p binary) (error "npm installed no %s executable" program))
          ;; Some npm packages expose native binaries, not JavaScript launchers.
          (be/tool-wrapper
           program
           (let ((target (file-truename binary)))
             (if (with-temp-buffer
                   (insert-file-contents-literally target nil 0 128)
                   (string-match-p "\\`#![^\n]*\\bnode\\b" (buffer-string)))
                 (list node target)
               (list target)))))))))

(defun be/github-asset (repository regexp)
  "Return the latest stable release asset of REPOSITORY matching REGEXP."
  (require 'url)
  (let ((buffer (let* ((token (or (getenv "GH_TOKEN") (getenv "GITHUB_TOKEN")))
                      (url-request-extra-headers
                       (when token (list (cons "Authorization" (concat "Bearer " token))))))
                  (url-retrieve-synchronously
                   (concat "https://api.github.com/repos/" repository "/releases/latest") t t 30))))
    (if (not buffer) (be/github-public-asset repository regexp)
      (unwind-protect
        (with-current-buffer buffer
          (if (/= url-http-response-status 200)
              (progn
                (message "GitHub API HTTP %s; using public release assets for %s" url-http-response-status repository)
                (be/github-public-asset repository regexp))
          (goto-char (point-min))
          (re-search-forward "\r?\n\r?\n")
          (or (cl-find-if
               (lambda (asset) (string-match-p regexp (alist-get 'name asset)))
               (alist-get 'assets (json-parse-buffer :object-type 'alist :array-type 'list
                                                    :null-object nil :false-object nil)))
              (error "No %s release asset matching %s" repository regexp))))
        (kill-buffer buffer)))))

(defun be/github-public-asset (repository regexp)
  "Resolve a public release without API credentials or API rate-limit capacity."
  (let* ((base (concat "https://github.com/" repository "/releases/"))
         (page (be/upstream-text (concat base "latest")))
         (tag-pattern (concat "/" (regexp-quote repository) "/releases/tag/\\([^\" <>]+\\)")))
    (unless (string-match tag-pattern page) (error "Cannot discover release tag for %s" repository))
    (let* ((tag (match-string 1 page))
           (assets (be/upstream-text (concat base "expanded_assets/" tag)))
           (pattern (concat "href=\"\\(/" (regexp-quote repository) "/releases/download/[^\"]+\\)\""))
           (start 0) found)
      (while (and (not found) (string-match pattern assets start))
        (let* ((path (match-string 1 assets))
               (end (match-end 0))
               (filename (url-unhex-string (file-name-nondirectory path))))
          (setq start end)
          (when (string-match-p regexp filename)
            (setq found `((name . ,filename) (browser_download_url . ,(concat "https://github.com" path)))))))
      (or found (error "No %s public release asset matching %s" repository regexp)))))

(defun be/extract-tool (name url filename executable &optional digest)
  "Install archive URL for NAME and return its EXECUTABLE path.
FILENAME determines the archive format.  Validate DIGEST when GitHub supplies it.
Only replace an earlier private installation after extraction finds the binary."
  (require 'url-handlers)
  (let* ((target (be/tool-directory name))
         (parent (file-name-directory (directory-file-name target)))
         (archive (make-temp-file "emacs-tool-" nil (concat "-" filename)))
         staging)
    (make-directory parent t)
    (setq staging (make-temp-file (expand-file-name ".install-" parent) t))
    (unwind-protect
        (progn
          (url-copy-file url archive t)
          (when (and digest (string-prefix-p "sha256:" digest))
            (unless (equal (substring digest 7)
                           (with-temp-buffer
                             (insert-file-contents-literally archive)
                             (secure-hash 'sha256 (current-buffer))))
              (error "Checksum mismatch for %s" name)))
          (cond ((string-suffix-p ".zip" filename)
                 (be/run-installer (list "unzip" "-q" archive "-d" staging)))
                ((string-match-p "\\.tar\\.\\(?:gz\\|xz\\|bz2\\)\\'" filename)
                 (be/run-installer (list "tar" "-xf" archive "-C" staging)))
                ((string-suffix-p ".gz" filename)
                 (with-temp-buffer
                   (set-buffer-multibyte nil)
                   (let ((coding-system-for-read 'no-conversion))
                   (unless (zerop (call-process "gzip" nil t nil "-dc" archive))
                     (error "Cannot decompress %s" name)))
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region (point-min) (point-max) (expand-file-name executable staging)))))
                (t (copy-file archive (expand-file-name executable staging) t)))
          (let ((binary (car (directory-files-recursively
                              staging (concat "\\(?:\\`\\|/\\)" (regexp-quote executable) "\\'")))))
            (unless binary (error "%s archive has no %s" name executable))
            (set-file-modes binary #o755)
            (let ((relative (file-relative-name binary staging)))
              (when (file-directory-p target) (delete-directory target t))
              (rename-file staging (directory-file-name target))
              (setq staging nil)
              (expand-file-name relative target))))
      (delete-file archive)
      (when staging (delete-directory staging t)))))

(defun be/install-release-tool (name repository regexp executable)
  "Install NAME from a matching official GitHub release and expose its binary."
  (let* ((asset (be/github-asset repository regexp))
         (binary (be/extract-tool name (alist-get 'browser_download_url asset)
                                 (alist-get 'name asset) executable (alist-get 'digest asset))))
    (be/tool-wrapper name (list binary))))

(defun be/java-runtime (&optional minimum)
  "Return Java MINIMUM or newer without replacing a project's Java.
MINIMUM defaults to 21; each runtime is cached separately."
  (let* ((minimum (or minimum 21))
         (runtime (format "jdk-%s" minimum))
         (java (executable-find "java"))
         (native
          (and java
             (with-temp-buffer
               (and (zerop (call-process java nil '(t t) nil "-XshowSettings:properties" "-version"))
                    (goto-char (point-min))
                    (re-search-forward "version \"\\([0-9]+\\)" nil t)
                    (>= (string-to-number (match-string 1)) minimum)
                    (goto-char (point-min))
                    (re-search-forward "^[ \t]*java\\.home = \\(.*\\)$" nil t)
                    (let ((binary (expand-file-name "bin/java" (string-trim (match-string 1)))))
                      (and (file-executable-p binary) binary)))))))
    (if native native
      (or (when (file-directory-p (be/tool-directory runtime))
            (car (directory-files-recursively (be/tool-directory runtime) "/bin/java\\'")))
          (pcase-let ((`(,os ,arch) (be/tool-platform)))
            (be/extract-tool
             runtime
             (format "https://api.adoptium.net/v3/binary/latest/%s/ga/%s/%s/jdk/hotspot/normal/eclipse"
                     minimum (if (equal os "darwin") "mac" os)
                     (if (equal arch "x86_64") "x64" arch))
             "jdk.tar.gz" "java"))))))

(defun be/install-managed-tool (name)
  "Install the official user-local distribution of NAME."
  (pcase-let ((`(,os ,arch) (be/tool-platform)))
    (pcase name
      ("vterm-module"
       (let ((directory (or (getenv "BE_TOOL_VTERM_DIRECTORY")
                            (when-let ((library (locate-library "vterm")))
                              (file-name-directory library))
                            (error "Install the vterm Lisp package before its native module")))
             (vterm-always-compile-module t))
         (add-to-list 'load-path directory)
         (require 'be-git)
         (be/with-git-urls '("https://github.com/Sbozzolo/libvterm-mirror.git")
                           #'be/with-build-limits #'require 'vterm)))
      ((pred (lambda (tool) (assoc tool be/npm-tools))) (be/install-npm-tool name))
      ("node"
       (let ((node (be/node-runtime)))
         (unless (executable-find "node") (be/tool-wrapper name (list node)))))
      ((or "tinymist" "typst")
       (be/install-release-tool
        name (if (equal name "tinymist") "Myriad-Dreamin/tinymist" "typst/typst")
        (format "\\`%s-%s-%s\\.tar\\.%s\\'" name arch
                (if (equal os "darwin") "apple-darwin" "unknown-linux-musl")
                (if (equal name "typst") "xz" "gz")) name))
      ("helm_ls"
       (be/install-release-tool name "mrjosh/helm-ls"
                                (format "\\`helm_ls_%s_%s\\'" os
                                        (if (equal arch "x86_64") "amd64" "arm64")) name))
      ("fourmolu"
       (be/install-release-tool name "fourmolu/fourmolu"
                                (format "\\`fourmolu-.*-%s-%s\\.zip\\'" os
                                        (if (equal arch "aarch64") "arm64" arch)) name))
      ("dhall"
       (if (and (equal os "linux") (equal arch "aarch64"))
           (progn
             (be/run-installer '("cabal" "update"))
             (be/run-installer (list "cabal" "install" "--jobs=2" "--install-method=copy"
                                     (concat "--installdir=" (expand-file-name "~/.local/bin")) "dhall")))
         (be/install-release-tool name "dhall-lang/dhall-haskell"
                                  (format "\\`dhall-[0-9.]+-%s-%s\\.tar\\.bz2\\'" arch os) name)))
      ("typstyle"
       (be/install-release-tool name "typstyle-rs/typstyle"
                                (format "\\`typstyle-%s-%s\\'" arch
                                        (cond ((equal os "darwin") "apple-darwin")
                                              ((equal arch "aarch64") "unknown-linux-gnu")
                                              (t "unknown-linux-musl"))) name))
      ("google-java-format"
       (let* ((asset (be/github-asset "google/google-java-format" "-all-deps\\.jar\\'"))
              (jar (be/extract-tool name (alist-get 'browser_download_url asset)
                                   (alist-get 'name asset) name (alist-get 'digest asset))))
         (be/tool-wrapper name (list (be/java-runtime) "-jar" jar))))
      ("jdtls"
       (require 'url)
       (let* ((java (be/java-runtime 25))
              (base "https://download.eclipse.org/jdtls/snapshots/")
              (buffer (url-retrieve-synchronously (concat base "latest.txt") t t 30))
              filename)
         (unless buffer (error "Cannot read JDTLS version"))
         (unwind-protect
             (with-current-buffer buffer
               (unless (= url-http-response-status 200) (error "JDTLS version lookup failed"))
               (goto-char (point-min)) (re-search-forward "\r?\n\r?\n")
               (setq filename (string-trim (buffer-substring-no-properties (point) (point-max))))
               (unless (string-match-p "\\`jdt-language-server-[[:alnum:].-]+\\.tar\\.gz\\'" filename)
                 (error "Unexpected JDTLS archive name: %s" filename)))
           (kill-buffer buffer))
         (let ((binary (be/extract-tool name (concat base filename) filename "jdtls")))
           (be/tool-wrapper name (list binary "--java-executable" java
                                       "--jvm-arg=-Xms128m" "--jvm-arg=-Xmx1g")))))
      ((or "dotnet" "dotnet11")
       (let ((directory (be/tool-directory "dotnet")))
         (be/install-user-script "https://dot.net/v1/dotnet-install.sh"
                                 "--channel" "11.0" "--install-dir" directory "--no-path")
         (when (equal name "dotnet")
           (be/tool-wrapper name (list (expand-file-name "dotnet" directory))))))
      ("omnisharp"
       (let* ((asset (be/github-asset "OmniSharp/omnisharp-roslyn"
                                     (format "\\`omnisharp-%s-%s\\.tar\\.gz\\'"
                                             (if (equal os "darwin") "osx" os)
                                             (if (equal arch "x86_64") "x64" "arm64"))))
              (binary (be/extract-tool name (alist-get 'browser_download_url asset)
                                      (alist-get 'name asset) "OmniSharp" (alist-get 'digest asset)))
              (dotnet (file-truename (be/tool-present-p "dotnet11"))))
         ;; Installed dotnet may itself be our launcher; use the private SDK when present.
         (be/tool-wrapper name (list "env" (concat "DOTNET_ROOT="
                                      (if (file-exists-p (expand-file-name "dotnet" (be/tool-directory "dotnet")))
                                          (directory-file-name (be/tool-directory "dotnet"))
                                        (file-name-directory dotnet))) binary))))
      ("csharpier"
       (let* ((directory (be/tool-directory "csharpier"))
              (dotnet (be/tool-present-p "dotnet11"))
              (process-environment (copy-sequence process-environment))
              (root (if (file-exists-p (expand-file-name "dotnet" (be/tool-directory "dotnet")))
                        (directory-file-name (be/tool-directory "dotnet"))
                      (file-name-directory (file-truename dotnet)))))
         (setenv "DOTNET_ROOT" root)
         (setenv "DOTNET_CLI_TELEMETRY_OPTOUT" "1")
         (be/run-installer (list dotnet "tool" "install" "--allow-roll-forward" "--tool-path" directory "csharpier"))
         (be/tool-wrapper name (list "env" (concat "DOTNET_ROOT=" root)
                                     (expand-file-name "csharpier" directory)))))
      ("cs"
       (let* ((target (format "%s-%s" arch (if (equal os "darwin") "apple-darwin" "pc-linux")))
              (url (if (and (equal os "linux") (equal arch "aarch64"))
                       "https://github.com/VirtusLab/coursier-m1/releases/latest/download/cs-aarch64-pc-linux.gz"
                     (format "https://github.com/coursier/launchers/raw/master/cs-%s.gz" target))))
         (be/tool-wrapper name (list (be/extract-tool name url "cs.gz" "cs")))))
      ((or "metals" "sbt" "scalafmt")
       (let* ((process-environment (copy-sequence process-environment))
              (java-home (directory-file-name (file-name-directory (directory-file-name
                                                                  (file-name-directory (be/java-runtime))))))
              (directory (expand-file-name "bin" (be/tool-directory "coursier"))))
         (setenv "JAVA_HOME" java-home)
         (make-directory directory t)
         (be/run-installer (list "cs" "install" "--install-dir" directory name))
         (be/tool-wrapper name (list "env" (concat "JAVA_HOME=" java-home)
                                     (expand-file-name name directory)))))
      ("ghcup"
       (let ((process-environment (copy-sequence process-environment)))
         (setenv "BOOTSTRAP_HASKELL_NONINTERACTIVE" "1")
         (setenv "BOOTSTRAP_HASKELL_MINIMAL" "1")
         (setenv "BOOTSTRAP_HASKELL_ADJUST_BASHRC" "0")
         (be/install-user-script "https://get-ghcup.haskell.org")))
      ((or "ghc" "cabal" "stack" "haskell-language-server-wrapper")
       (be/run-installer (list "ghcup" "install"
                               (if (equal name "haskell-language-server-wrapper") "hls" name)
                               "recommended" "--set")))
      (_ (error "Unknown upstream tool: %s" name)))))

(provide 'be-toolchains)
;;; be-toolchains.el ends here
