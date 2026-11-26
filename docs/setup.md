# Setup and maintenance

## Startup and first use

Install [Emacs](https://www.gnu.org/software/emacs/) 29 or newer and Git, clone this repository into the Emacs
configuration directory, and start Emacs. On macOS, install [Homebrew](https://brew.sh/) first.
On [Ubuntu](https://ubuntu.com/) and [Fedora](https://fedoraproject.org/), apt-get and dnf are detected automatically. Package
installation uses TRAMP's sudo authentication rather than embedding a password
in a command. Already-installed executables are reused.

Startup installs the UI, completion, navigation, and general editing packages.
`mixins/languages.el` registers file associations and first-use commands; it
does not evaluate the language package declarations. Each language's
configuration lives in `mixins/languages/`. Opening a supported file loads its
configuration, waits for its package queue, and enters the editing mode.
Common Lisp and SBCL are installed only when Common Lisp is used.

Additional entry points are available through `M-x`:

- `be/sly`: Common Lisp REPL.
- `be/agent-shell`, `be/codex`, `be/aider`, `be/gptel`: AI tools.
- `be/music`: media playback.
- `be/github`: GitHub client, installing its CLI before loading the client.
- `be/org-agenda`, `be/org-capture`: Org commands before opening an Org file.
- `be/apheleia-mode` (`C-c t a`): the existing formatting command, loaded on demand.

The existing AI keybindings also activate their module on first use. PDF and
EPUB viewers activate when opening the corresponding file. Org extensions
activate when entering Org mode. Opening an Emacs Lisp file activates Parinfer
without installing Common Lisp tools.

First use can take time and needs network access. Failures appear in
`*Warnings*`, `*Emacs dependency installation*`, or `M-x elpaca-log`. An install
failure leaves basic editing available. Remote files never trigger local
system-tool installation or automatic server provisioning.

CLI downloads and builds run in a serial background queue with mode-line
progress. See [First-use CLI tools](tools.md) for installation routes, SDK
isolation, licensing, retry commands, and the remaining environment requirements.

## Package compatibility

Installation is bounded in Emacs itself: `elpaca-queue-limit` allows two package
orders at once, native compilation uses one worker, and package subprocesses
receive two-job limits for Make, CMake, Cargo, and Go. Git packing and indexing
use one thread each. These settings apply equally
to GUI, daemon, and batch startup; chezmoi does not need to supply a CPU limit.
This controls concurrent work rather than network bandwidth or a hard memory
quota. The first-use boundaries also keep unused languages out of the queue.
New package clones use depth 1 by default; a recipe can request full history
explicitly when it needs it.

[Elpaca](https://github.com/progfolio/elpaca) remains the package manager.
Separately released bundled libraries are upgraded before their consumers
load. `local-packages/be-packages.el` contains compatibility exceptions and
uses standalone [ELPA mirrors](https://github.com/emacs-straight) for core
packages, avoiding one complete Emacs checkout per library.

On Emacs 29, Casual is held at 2.14.0 because newer releases require Emacs 30.
On releases before 29.4, Consult-gh uses v2.0 to support Ubuntu's Emacs 29.3.
Package metadata still enforces the real minimum Emacs version. Compatibility
libraries cannot supply features missing from the Emacs core. Updates may
occasionally need another explicit exception here; the configuration does not
guess an arbitrary older revision or disable dependency checks.

`early-init.el` sets lexical binding as the global default. Explicit file-local
binding declarations still take precedence, and existing compiled libraries
keep their compiled semantics. Legacy source that implicitly relies on dynamic
scope can behave differently under this default; it is not a security fix by
itself. Distribution site files run before the user configuration, so their
missing-declaration warnings can remain.

Installer network libraries load only when an installer needs them, keeping
their `map` dependency out of the early bootstrap. Emacs can already have
`seq` and `eldoc` loaded before user initialization; their compatibility
upgrades remain necessary on older supported versions.

Use `M-x elpaca-update-all` for package updates. For failed packages, inspect
`M-x elpaca-log`, fix the reported cause, and use `M-x elpaca-rebuild`. Then
revisit the file or invoke its first-use command. Installed packages from older
configurations are left on disk; unused language declarations no longer queue
them on startup.

## Git transport

Public package and grammar downloads on GitHub, GitLab, Codeberg, and SourceHut
prefer SSH when a bounded, noninteractive authentication probe succeeds.
The probe uses the SSH agent and SSH configuration, requires an already-trusted
host key, and never asks for a password. When it fails, downloads use HTTPS.
Results expire after one minute so new credentials are discovered on subsequent
downloads and updates.

Before fetching an existing package checkout, its supported hosting remotes are
updated to the currently available transport. This changes package checkouts
only; the configuration repository's origin and global Git configuration are
not rewritten. Private repositories still require working credentials.
Unknown hosting providers retain their configured URL.
The fallback also overrides broad Git `insteadOf` rules within package
subprocesses, so a global SSH preference cannot defeat HTTPS fallback.
Git's `core.sshCommand` and `GIT_SSH_COMMAND` remain supported.

## Local keys and encrypted configuration

Startup reuses an available secret encryption key. If none exists, it installs
GnuPG when needed and creates an encryption-capable local storage key. Generated
keys have no passphrase and rely on OS account protection. They are not exported
or configured as signing identities. Use GnuPG to add passphrase protection if
that suits the machine's security requirements.

`custom-local.el.gpg` is machine-local, encrypted, and ignored by Git.
The legacy `custom.el.gpg`, `mixins/communication.el.gpg`, and `mixins/safe.el.gpg`
are loaded only when a matching secret recipient key exists. A new local key
cannot decrypt files encrypted to your personal key. Those files remain on disk
and produce a warning instead of blocking startup; importing their original key
restores access. Batch key generation cannot recover unavailable secrets.

The local key is the default recipient for new encrypted files. Existing files
retain the recipients recorded in their file-local settings. Keep secrets in
encrypted files or the OS secret store, not ordinary project configuration.

## Grammars, fonts, and language servers

Tree-sitter entry points check the grammar before entering the mode. Missing
grammars are compiled using their listed source revisions; the common grammars
use ABI 14 releases compatible with Emacs 29. If tree-sitter, a compiler, or the
grammar is unavailable, the entry point uses its ordinary editing mode.
Grammar libraries live in directories named for the Emacs major version and
grammar release, so an older cached parser does not mask an updated source.
JavaScript and TypeScript use newer ABI 14 grammar releases on Emacs 30+ to
match those versions' native font-lock rules. Combobulate provides structural
editing while native modes provide highlighting; the fork's incompatible
extra highlight queries are disabled.
A failed grammar build is not repeatedly retried during the same session;
after fixing the cause, clear `be/grammar-failures` or restart Emacs.

All-the-icons and Nerd Icons fonts install when a graphical frame first needs
them, including frames created after daemon startup. Terminal-only sessions do
not install fonts. The preferred text font remains a personal display choice;
it is separate from icon fonts.

Eglot is the only configured language-server client; Tide is removed and Rustic
is configured for Eglot. Python uses Pyright, JavaScript and TypeScript use
typescript-language-server, Go uses gopls, and Rust uses rust-analyzer.
Executable provisioning is centralized in `local-packages/be-system.el`.
Python formatters use uv; npm-based tools, including Pyright, use private Node.
System packages use the native manager. Java, Scala, and C# can provision
their editor SDKs and servers automatically without changing project SDK
settings. Existing project environments remain usable through envrc.
See [tools.md](tools.md) for the complete installation routes and limits.

## Validation

Run the checks in [../README.md](../README.md). Behavior tests fake process
and network boundaries; the encryption test creates real disposable keyrings.
The Lisp reader check includes both startup and deferred configuration files.
Newer source always takes precedence over older bytecode during startup, so
updating the configuration does not require deleting compiled helper files.

`tests/bootstrap-smoke.el` runs actual package installation and checks that
startup has not queued unused language packages. Run it against a disposable
copy containing `init.el`, `early-init.el`, `mixins/`, `local-packages/`, and
`tests/`, with a disposable GnuPG home:

```sh
EMACS_TEST_DIRECTORY="$test_directory" GNUPGHOME="$test_keyring" NATIVE_DISABLED=1 \
  emacs -Q --batch -l "$test_directory/tests/bootstrap-smoke.el"
```

Both directories must exist; give the keyring directory mode 0700. Repeat the
command to check installed-state startup. Do not point this check at the live
configuration when testing package installation or key generation.

Run `tests/first-use-smoke.el` with the same environment to check real JSON,
Markdown, Java, JavaScript, TSX, and formatting activation and grammar builds. Language-server
startup is stubbed in that check so it does not install project SDKs.
Set `NATIVE_DISABLED=0` for the bootstrap check to exercise ordinary startup
with compile-angel enabled; its native compilation cache stays in the test copy.
