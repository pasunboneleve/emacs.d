# Changelog

## [Unreleased]

## [1.0.2] - 2026-11-27

### Fixed

- Use lexical binding as the global evaluation default while honoring explicit
  file declarations and the native loader policy on each supported Emacs.
  Load CLI installer network libraries only when needed, avoiding premature
  activation of their `map` dependency. Activate managed `map` before recipe
  menus require it during package-manager bootstrap, and skip redundant
  activation of the already bootstrapped package manager.
- Keep published tags immutable and record subsequent fixes as distinct patch
  releases with their own version and changelog section.

## [1.0.1] - 2026-11-26

### Fixed

- Queue startup package declarations before processing their dependencies,
  avoiding duplicate-order failures in normal and daemon startup. Use built-in
  terminal completion on Emacs 31 instead of the Corfu terminal extension.
- Enable startup modes when their packages become ready and open the sidebar
  after both package activation and frame layout. Exercise the real startup
  phase in cross-platform bootstrap tests.

## [1.0.0] - 2026-11-26

### Changed

- Provision missing language servers and configured CLI dependencies on first
  use, including Java, C#, Scala, Haskell, Elm, and Mermaid preview. Run tool
  and grammar installation in a serial background queue with progress and
  resource limits. Preserve project-local formatters and project SDK choices.
- Exercise real CLI installation, language-server initialization, and Mermaid
  SVG rendering in cross-platform CI.

- Require Emacs 29 or newer. Install optional language, AI, document, and
  entertainment packages when first used; keep everyday UI available at startup.
- Use Eglot for language-server support, including Java and Rust. Remove the
  remaining Tide and lsp-mode configuration paths.
- Install required system tools through apt, dnf, or Homebrew, with bounded
  build parallelism. Elpaca installs at most two packages concurrently, uses
  shallow clones, and runs at most one native compilation worker.
  Keep garbage collection active during startup and completion to prevent
  unbounded memory growth during a cold installation.
- Prefer working SSH credentials for Git downloads, fall back to HTTPS when
  unavailable, and recheck for credentials later. Existing global SSH rewrites
  no longer prevent public downloads on machines without private keys.
- Skip encrypted personal configuration when its key is unavailable. Preserve
  those files and generate a separate machine-local encryption key for new
  local settings.
- Install tree-sitter grammars on first use and icon fonts on graphical startup.
  Select compatible package and grammar versions for Emacs 29.
- Require Ubuntu, Fedora, and macOS CI before merging pull requests.

### Compatibility and rollback

Optional feature entry points now perform their first installation. Unused
language packages are no longer installed at startup. Java uses Eglot's JDTLS
setup rather than machine-specific JDK paths. Combobulate's extra highlight
queries are disabled; native modes provide syntax highlighting.

The previous configuration is commit
[`e8e1e71`](https://github.com/pasunboneleve/emacs.d/commit/e8e1e71e16c601f75785f48e6ed0b418ea241a52).
The portability and on-demand implementation starts at
[`4caae54`](https://github.com/pasunboneleve/emacs.d/commit/4caae54a76cf3871115544267847902c153e68a2).
Use the previous commit in a separate checkout for rollback; generated package
caches and local encrypted settings are not part of Git history.
