# Changelog

## [Unreleased]

## [1.0.0] - 2026-11-26

### Changed

- Require Emacs 29 or newer. Install optional language, AI, document, and
  entertainment packages when first used; keep everyday UI available at startup.
- Use Eglot for language-server support, including Java and Rust. Remove the
  remaining Tide and lsp-mode configuration paths.
- Install required system tools through apt, dnf, or Homebrew, with bounded
  build parallelism. Elpaca installs at most two packages concurrently, uses
  shallow clones, and runs at most one native compilation worker.
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
