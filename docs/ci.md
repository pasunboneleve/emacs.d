# Continuous integration

`.github/workflows/ci.yml` runs for every pull request, pushes to `main`, and
manual dispatches. The stable required check is `validate`; it fails if any
platform job fails, is cancelled, or is skipped. There are no path filters.

The jobs use Ubuntu 24.04's stock Emacs 29.3, Fedora 45's stock Emacs, and
Homebrew Emacs on macOS 15. The stock Fedora and Homebrew versions advance
with their package repositories. Each job starts without an Emacs package
cache or private SSH/GPG keys. Elpaca still limits concurrent installations
to two, and jobs have a 45-minute limit.
Startup keeps garbage collection active with a 64 MiB allocation threshold;
this is a collection trigger, not a hard process-memory limit.

The bootstrap check binds `after-init-time` to nil while loading configuration,
matching real startup so duplicate Elpaca orders cannot hide behind batch mode.
Separate child-process tests check lexical defaults, explicit dynamic-binding
overrides, and that installer bootstrap does not load network libraries early.

The shared runner checks Lisp syntax, portability ERT tests, real GPG key
creation and reuse, package bootstrap, first-use file modes and tree-sitter
grammars, and ordinary startup with native compilation enabled where
available. A global Git SSH rewrite exercises HTTPS fallback without keys.
The tests also assert that unrelated language packages stay uninstalled.
The file-mode test stubs server startup; a separate CLI smoke check provisions
the SDKs, initializes Tinymist/JDTLS/OmniSharp over LSP, and renders a Mermaid
SVG using its browser. It does not authenticate external services or build
projects. GUI font installation is tested
through simulated frame callbacks, not a graphical desktop.

Run the same suite from the repository root, with Emacs, Git, GnuPG, curl,
and a C/C++ toolchain installed:

```sh
bash tests/run-ci.sh
```

On a Linux workstation with systemd, limit the whole test process tree to
2 GiB of memory and disable swap for that scope:

```sh
systemd-run --user --scope -p MemoryMax=2G -p MemorySwapMax=0 bash tests/run-ci.sh
```

The script copies tracked Lisp/encrypted files into a temporary configuration
and gives it a separate home and keyring. Set `CI_TEST_ROOT` to a new absolute
directory to choose its location. Logs remain there after execution; GPG
agents are stopped on exit. GitHub uploads only diagnostic logs, never the
generated keyring. Package downloads are deliberately uncached so cold-start
regressions cannot hide behind previous builds.

`main` requires a PR and the GitHub Actions `validate` check, including for
administrators. Merge commits retain the individual changes. Version tags
are created only after the release PR is merged and its post-merge CI passes.
Published tags are immutable. Each bugfix release increments the patch version
and gets its own changelog section and new tag. The pre-work configuration is
preserved by `pre-2026-11-26`; later fixes never replace that reference.

`VERSION` and the dated section in `CHANGELOG.md` identify the intended version
tag. This is a tag-only release: there are no binary artifacts to publish.
The changelog records the previous configuration commit for rollback. A tag
captures configuration sources; upstream package revisions still follow their
recipes, so it is not a snapshot of the installed package cache.
