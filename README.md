# emacs.d

[![CI](https://github.com/pasunboneleve/emacs.d/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/pasunboneleve/emacs.d/actions/workflows/ci.yml?query=branch%3Amain)

A personal [Emacs](https://www.gnu.org/software/emacs/) configuration for
Emacs 29 and newer, with everyday UI at startup and language tools installed
on first use. It supports [Ubuntu](https://ubuntu.com/),
[Fedora](https://fedoraproject.org/), and macOS with [Homebrew](https://brew.sh/).

The configuration stays in ordinary Lisp files: `init.el` handles startup,
`mixins/` contains feature configuration, and `local-packages/be-*.el` owns
the few shared installation helpers. Eglot provides language-server support.

## Start

Clone this repository into your Emacs configuration directory and start Emacs.
Git and Emacs must already be installed. The first startup downloads the UI
packages; the first visit to a language file installs its editing packages.
System package installation may request sudo authentication.

Public Git downloads prefer authenticated SSH and fall back to HTTPS.
Missing private keys leave encrypted configuration files intact and skipped.
New local secrets use a machine-local encryption key created when necessary.

See [docs/setup.md](docs/setup.md) for installation behavior, key handling,
first-use commands, and recovery.

See [CHANGELOG.md](CHANGELOG.md) for release changes and the previous configuration.

## Validate

```sh
emacs -Q --batch -l tests/check-lisp.el
emacs -Q --batch -l tests/portability-tests.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -l tests/secrets-integration.el -f ert-run-tests-batch-and-exit
```

The encryption test uses disposable keyrings. See
[docs/setup.md](docs/setup.md) for isolated bootstrap validation.

GitHub checks fresh installations on Ubuntu, macOS, and Fedora before merging.
See [docs/ci.md](docs/ci.md) for coverage and local reproduction.

The mixin structure draws on [Bedrock](https://github.com/ashton314/emacs-bedrock)
and [Mastering Emacs](https://www.masteringemacs.org/).
