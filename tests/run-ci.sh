#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/.."
source_directory=$PWD
test_root=${CI_TEST_ROOT:-$(mktemp -d)}
mkdir -p "$test_root/config" "$test_root/home" "$test_root/logs"
export HOME="$test_root/home"
export GNUPGHOME="$HOME/.gnupg"
mkdir -m 700 "$GNUPGHOME"
export EMACS_TEST_DIRECTORY="$test_root/config"
export NATIVE_DISABLED=1
trap 'gpgconf --kill all' EXIT

# Copy only tracked sources: local bytecode and package caches must not mask failures.
git -c safe.directory="$source_directory" ls-files -z -- '*.el' '*.gpg' | while IFS= read -r -d '' path; do
  mkdir -p "$EMACS_TEST_DIRECTORY/$(dirname "$path")"
  cp "$source_directory/$path" "$EMACS_TEST_DIRECTORY/$path"
done
cd "$EMACS_TEST_DIRECTORY"
emacs --version | head -n 1
emacs -Q --batch -l tests/check-lisp.el 2>&1 | tee "$test_root/logs/syntax.log"
emacs -Q --batch -l tests/portability-tests.el -f ert-run-tests-batch-and-exit 2>&1 | tee "$test_root/logs/portability.log"
emacs -Q --batch -l tests/secrets-integration.el -f ert-run-tests-batch-and-exit 2>&1 | tee "$test_root/logs/secrets.log"

# Exercise HTTPS fallback despite a pre-existing SSH rewrite and no private keys.
git config --global url.git@github.com:.insteadOf https://github.com/
emacs -Q --batch -l tests/first-use-smoke.el 2>&1 | tee "$test_root/logs/first-use.log"
NATIVE_DISABLED=0 emacs -Q --batch -l tests/bootstrap-smoke.el 2>&1 | tee "$test_root/logs/ordinary-startup.log"
