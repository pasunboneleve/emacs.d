# First-use CLI tools

Missing CLI tools install when their feature is first used. Interactive Emacs
queues one installation at a time, reports the current tool in the mode line
and `*Messages*`, and keeps output in `*Emacs dependency installation*`.
Downloads and builds run outside the editor process. Native packages use
TRAMP's sudo prompt when required; authentication itself can still need input.

Eglot starts after the language server becomes ready. A tree-sitter buffer
temporarily uses its fallback mode and switches when the grammar is ready.
Pending work is not recorded as a failed grammar. Explicit commands wait for
their tools, but do not replay if the buffer's text changed during installation.
Formatters provision their executable on demand; the next save or format command
uses the ready tool. Project-local npm/PnP formatters take precedence over a
global fallback.

Public release discovery falls back to GitHub's public release pages when its
API is rate-limited. CI supplies its temporary read-only token for API requests;
normal installations do not require a personal GitHub token.

Use `M-x be/install-tool` to install or retry a named executable. Failed
installations produce a warning and leave editing available. Remote buffers
do not trigger local installation. Batch validation deliberately waits for
installation and reports a failing exit status when it cannot complete.

## Installation routes

| Feature | Tools | Route |
| --- | --- | --- |
| Typst | Tinymist, Typst, Typstyle | Official release binaries |
| Java | JDTLS, google-java-format | Eclipse snapshot archive and upstream JAR; Java 25 for JDTLS, 21+ for formatting |
| C# | OmniSharp, CSharpier | Official release and .NET tool; private .NET 11 SDK when needed |
| Scala | Metals, sbt, Scalafmt | Coursier launchers and Java 21+ |
| Haskell | HLS, GHC, Cabal, Stack; Fourmolu | GHCup; Fourmolu release binary |
| Elm | Language server, compiler, formatter | npm |
| JavaScript/TypeScript | Language server, TypeScript, Prettier | npm |
| Python | Pyright; Ruff, Black, isort | npm; uv tools |
| Helm/YAML | helm-ls, YAML language server | Official release binary; npm |
| Mermaid preview | `mmdc`, Puppeteer browser | `@mermaid-js/mermaid-cli` through npm; native browser libraries |
| SQL | SQLFluff, PostgreSQL client | uv; native package manager |
| Racket, Graphviz | `racket`, `dot` | Native package manager |
| Dhall | `dhall` | Official release binary; Cabal build on Linux ARM64 |
| Go, Rust, Terraform | gopls, rust-analyzer, terraform-ls | Native packages, rustup, Go |
| Aider | `aider`, Vterm native backend | uv tool with Python 3.12; backend compiled in the worker before loading the AI module |

Only tools required by the active feature are requested. For example, Mermaid
preview does not install on opening ordinary Markdown, and opening Emacs Lisp
does not install Haskell or Common Lisp.

## SDKs and licenses

Java tools reuse a suitable installed Java or download
[Eclipse Temurin OpenJDK](https://adoptium.net/docs/faq/), licensed under
GPLv2 with the Classpath Exception. This avoids Oracle's proprietary JDK
download flow. JDTLS uses Java 25 because its current Eclipse snapshot bundles
components requiring Java 25. The other Java tools accept Java 21 or newer.
Its launcher uses a 128 MiB initial heap and a 1 GiB maximum heap.

SDKs live under `~/.local/share/emacs-tools/`; launchers live in `~/.local/bin/`.
npm-based editor tools use a private Node 24 runtime. Java and .NET launchers
select their SDK for that tool without changing global `JAVA_HOME`,
`DOTNET_ROOT`, project manifests, or shell profiles. Existing executable tools
are otherwise reused. GHCup installs its recommended compiler only when a
compiler is missing; project-specific GHC/HLS or .NET SDK constraints still
belong to the project environment.

On Linux desktops with a user systemd bus, each private installer and all its
children run with a 2 GiB memory limit, no swap, one CPU's quota, and reduced
priority. Other environments retain reduced priority and the shared two-job
build limits. Native package installation follows the operating system's
package manager. These are installation limits, not limits on project builds.

No installer accepts proprietary license prompts or changes OS security
policy. Homebrew and Apple's command-line tools must already be usable on
macOS. Network restrictions, unavailable sudo access, project credentials,
external PDF viewer preferences, and a disabled Chromium sandbox cannot be
repaired by installing an Emacs package. Mermaid does not disable the browser
sandbox in the user's configuration.

## Validation

```sh
emacs -Q --batch -l tests/tool-installation-tests.el -f ert-run-tests-batch-and-exit
```

The full [CI runner](ci.md) also installs real tools in a disposable home,
checks CLI execution, initializes Tinymist/JDTLS/OmniSharp over LSP, and renders
a Mermaid SVG. The root-only Linux container fixture disables Chromium's
sandbox for that one test invocation; it does not alter editor configuration.
