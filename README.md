# emacs.d

A reproducible Emacs configuration focused on fast navigation, code
exploration, and architecture workflows — structured to minimise
state, reduce boilerplate, and avoid config bankruptcy.

## Why this exists

Most Emacs configs start simple and slowly collapse under their own
weight.

Packages accumulate. State leaks. Startup slows down. Changes become
risky.

This configuration is built to stay maintainable over time:

- minimal implicit state
- clear module boundaries
- reproducible across machines
- explicit handling of private configuration

No rewrites. No “start over from scratch”. No config bankruptcy.

## Architecture

This is not a framework (like
[Doom](https://github.com/doomemacs/doomemacs) or
[Spacemacs](https://github.com/syl20bnr/spacemacs)). It’s a
**core + mixins** system.

- `early-init.el`: startup hygiene (GC, package disabling, secrets, no
  flicker)
- `init.el`: bootstrap (elpaca, use-package, defaults, global
  behaviour)
- `mixins/`: domain-specific modules loaded in order

Each mixin owns a concern:

- `ui.el` — usability and presentation
- `dev.el` — editing, navigation, version control, shells
- `languages.el` — language support orchestration (LSP/eglot +
  per-language modules)
- `minibuffers.el` — completion stack (vertico, consult, corfu, etc.)
- `ai.el` — AI-assisted workflows (local + API-backed)
- `viewers.el`, `org-config.el`, `tramp-config.el`, etc.

Private behaviour is isolated into encrypted modules:
- `safe.el.gpg`
- `communication.el.gpg`

## Key ideas

- **State is the enemy** Everything is structured to minimise hidden
  state and side effects.

- **Modules over sprawl** Features live in mixins, not scattered
  across init files.

- **System dependencies are explicit** External tools and language
  servers are part of the design (`system-packages`).

- **Mixed LSP strategy** Uses `eglot` or `lsp-mode` depending on
  ecosystem maturity.

- **Private config is first-class** Secrets and personal behaviour are
  separated via GPG, not commented hacks.

## Installation

```bash
git clone <repo> ~/.emacs.d
```

If you don’t use encrypted modules, comment out `.gpg` loads in
`init.el`.

## Environment

Tested on:

* Emacs 30
* Fedora Linux (GNOME, Wayland)

Other systems should work with minor adjustments (notably
`system-packages`).

## Fonts

Install a Nerd Font (e.g. Inconsolata Nerd Font) for icons:

```elisp
M-x nerd-icons-install-fonts
```

## Non-goals

* This is not a drop-in distro
* This is not beginner-oriented
* This is not “everything included”

It’s a **maintainable personal system** you can adapt and evolve.
