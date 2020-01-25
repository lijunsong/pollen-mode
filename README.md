# pollen-mode

An Emacs major mode for editing [pollen file](http://docs.racket-lang.org/pollen/).

The major mode provides font locks for

- malformed (dangling) command character
- comment
- tag functions

These files also provide

- Key-bindings in a minor mode for editing pollen files.
- A company-mode backend for tag function completions.

# Editing assistance

## Lozenge

Pollen mode makes it easy to insert lozenge. `@` will be turned into
lozenge. To insert `@`, press `@` twice. The preceding lozenge will
be turned into an `@`.

For the old behavior to insert the lozenge, (press `@` and then `TAB`),
remove the binding for `@` and bind `TAB` to
`pollen-insert-tab-or-command-char`.

Along with tag function completions (see below), it is easier to
insert *correct* tag functions now.

![Lozenge tag completion](./images/lozenge-tag-completions.gif?raw=true "Lozenge tag completion")

## Block editing

Pollen mode supports block editing. `C-c C-c` will put the block under
cursor into another buffer for special editing. You can turn on any
mode you want in the new buffer without interfering original one.

![Block Editing](./images/block-editing.gif?raw=true "Block Editing")

## Editing preprocess file

Pollen mode makes it easy to edit pollen preprocess files too. Emacs
already provides useful mode for the file that pollen is
preprocessing. When opens pollen preprocess file, Emacs will
automatically turn on pollen minor mode for the buffer, so that you
won't lose any convenient key-bindings of its major mode.

## Tag Function Completions

`company-pollen` is a company-mode backend for supporting identifier
completions. It supports completing identifiers exported from pollen
modules you're editing. The completion works even if you have nested
directory as the completion facility respects pollen configuration
search path.

## Server Integration

`pollen-mode` also provides functions to start, stop and resume pollen
server. Most of actions need just one keystroke once the server has
started. Start the server by `pollen-server-start`, and follow
instructions on the server window.

![Pollen Server](./images/pollen-server.gif?raw=true "Pollen Server")

# Installation

This package is available on Melpa (`M-x list-packages`). Search for
`pollen-mode` and `company-pollen`.

Advanced users can also download the source from
https://github.com/lijunsong/pollen-mode

# Use

To use the major mode, minor mode, and pollen server, do

```
(require 'pollen-mode)
```

To use completion, do

```
(require 'company-pollen)
```

To start the server, run `M-x pollen-server-start`, and then the server buffer
has indicator for your next step.

Feedbacks and feature requests are very welcome. I write with pollen-mode
everyday and I'd like to hear your thoughts on how to improve our editing
experience.
