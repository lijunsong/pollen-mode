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

Pollen mode makes it easy to insert lozenge in a non-disturbing way:
insert `@` and hit the tab. `@` will be turned into lozenge.

Along with tag function completions (see below), it is easier to
insert *correct* tag functions now.

![Lozenge tag completion](./images/lozenge-tag-completions.gif?raw=true "Lozenge tag completion")

## Block editing

Pollen mode supports block editing. `C-c '` will put the block under
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

# Use

*Notes: I am preparing a pollen-mode package.*

To use the major mode and pollen server, place these files in your
loading path and load it

```
(require 'pollen-mode)
```

To start the server, `M-x pollen-server-start` is the start point, and
then the server buffer has indicator for your next step.

To use completion, install `company-mode` and do

```
(require 'company-pollen)
```

Feedbacks and feature requests are welcome. I write with pollen-mode
everyday and I'd like to hear what you think about that can improve
editing experience[1].

[1] And I agree that pollen probably needs an IDE that really
understands racket code to make editing experience awesome.
