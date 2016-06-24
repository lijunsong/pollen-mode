# pollen-mode

An Emacs major mode for editing [pollen file](http://docs.racket-lang.org/pollen/).

The major mode provides font locks for

- malformed (dangling) command character
- comment
- tag functions

These files also provide

- Keybindings in a minor mode for editing pollen files.
- A company-moe backend for tag function completions.

# Editing assistant

## Lozenge

Pollen mode makes it easy to insert lozenge in a non-disturbing way:
insert `@` and hit the tab. `@` will be turned into lozenge.

Actually I plan to abandon emacs typical keybindings for most
editing in this pollen-mode. I think composing with multiple key
strokes is annoying.

## Block editing

Pollen mode provides a keybinding for editing blocks. `C-c '` will put
the block under cursor into another buffer for special editing. You
can turn on any mode you want in the new buffer without interfering
original one.

Block editing is still improving, but it just works for now.

## Editing preprocess file

Pollen mode makes it easy to edit pollen preprocess files too. Emacs
already provides useful mode for the file that pollen is
generating. When opens pollen preprocess file, Emacs will
automatically turn on pollen minor mode for the buffer, so that you
won't lose any convenient keybindings in other major mode.

## Auto Completions

`company-pollen` is a company-mode backend for supporting identifier
completions. It supports completing identifiers exported from
`pollen.rkt`.

# Use

Place these files in your loading path and load it

```
(require 'pollen-mode)
```

To use completion, install `company-mode` and do

```
(require 'company-pollen)
```

Feedbacks and feature requests are welcome. I write with pollen-mode
everyday and I'd like to hear what you think about that can improve
editing experience[1].

[1] And I agree that pollen probably needs an IDE that really
understands racket code to make editing experience awesome.
