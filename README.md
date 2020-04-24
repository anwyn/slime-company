slime-company
=============

A [company-mode](https://company-mode.github.io) completion backend for
[Slime](https://github.com/slime/slime), "The Superior Lisp Interaction Mode for Emacs".

![screenshot](slime-company.png)

## Setup

The recommended way to install `slime-company` is via
[MELPA](http://melpa.org/#/slime-company). If not using MELPA, put
this file somewhere into your load-path (or just into slime-path/contribs).

To activate the contrib add it to the `slime-setup` call in your `.emacs`

```el
(slime-setup '(slime-fancy slime-company))
```

You may also want to `M-x customize-group slime-company` to select the
completion method (use `fuzzy' if you like to complete package names),
the major modes where `slime-company` is automatically activated, what
do do after a successful completion and how to display the argument
list of a function.

These customization variables can also be set manually. An example with
`use-package' looks like this:

```
(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space))
```

The following bindings for `company-active-map` will add the usual
navigation keys to the completion menu:

```el
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(define-key company-active-map (kbd "M-.") 'company-show-location)
```

