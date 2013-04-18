slime-company
=============

A company-mode completion backend for Slime.

This is a backend implementation for the Emacs completion package
company-mode by Nikolaj Schumacher. More info about this package
is available at http://company-mode.github.io/

# Installation

Put this file somewhere into your load-path (or just into
slime-path/contribs) and then call

    (slime-setup '(slime-company))

I also have the following, IMO more convenient key bindings for
company mode in my .emacs:

    (define-key company-active-map (kbd "\C-n") 'company-select-next)
    (define-key company-active-map (kbd "\C-p") 'company-select-previous)
    (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "\C-v") 'company-show-location)
    (define-key company-active-map (kbd "<tab>") 'company-complete)
    (define-key company-active-map (kbd "\C-g") '(lambda ()
                                                   (interactive)
                                                   (company-abort)))
