# Emacs Click Mode [![MELPA badge][melpa-badge]][melpa-link]

[melpa-link]: https://melpa.org/#/click-mode
[melpa-badge]: https://melpa.org/packages/click-mode-badge.svg

`click-mode` is an Emacs major mode for
editting [Click Modular Router](http://read.cs.ucla.edu/click/click)
configuration files. It provides basic syntax highlighting and
indentation, making click files nearly comprehensible!

![Screenshot](https://github.com/bmalehorn/click-mode/raw/master/example.png)

## Install

`click-mode` is available
on [melpa](http://melpa.org/#/getting-started). You can install it with:

    M-x package-install RET click-mode RET

Then simply open a `.click` file and enjoy!

You may also want to add this to your `.emacs`:

    (add-to-list 'auto-mode-alist '("\\.template\\'" . click-mode))
    (add-to-list 'auto-mode-alist '("\\.inc\\'" . click-mode))
