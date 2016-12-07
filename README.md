# js-format.el
Format javascript code using node server.

Send code to local node server to format its style, using [standard](http://standardjs.com)

## Install

1. You need NodeJS >= 6 in your system path

2. `js-format.el` is available via MELPA and can be installed via

    ``` emacs-lisp
    ;; confirm below package config exists
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

    ;; then run below
    M-x package-install js-format

    ;; then require the package in your config
    (require 'js-format)
    ```

    Or if you don't use MELPA, download this repo, add below lines to your `init.el`

    ``` emacs-lisp
    (add-to-list 'load-path "folder-of-js-format.el")
    (require 'js-format)
    ```

3. It should auto setup for the first time of use, but you can setup using below:

    `npm install` (stored in `js-format-setup-command` var)

    from **js-format folder** to install npm dependencies.

## Usage

- `js-format-mark-statement` to mark current statement under point.

- `js-format-region` to mark current statement, pass it to *node server*, then get
 back the result code to replace the statement.

- `js-format-buffer` to format the whole buffer.

You may also want to bind above func to keys:

    (global-set-key (kbd "M-,") 'js-format-mark-statement)
    (global-set-key (kbd "C-x j j") 'js-format-region)
    (global-set-key (kbd "C-x j b") 'js-format-buffer)

## Customize format style

You can rewrite `format(code, cb)` function in *formatter.js* file,
 to customize your style of format.
