# js-format.el
Format javascript code using node server.


Send code to local node server to format its style,
 using [standard](http://standardjs.com)

## Install

1. You need NodeJS >= 6 in your system path

2. `js-format.el` is available via MELPA and can be installed via

    M-x package-install js-format

 If failed, ensure you have

    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
    ;; or (add-to-list 'load-path "folder-of-js-format.el")

 line in your package config.

3. Althrough it should auto install later, you can run `npm install`
 from js-format folder to install npm dependencies with no harm.

## Usage

After `(require 'js-format)`, below function can be used:

`js-format-mark-statement` to mark current statement under point.

`js-format-region` to mark current statement, pass it to *node server*, then get
 back the result code to replace the statement.

`js-format-buffer` to format the whole buffer.

You may also want to bind above func to keys:

    (global-set-key (kbd "M-,") 'js-format-mark-statement)
    (global-set-key (kbd "C-x j j") 'js-format-region)
    (global-set-key (kbd "C-x j b") 'js-format-buffer)

## Customize format style

You can rewrite `function format(code, cb){}` function in *formatter.js* file,
 to customize your style of format.
