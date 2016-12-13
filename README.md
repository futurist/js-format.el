# js-format.el
Emacs package for format or transform code style using different javascript formatter (standard, jsbeautify, esformatter, etc.)

Send region or buffer to a format server (will setup localhost:58000 by default), with below formatters:

 - [standard](http://standardjs.com)  # **zero** config
 - [jsbeautify](https://github.com/beautify-web/js-beautify)  # **little** config
 - [esformatter](https://github.com/millermedeiros/esformatter)  # **total** config
 - [stylefmt](https://github.com/morishitter/stylefmt)  # css formatter

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

3. It should auto setup for the first time of use, according to different style package's setup command.

## Usage

- `js-format-setup` to switch and setup style (default value: `"standard"`).

To make different mode using different format style, you can add below:

``` emacs-lisp
;; using "standard" as js formatter
(after-load 'js2-mode
  (add-hook 'js2-mode-hook
            (lambda()
              (js-format-setup "standard"))))

;; using "jsbeautify-css" as css formatter
(after-load 'css-mode
  (add-hook 'css-mode-hook
            (lambda()
              (js-format-setup "jsb-css"))))
```

The **style name** is from **styles.json** key, you can change the key to any name.


- `js-format-mark-statement` to mark current statement under point (only in `js2-mode`).

- `js-format-region` to try mark current statement, pass it to `js-format-server`, then get
 back the result code to replace the statement.

- `js-format-buffer` to format the whole buffer.

You may also want to bind above func to keys:

    (global-set-key (kbd "M-,") 'js-format-mark-statement)
    (global-set-key (kbd "C-x j j") 'js-format-region)
    (global-set-key (kbd "C-x j b") 'js-format-buffer)

## Add new format style guide

   1. Create a folder, with name say "my-style"
   2. Add package.json file, to specify an entry file in "main", or will use "index.js" if not specified.
   3. Entry file should have `function format(code, cb){}` exported as a node module.
   4. Add a style name and the folder into "styles.json" file to register the new style.

## Why use NodeJS Server instead of `call-process' etc.?

At first I'm using `call-process' to run a JS code, but every time
there's a lag, since starting a new node is a heavy operation, and
the output/input pipe not easily controlled if run as deamon, with
need of formatting region constantly, or even, auto formatting when
press RETURN, that lag is fatal.

Using server instead, giving a fast response from socket, and you
can format remotely (setup a format server in your workplace).

NodeJS is a good choise for using NPM, with rich module to import,
and easy to write a new style with javascript.
