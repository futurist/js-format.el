;;; js-format.el --- Format or transform code style using NodeJS server with different javascript formatter  -*- lexical-binding: t; -*-

;; Filename: js-format.el
;; Description: Format or transform code style using NodeJS server with different javascript formatter (standard, jsbeautify, esformatter, airbnb, etc.)
;; Author: James Yang <jamesyang999@gmail.com>
;; Copyright (C) 2016, James Yang, all rights reserved.
;; Time-stamp: <2016-12-12 19:07:15 James Yang>
;; Created: 2016-12-05 13:57:46
;; Version: 0.1.0
;; URL: http://github.com/futurist/js-format.el
;; Keywords: js, javascript, format, standard, jsbeautify, esformatter, airbnb
;; Package-Requires: ((emacs "24.1") (js2-mode "20101228"))
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Send region or buffer to a format server (will setup localhost:58000 by default), with below formatters:
;;  - [standard](http://standardjs.com)  # zero config
;;  - [jsbeautify](https://github.com/beautify-web/js-beautify)  # little config
;;  - [esformatter](https://github.com/millermedeiros/esformatter)  # total config
;;  - [airbnb](https://github.com/airbnb/babel-preset-airbnb)  # **Airbnb** style formatter
;;  - [stylefmt](https://github.com/morishitter/stylefmt)  # css

;; ## Install

;; 1. You need NodeJS >= 6 in your system path

;; 2. `js-format.el` is available via MELPA and can be installed via

;;     M-x package-install js-format

;;  If failed, ensure you have

;;     (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;     ;; or (add-to-list 'load-path "folder-of-js-format.el")

;;  line in your package config.

;; 3. It should auto setup for the first time of use, according to different style package's setup command.

;; ## Usage

;; After `(require 'js-format)`, below function can be used:

;; `js-format-setup` to switch and setup style (buffer local).
;; With C-u prefix, you can also setup the server (buffer local).
;; To make different mode using different format style, you can add below:

;;  ;; automatically switch to JSB-CSS style using jsbeautify-css as formatter
;;  (after-load 'css-mode
;;    (add-hook 'css-mode-hook
;;          (lambda()
;;            (js-format-setup "jsb-css"))))

;; The style name is from "styles.json" file, you can change the key to any.

;; `js-format-mark-statement` to mark current statement under point (only in `js2-mode').

;; `js-format-region` to try mark current statement, pass it to `js-format-server', then get
;;  back the result code to replace the statement.

;; `js-format-buffer` to format the whole buffer.

;; You may also want to bind above func to keys:

;;     (global-set-key (kbd "M-,") 'js-format-mark-statement)
;;     (global-set-key (kbd "C-x j j") 'js-format-region)
;;     (global-set-key (kbd "C-x j b") 'js-format-buffer)
;;     (global-set-key (kbd "C-x j s") 'js-format-setup)

;; ## Add new format style guide

;; 1. Create a folder, with name say "my-style"
;; 2. Add package.json file, to specify an entry file in "main", or will use "index.js" if not specified.
;; 3. Entry file should have `function format(code, cb){}` exported as a node module.
;; 4. Add a style name and the folder into "styles.json" file to register the new style.

;; ## Why use NodeJS Server instead of `call-process' etc.?

;; At first I'm using `call-process' to run a JS code, but every time
;; there's a lag, since starting a new node is a heavy operation, and
;; the output/input pipe not easily controlled if run as deamon, with
;; need of formatting region constantly, or even, auto formatting when
;; press RETURN, that lag is fatal.

;; Using server instead, giving a fast response from socket, and you
;; can format remotely (setup a format server in your workplace).

;; NodeJS is a good choise for using NPM, with rich module to import,
;; and easy to write a new style with javascript.

;;; Code:

(require 'js2-mode)
(require 'url)
(require 'json)
(require 'ido)

(defvar js-format-proc-name "JSFORMAT"
  "Process name of NodeJS.")

(defvar js-format-style nil
  "The js-format style to use.")

(defvar js-format-default-server "http://localhost:58000"
  "Global default format server, when `js-format-server' is nil, this value will be used.")

(defvar js-format-server nil
  "Format server for each buffer locally to format string.")

(progn
  (make-variable-buffer-local 'js-format-style)
  (make-variable-buffer-local 'js-format-server))

(defvar js-format-folder
  (let ((script-file (or load-file-name
                          (and (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name)))
    (file-name-directory (file-truename script-file)))
  "Root folder of js-format.")

(defvar js-format-start-command
  (let ((bin-file (expand-file-name "./server.js" js-format-folder)))
    (cons (or (executable-find "node")
              "node")
          (list (if (file-exists-p bin-file) bin-file (error "Js-format: cannot find server.js")))))
  "The command to be run to start the js-format server.
Should be a list of strings, giving the binary name and arguments.")

;;;###autoload
(defun js-format-mark-statement (&optional skip-non-statement)
  "Mark js statement at point.
Will avoid mark non-formattable node when SKIP-NON-STATEMENT is non-nil."
  (interactive "P")
  (unless js2-mode-ast (error "Not in js2-mode, You should mark region manually"))
  (js2-backward-sws)
  (when (looking-at-p "[\t \n\r]") (forward-char -1))
  (let* ((region-beg (if (use-region-p) (region-beginning) (point-max)))
         (cur-node (js2-node-at-point))
         (parent cur-node)
         beg end)
    ;; blacklist: 33=prop-get, 39=name, 40=number, 45=keyword
    ;; 128=scope, 108=arrow function, 66=object, 123=catch
    (while (and parent (or
                        (memq (js2-node-type parent) '(33 39 40 45)) ; always skip name, prop-get, number, keyword
                        (and skip-non-statement (memq (js2-node-type parent) '(66 123))) ;pure object will fail format
                        ;; (and (= (js2-node-type parent) 108) (eq (js2-function-node-form parent) 'FUNCTION_ARROW)) ;skip arrow function
                        (<= region-beg (js2-node-abs-pos parent))))
      (setq parent (js2-node-parent-stmt parent)))
    (setq beg (and parent (js2-node-abs-pos parent)))
    (setq end (and parent (js2-node-abs-end parent)))
    (when (and beg end (/= (- end beg) (- (point-max) (point-min))))
      (set-mark-command nil)
      (transient-mark-mode '(4))
      (goto-char beg)
      (set-mark-command nil)
      (goto-char end))))

;;;###autoload
(defun js-format-buffer ()
  "Format current buffer."
  (interactive)
  (save-excursion
    (let ((col (current-column))
          (line (line-number-at-pos))
          start)
      (goto-char (point-min))
      ;; skip # line for cli.js
      (when js2-mode-ast
        ;; skip server side script #! line only in js2-mode
        (while (and (not (eobp)) (looking-at-p "\\s *\#")) (forward-line 1)))
      (skip-chars-forward "\r\n[:blank:]")
      (setq start (point))
      (js-format-region start (point-max) nil `(,line ,col)))))

;;;###autoload
(defun js-format-line ()
  "Format line before point."
  (interactive)
  (save-excursion
    (let* ((pos (point))
           (col (current-column))
           (line (line-number-at-pos)))
      (goto-char (line-beginning-position))
      (skip-chars-forward "\t \n\r")
      (js-format-region (point) pos nil `(,line ,col)))))

;;;###autoload
(defun js-format-region (start end &optional not-jump-p pos-list)
  "Format region between START and END.
When NOT-JUMP-P is non-nil, won't jump to error position when format error.
POS-LIST is list of (line column) to restore point after format."
  (interactive (progn
                 (when (not (use-region-p))
                   (js-format-mark-statement t))
                 (list (region-beginning) (region-end) current-prefix-arg nil)))
  (while (not js-format-style)
    (let ((inhibit-message t))
      (call-interactively 'js-format-setup)))
  (save-excursion
    (let ((kill-ring nil)
          (cur-buffer (buffer-name))
          (errorsign "#!!#")
          (error-pos nil)
          success result get-formatted)
      (goto-char start)
      (skip-chars-forward "\t\n \r" end)
      (push-mark)
      (setq start (point))
      (goto-char end)
      (skip-chars-backward "\t\n \r" start)
      (setq end (point))
      (setq result (buffer-substring start end))
      (setf get-formatted
            #'(lambda (formatted)
                (setq success (not (string-prefix-p errorsign formatted) ))
                (switch-to-buffer cur-buffer)
                (if (string= "" formatted)
                    (message "js-format return nil")
                  (if (not success)
                      (progn (deactivate-mark)
                             (when (string-match "\"index\":\\([0-9]+\\)" formatted)
                               (setq error-pos (+ start (string-to-number (or (match-string 1 formatted) ""))))
                               (unless not-jump-p  (goto-char error-pos)))
                             (message "js-format-error[%s]: %s" js-format-style (car (split-string formatted errorsign t))))
                    (delete-region start end)
                    ;; (when (string-prefix-p ";" formatted) (setq formatted (substring formatted 1)))
                    (insert formatted)
                    (setq end (point))
                    ;; inhibit message in Emacs 25
                    (let ((inhibit-message t))
                      (push-mark start t t)
                      (goto-char end)
                      ;; mimic a TAB key
                      (call-interactively 'indent-for-tab-command)
                      ;; try to restore previous position
                      (when pos-list
                        (deactivate-mark nil)
                        (goto-line (car pos-list))
                        (move-to-column (car (cdr pos-list)) nil)))))))
      (js-format-run result get-formatted))))

(defun js-format-run (data done)
  "Call http server with DATA, and call DONE when received response."
  (let* (server callback runner local-done)
    (setf local-done #'(lambda(err)
                       (if err
                           (js-format-start-server callback)
                         (let ((result (prog2 (search-forward "\n\n" nil t) (buffer-substring (point) (point-max)) (kill-buffer))))
                           (setf result (decode-coding-string result (symbol-value 'buffer-file-coding-system)))
                           (funcall done result)))))
    (setf callback #'(lambda ()
                     (setf callback nil)
                     (funcall runner)))
    (setf runner #'(lambda()
                     (setq server (concat (or js-format-server js-format-default-server) "/format/" js-format-style))
                     ;; using backquote to quote the value of data
                     (js-format-http-request local-done server "POST" `,data)))
    (funcall runner)))

(defun js-format-start-server (cb-success)
  "Start node server when needed, call CB-SUCCESS after start succeed."
  (unless (get-process js-format-proc-name)
    (let* ((cmd js-format-start-command)
           (proc (apply #'start-process js-format-proc-name nil cmd))
           (all-output ""))
      (set-process-query-on-exit-flag proc nil)
      ;; monitor startup outputs
      (set-process-filter proc
                          #'(lambda (proc output)
                              (if (and (not (string-match "Listening on port \\([0-9][0-9]*\\)" output)))
                                  ;; it it's failed start server, log all message
                                  (setf all-output (concat all-output output))
                                (set-process-filter proc nil)
                                (funcall cb-success)
                                ;; monitor exit events
                                ;; (message "js-format server start succeed, quit with `js-format-exit'")
                                (set-process-sentinel proc #'(lambda (proc event)
                                                               (when (eq (process-status proc) 'exit)
                                                                 (message "js-format server exit %s" event)))))))
      ;; monitor startup events
      (set-process-sentinel proc #'(lambda (proc _event)
                                     (when (eq (process-status proc) 'exit)
                                       (message "js-format: %s" (concat "Could not start node server\n" all-output))))))))

;;;###autoload
(defun js-format-setup (&optional style server)
  "Switch to and setup the active format style to STYLE.
If STYLE changed, will call the style's setup command to setup.
If with C-u, will prompt to set `js-format-server'.
RETURN the current active style."
  (interactive (let ((config (json-read-file (expand-file-name "styles.json" js-format-folder))))
                 (list
                  (ido-completing-read "js-format style: "
                                       (mapcar #'(lambda(v) (symbol-name (car v))) config))
                  (when current-prefix-arg
                    (read-string "js-format server: "
                                 (or js-format-server js-format-default-server))))))
  (unless (or (not (stringp style))
              (string= "" style))
    (setq js-format-style style))
  (unless (or (not (stringp server))
              (string= "" server))
    (setq js-format-server server))
  (setq style js-format-style)
  (unless style
    (error "No style specified for js-format."))
  (let (callback local-done)
    (message "[js-format] \"%s\" setup in background, plesae try format after that." style)
    (setf callback #'(lambda()
                       (js-format-setup style server)))
    (setf local-done #'(lambda(err)
                         (if err
                             (js-format-start-server callback)
                           (let ((result (prog2 (search-forward "\n\n" nil t)
                                             (buffer-substring (point) (point-max)))))
                             (when (and (stringp result) (not (string= result "")))
                               (message "[js-format] setup result:\n%s" result))))))
    (let ((inhibit-message t))
      (js-format-http-request local-done (concat (or js-format-server js-format-default-server) "/setup/" style))))
  ;; return active style
  style)

;;;###autoload
(defun js-format-exit ()
  "Exit js-format node server."
  (interactive)
  (js-format-http-request '= (concat (or js-format-server js-format-default-server) "/exit")))

(defun js-format-http-request (callback url &optional method data cbargs)
  "CALLBACK after request URL using METHOD (default is GET), with DATA.
Call CALLBACK when finished, with CBARGS pass into."
  ;; Usage: (js-format-http-request 'callback "http://myhost.com" "POST" '(("name" . "your name")))
  (let ((url-request-method (or method "GET"))
        (url-request-extra-headers '(("Content-Type" . "text/plain")))
        (url-request-data (base64-encode-string (encode-coding-string (or data "") 'utf-8))))
    (url-retrieve url callback cbargs nil t)))



(provide 'js-format)
;;; js-format.el ends here
