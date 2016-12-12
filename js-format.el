;;; js-format.el --- Format javascript using different code style  -*- lexical-binding: t; -*-

;; Filename: js-format.el
;; Description: Format javascript using different code style (standard, jsbeautify, esformatter)
;; Author: James Yang <jamesyang999@gmail.com>
;; Copyright (C) 2016, James Yang, all rights reserved.
;; Time-stamp: <2016-12-10 15:00:04 James Yang>
;; Created: 2016-12-05 13:57:46
;; Version: 0.1.0
;; URL: http://github.com/futurist/js-format.el
;; Keywords: js, javascript, format, standard, jsbeautify, esformatter
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

;; Send code to local node server to format its style:
;;  - [standard](http://standardjs.com)  # zero config
;;  - [jsbeautify](https://github.com/beautify-web/js-beautify)  # little config
;;  - [esformatter](https://github.com/millermedeiros/esformatter)  # total config

;; ## Install

;; 1. You need NodeJS >= 6 in your system path

;; 2. `js-format.el` is available via MELPA and can be installed via

;;     M-x package-install js-format

;;  If failed, ensure you have

;;     (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;     ;; or (add-to-list 'load-path "folder-of-js-format.el")

;;  line in your package config.

;; 3. Althrough it should auto install later, you can run `npm install` from
;;  js-format folder to install npm dependencies with no harm.

;; ## Usage

;; After `(require 'js-format)`, below function can be used:

;; `js-format-mark-statement` to mark current statement under point.

;; `js-format-region` to mark current statement, pass it to *node server*, then get
;;  back the result code to replace the statement.

;; `js-format-buffer` to format the whole buffer.

;; You may also want to bind above func to keys:

;;     (global-set-key (kbd "M-,") 'js-format-mark-statement)
;;     (global-set-key (kbd "C-x j j") 'js-format-region)
;;     (global-set-key (kbd "C-x j b") 'js-format-buffer)

;; ## Customize format style

;; You can rewrite `function format(code, cb){}` in *formatter.js* file,
;;  to customize your style of format.

;;; Code:

(require 'js2-mode)
(require 'url)

(defvar js-format-style "standard"
  "The js-format style to use.")

(defvar js-format-proc-name "JSFORMAT"
  "Process name of NodeJS.")

(defvar js-format-host "http://localhost:58000"
  "Saved host for every connecting to format string.")

(defvar js-format-folder
  (let ((script-file (or load-file-name
                          (and (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name)))
    (file-name-directory (file-truename script-file)))
  "Root folder of js-format.")

(defvar js-format-setup-command "npm install"
  "Command to install node dependencies.")

(defvar js-format-command
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
  (unless js2-mode-ast (error "Not in js2-mode, You should mark region manually."))
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
      (transient-mark-mode '(4))
      (goto-char beg)
      (set-mark-command nil)
      (goto-char end))))

;;;###autoload
(defun js-format-buffer ()
  "Format current buffer."
  (interactive)
  (let ((col (current-column))
        (line (line-number-at-pos))
        start)
    (goto-char (point-min))
    ;; skip # line for cli.js
    (while (and (not (eobp)) (looking-at-p "\\s *\#")) (forward-line 1))
    (skip-chars-forward "\r\n[:blank:]")
    (setq start (point))
    (save-excursion
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
                  (when (string-prefix-p ";" formatted) (setq formatted (substring formatted 1)))
                  (insert formatted)
                  (setq end (point))
                  (let ((inhibit-message t))
                    (goto-char start)
                    (set-mark-command nil)
                    (goto-char end)
                    (indent-for-tab-command))
                  ;; try to restore previous position
                  (when pos-list
                    (goto-line (car pos-list))
                    (move-to-column (car (cdr pos-list)) nil))))))
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
                     (setq server (concat js-format-host "/format/" js-format-style))
                     ;; using backquote to quote the value of data
                     (js-format-http-request local-done server "POST" `,data)))
    (funcall runner)))

(defun js-format-start-server (cb-success)
  "Start node server when needed, call CB-SUCCESS after start succeed."
  (let* ((cmd js-format-command)
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
                              (set-process-sentinel proc #'(lambda (proc event)
                                                             (when (eq (process-status proc) 'exit)
                                                               (message "js-format server exit %s" event))))
                              (message "js-format server start succeed, quit with `js-format-exit'"))))
    ;; monitor startup events
    (set-process-sentinel proc #'(lambda (proc _event)
                                   (when (eq (process-status proc) 'exit)
                                     (if (not (string-match "Cannot find module" all-output))
                                         (message "js-format: %s" (concat "Could not start node server\n" all-output))
                                       (message "Js-format now running `%s` in folder '%s', please wait..." js-format-setup-command js-format-folder)
                                       (let ((default-directory js-format-folder) result)
                                         (setq result (shell-command-to-string js-format-setup-command))
                                         (if (file-exists-p (expand-file-name "node_modules/" js-format-folder))
                                             (js-format-start-server cb-success)
                                           (message "%s\n`%s` failed, goto folder %s, to manually install.\n\n" result js-format-setup-command js-format-folder)))))))))

(defun js-format-setup (&optional style)
  "Exit js-format node server."
  (interactive (list (read-string "js-format style: ")))
  (unless (or (not (stringp style))
              (string= "" style))
    (setq js-format-style style))
  (setq style js-format-style)
  (message "[js-format] \"%s\" setup in background, plesae try format after that." style)
  (let (callback local-done)
    (setf callback #'(lambda()
                       (js-format-setup style)))
    (setf local-done #'(lambda(err)
                         (if err
                             (js-format-start-server callback)
                           (let ((result (prog2 (search-forward "\n\n" nil t)
                                             (buffer-substring (point) (point-max)))))
                             (when (and (stringp result) (not (string= result "")))
                               (message "[js-format] setup result:\n%s" result))))))
    (let ((inhibit-message t))
      (js-format-http-request local-done (concat js-format-host "/setup/" style)))))

(defun js-format-exit ()
  "Exit js-format node server."
  (interactive)
  (js-format-http-request '= (concat js-format-host "/exit")))

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
