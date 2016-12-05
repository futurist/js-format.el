;;; js-format.el --- Format javascript code using node. -*- lexical-binding: t; -*-

;; Filename: js-format.el
;; Description: Format javascript code using node (standard as formatter)
;; Author: James Yang <jamesyang999@gmail.com>
;; Copyright (C) 2016, James Yang, all rights reserved.
;; Time-stamp: <2016-12-05 13:57:46 James Yang>
;; Created: 2016-12-05 13:57:46
;; Version: 0.1.0
;; URL: http://github.com/futurist/js-format.el
;; Keywords: js, javascript, format, standard, formatter, node
;; Package-Requires: ((emacs "24.1") (js2-mode "20160623"))
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

;; Send code to local node server to format its style,
;;  using [standard](http://standardjs.com)

;; ## Install

;; 1. You need NodeJS >= 6 in your system path

;; 2. `js-format.el` is available via MELPA and can be installed via

;;     M-x package-install js-format

;;  If failed, ensure you have

;;     (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;;  line in your package config.

;; 3. After the install, you MUST go into the installed folder, run `npm install`
;;  to install npm dependencies.

;; ## Usage

;; `js-format-mark-statement` to mark current statement under point.

;; `js-format-region` to mark current statement, pass it to *node server*, then get
;;  back the result code to replace the statement.

;; `js-format-buffer` to format the whole buffer.

;; You may also want to bind above func to keys:

;;     (global-set-key (kbd "M-,") 'js-format-mark-statement)
;;     (global-set-key (kbd "C-x j j") 'js-format-region)
;;     (global-set-key (kbd "C-x j b") 'js-format-buffer)

;; ## Customize format style

;; You can rewrite `function format(code, cb){}` function in *formatter.js* file,
;;  to customize your style of format.

;;; Code:

(defvar js-format-proc-name "JSFORMAT")

(defvar js-format-proc-port nil)

(defvar js-format-folder
  (let ((script-file (or load-file-name
                          (and (boundp 'bytecomp-filename) bytecomp-filename)
                          buffer-file-name)))
    (file-name-directory (file-truename script-file)))
  "Root folder of js-format.")

(defvar js-format-command
  (let ((bin-file (expand-file-name "./server.js" js-format-folder)))
    (cons "node" (list (if (file-exists-p bin-file) bin-file (error "js-format: cannot find server.js")))))
  "The command to be run to start the js-format server. Should be a
list of strings, giving the binary name and arguments.")

;;;###autoload
(defun js-format-mark-statement (&optional skip-non-statement)
  (interactive "P")
  (let* ((cur (point))
         (back (js2-backward-sws))
         (last (when (looking-at-p "[\t \n\r]") (forward-char -1)))
         (region-beg (if (use-region-p) (region-beginning) (point-max)))
         (region-end (if (use-region-p) (goto-char (region-end)) (point-min)))
         (cur-node (js2-node-at-point))
         (parent cur-node)
         beg end)
    ;; blacklist: 33=prop-get, 39=name, 40=number, 45=keyword
    ;; 128=scope, 108=arrow function, 66=object
    (while (and parent (or
                        (memq (js2-node-type parent) '(33 39 40 45)) ; always skip name, prop-get, number, keyword
                        (and skip-non-statement (memq (js2-node-type parent) '(66))) ;pure object will fail format
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
(defun js-format-buffer (&optional arg)
  (interactive "P")
  (let ((cur (point)) start)
    (goto-char (point-min))
    ;; skip # line for cli.js
    (while (and (not (eobp)) (looking-at-p "\\s *\#")) (next-line 1))
    (skip-chars-forward "\r\n[:blank:]")
    (setq start (point))
    (goto-char cur)
    (save-excursion
      (let* ((col (current-column))
             (line (line-number-at-pos)))
        (js-format-region start (point-max) nil `(,line ,col) t)))))

;;;###autoload
(defun js-format-line (&optional arg)
  (interactive "P")
  (save-excursion
    (let* ((pos (point))
           (col (current-column))
           (line (line-number-at-pos)))
      (goto-char (line-beginning-position))
      (skip-chars-forward "\t \n\r")
      (js-format-region (point) pos nil `(,line ,col)))))

;;;###autoload
(defun js-format-region (start end &optional not-jump-p pos-list reset-after)
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
            (lambda (formatted)
              (setq success (not (string-prefix-p errorsign formatted) ))
              (switch-to-buffer cur-buffer)
              (if (string= "" formatted)
                  (message "js-format return nil")
                (if (not success)
                    (progn (deactivate-mark)
                           (string-match "\"index\":\\([0-9]+\\)" formatted)
                           (setq error-pos (+ start (string-to-number (or (match-string 1 formatted) "")) ))
                           (unless not-jump-p  (goto-char error-pos))
                           (message "js-format error: %s" (car (split-string formatted errorsign t)) ) )
                  (delete-region start end)
                  (when (string-prefix-p ";" formatted) (setq formatted (substring formatted 1)))
                  (insert formatted)
                  (delete-char -1)  ;; js-format will add new line, don't need it
                  (js2-indent-region start (point))
                  ;; try to restore previous position
                  (when pos-list
                    (goto-line (car pos-list))
                    (move-to-column (car (cdr pos-list)) nil)
                    )
                  ;; js2-mode-reset after format
                  (when reset-after
                    (js2-mode-reset))))))
      (js-format-run result get-formatted))))

(defun js-format-result (errorlist)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  ;; (decode-coding-string "\\225\\357" 'utf-8) convert  unibyte string to Chinese!!!
  ;; (message "-------%s -%s -%s" (buffer-name) errorlist (prog1 (buffer-string) ))
  (when (not errorlist)
    (let ((result (prog2 (search-forward "\n\n" nil t) (buffer-substring (point) (point-max)) (kill-buffer))))
      ;; (message "%s" (decode-coding-string result 'utf-8)))
      (setf result (decode-coding-string result (symbol-value 'buffer-file-coding-system))))))

(defun js-format-run (data done)
  (let* ((host "http://localhost")
         (method "POST")
         server callback runner local-done)
    (setf local-done (lambda(s)
                       (let ((result (prog2 (search-forward "\n\n" nil t) (buffer-substring (point) (point-max)) (kill-buffer))))
                         (setf result (decode-coding-string result (symbol-value 'buffer-file-coding-system)))
                         (funcall done result))))
    (setf callback (lambda ()
                     (funcall runner)))
    (setf runner (lambda()
                   ;; (http-request 'js-format-result server method `,data) ; using backquote to quote the value of data
                   (setq server (concat host ":" (number-to-string (or js-format-proc-port 8000))))
                   ;; using backquote to quote the value of data
                   (http-request local-done server method `,data)))
    (if (or (get-process js-format-proc-name) js-format-proc-port)
        (funcall runner)
      (js-format-start-server callback))))

(setq debug-on-error t)

(defun js-format-start-server (cb-success)
  (let* ((cmd js-format-command)
         (proc (apply #'start-process js-format-proc-name nil cmd))
         (all-output ""))
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc (lambda (_proc _event)
                                 (delete-process proc)
                                 (setf js-format-proc-port nil)
                                 (if (not (string-match "Cannot find module" all-output))
                                     (message "js-format: %s" (concat "Could not start node server\n" all-output))
                                   (message "Js-format is now running `npm install`, please wait...")
                                   (shell-command (concat "cd \"" js-format-folder "\" && cnpm install")))))
    (set-process-filter proc
                        (lambda (proc output)
                          (if (and (not (string-match "Listening on port \\([0-9][0-9]*\\)" output))
                                   (not (string-match "EADDRINUSE .*:\\([0-9][0-9]*\\)" output)))
                              (setf all-output (concat all-output output))
                            (setf js-format-proc-port (string-to-number (match-string 1 output)))
                            (set-process-sentinel proc (lambda (proc _event)
                                                         (delete-process proc)
                                                         (setf js-format-proc-port nil)
                                                         (message "js-format server exit %s" _event)))
                            (set-process-filter proc nil)
                            (funcall cb-success))))))

(defun http-request (callback url &optional method args)
  "Send ARGS to URL as a POST request."
  ;; Usage: (http-request 'callback "http://myhost.com" "POST" '(("name" . "your name")))
  (let ((url-request-method (or method "GET"))
        (url-request-extra-headers '(("Content-Type" . "text/plain")))
        (url-request-data (base64-encode-string (encode-coding-string args 'utf-8))))
    (url-retrieve url callback)))



(provide 'js-format)
;;; js-format.el ends here
