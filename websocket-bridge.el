;;; websocket-bridge.el --- Bridge between for websocket and elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'websocket)
(require 'ansi-color)

(defvar websocket-bridge-app-list (list))
(defvar websocket-bridge-server nil)
(defvar websocket-bridge-server-port nil)

(defun websocket-bridge-get-free-port ()
  "Randomly get port."
  (save-excursion
    (let* ((process-buffer "*temp*")
           (process
            (make-network-process
             :name process-buffer
             :buffer process-buffer
             :family 'ipv4
             :server t
             :host "127.0.0.1"
             :service t))
           port)
      (setq port (process-contact process))
      (delete-process process)
      (kill-buffer process-buffer)
      (format "%s" (cadr port)))))

(defun websocket-bridge-message-handler (_websocket frame)
  "Message handler for given FRAME."
  (let* ((info (ignore-errors (json-parse-string (websocket-frame-text frame))))
         (info-type (when (hash-table-p info)
                      (gethash "type" info nil))))
    (pcase info-type
      ("client-app-name"
       (set
        (intern
         (format "websocket-bridge-client-%s"
                 (gethash "content" info nil)))
        _websocket))
      ("show-message" (message (gethash "content" info nil)))
      ("eval-code" (eval (read (gethash "content" info nil))))
      ("fetch-var"
       (websocket-send-text _websocket
                            (json-encode
                             (eval
                              (read (gethash "content" info nil)))))))))

(defun websocket-bridge-server-start ()
  "Start websocket bridge server."
  (interactive)
  (if websocket-bridge-server
      (message "[WebsocketBridge] Server has start.")
    (progn
      (setq
       websocket-bridge-server-port
       (websocket-bridge-get-free-port)
       websocket-bridge-server
       (websocket-server
        websocket-bridge-server-port
        :host 'local
        :on-message #'websocket-bridge-message-handler
        :on-close (lambda (_websocket))))
      (message
       (format "[WebsocketBridge] Server start %s"
               websocket-bridge-server)))))

(defun websocket-bridge-app-start (app-name command extension-path)
  "Start APP-NAME running COMMAND with args EXTENSION-PATH."
  (if (member app-name websocket-bridge-app-list)
      (message "[WebsocketBridge] Application %s has start." app-name)
    (let* ((process
            (intern (format "websocket-bridge-process-%s" app-name)))
           (process-buffer
            (format " *websocket-bridge-app-%s*" app-name)))
      (progn
        ;; Start process.
        (setq process
              (start-process-shell-command
               app-name
               process-buffer
               (format "%s %s %s %s"
                       command
                       extension-path
                       app-name
                       websocket-bridge-server-port)))
        ;; Make sure ANSI color render correctly.
        (set-process-sentinel
         process
         (lambda (p _m)
           (when (eq 0 (process-exit-status p))
             (with-current-buffer (process-buffer p)
               (ansi-color-apply-on-region (point-min) (point-max))))))

        (add-to-list 'websocket-bridge-app-list app-name t)))))

(defun websocket-bridge-server-exit ()
  "Exit Websocket Bridge server."
  (interactive)
  (if websocket-bridge-server
      (progn
        (websocket-server-close websocket-bridge-server)
        (setq websocket-bridge-server nil)
        (setq websocket-bridge-server-port nil)
        (message "[WebsocketBridge] Server has exited."))
    (message "[WebsocketBridge] Server not exist.")))

(defun websocket-bridge-app-exit (&optional app-name)
  "Exit websocket-bridge-app APP-NAME."
  (interactive)
  (cond
   ((not websocket-bridge-app-list)
    (message "[WebsocketBridge] There is no Application."))
   ((and app-name (not (member app-name websocket-bridge-app-list)))
    (message "[WebsocketBridge] Application %s not exist." app-name))
   (t
    (let* ((app-name
            (if (member app-name websocket-bridge-app-list)
                app-name
              (completing-read "[WebsocketBridge] Exit application: "
                               websocket-bridge-app-list))))
      (if (member app-name websocket-bridge-app-list)
          (let* ((process
                  (intern-soft
                   (format "websocket-bridge-process-%s" app-name)))
                 (process-buffer
                  (format " *websocket-bridge-app-%s*" app-name)))
            (when process
              (kill-buffer process-buffer)
              (makunbound process))
            (setq websocket-bridge-app-list
                  (delete app-name websocket-bridge-app-list))
            (makunbound
             (intern (format "websocket-bridge-client-%s" app-name))))
        (message "[WebsocketBridge] Application %s has exited." app-name))))))

(defun websocket-bridge-call (app-name &rest func-args)
  "Call APP-NAME with FUNC-ARGS."
  (if (member app-name websocket-bridge-app-list)
      (websocket-send-text
       (symbol-value
        (intern-soft (format "websocket-bridge-client-%s" app-name)))
       (json-encode (list "data" func-args)))
    (message "[WebsocketBridge] Application %s has exited." app-name)))

(defun websocket-bridge-app-open-buffer (app-name)
  "Open buffer for websocket-bridge-app APP-NAME if exists."
  (let ((app-process-buffer
         (get-buffer (format " *websocket-bridge-app-%s*" app-name))))
    (when app-process-buffer
      (pop-to-buffer app-process-buffer)
      (special-mode))))

(defun websocket-bridge-app-log-buffer ()
  "Interactively select an websocket-bridge app and open the log."
  (interactive)
  (let ((app-name
         (completing-read "Select websocket bridge app: " websocket-bridge-app-list)))
    (when (member app-name websocket-bridge-app-list)
      (websocket-bridge-app-open-buffer app-name))))

;; Default start websocket-bridge-server
(websocket-bridge-server-start)

(provide 'websocket-bridge)
;;; websocket-bridge.el ends here
