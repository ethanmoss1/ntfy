;;; ntfy.el --- publish notification using ntfy.sh -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Shom

;; Author: Shom Bandopadhaya <shom@bandopadhaya.com>
;; Created: 2022-04-30
;; Modified: 2025-01-05
;; Version: 0.2.0
;; Keywords: ntfy notification push-notification pub-sub
;; Package-Requires: ((emacs "27.2"))
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Interface to use the https://ntfy.sh service (or self-hosted version) to send
;; notification from Emacs.

;;; Code:
(require 'url)

(defgroup ntfy ()
  "Notification publishing in Emacs")

(defcustom ntfy-server nil
  "Set server for ntfy service."
  :group 'ntfy
  :type 'string)

(defcustom ntfy-topic nil
  "Set ntfy topic/channel."
  :group 'ntfy
  :type 'string)

(defcustom ntfy-header nil
  "Set header message for the notification."
  :group 'ntfy
  :type 'string)

(defcustom ntfy-tags nil
  "Set the emoji that'll appear before the header message.
Use comma separated string, see https://ntfy.sh/docs/publish/#tags-emojis for details."
  :group 'ntfy
  :type 'string)

(defcustom ntfy-priority 3
  "Set the message priority for the notification."
  :group 'ntfy
  :type '(choice (const :tag "urgent/max" 5)
                 (const :tag "high" 4)
                 (const :tag "default" 3)
                 (const :tag "low" 2)
                 (const :tag "min" 1)))


;;;###autoload
(defun ntfy-send-message (message)
  "Send ad-hoc MESSAGE from mini-buffer as notification."
  (interactive "sEnter message:")
  (ntfy--publish-message message))

;;;###autoload
(defun ntfy-send-message-with-header (header message)
  "Send ad-hoc MESSAGE from mini-buffer with custom HEADER as notification."
  (interactive "sEnter header: \nsEnter message: ")
  (setq ntfy-header header)
  (ntfy--publish-message message header))

;;;###autoload
(defun ntfy-send-message-with-header-and-tags (tags header message)
  "Send ad-hoc MESSAGE from mini-buffer.
  Custom HEADER and TAGS are set for the notification."
  (interactive "sEnter tags (emoji codes, comma separated no spaces): \nsEnter header: \nsEnter message: ")
  (ntfy--publish-message message header tags))

(defun ntfy--publish-message (message &optional header tags priority)
  "Publish message to server with Emacs Lib URL with MESSAGE.
  Configured HEADER and TAGS are used unless specified."
  ;; Check the inputs.
  (ntfy--check-inputs message
                      (if header header)
                      (if tags tags))

  ;; If no error is thrown, send the message.
  (let ((url-request-method "POST")
        (url-request-data message)
        (url-request-extra-headers `(("Title" . ,(or header ntfy-header))
                                     ("Tags" . ,(or tags ntfy-tags))
                                     ("Priority" . ,(int-to-string (or priority ntfy-priority))))))
    (url-retrieve-synchronously (format "%s/%s" ntfy-server ntfy-topic))))

;;;###autoload
(defun ntfy-send-url (url)
  "Send URL from mini-buffer."
  (interactive "sEnter URL: \n")
  (ntfy--publish-url url))

(defun ntfy--publish-url (url)
  "Publish URL to server with Emacs Lib URL."
  (let ((url-request-method "POST")
        (url-request-data (concat "Click to follow the shared URL;\n" url))
        (url-request-extra-headers `(("Title" . "Emacs shared a URL")
                                     ("Tags" . "link")
                                     ("Actions" . ,(format "view, View Link, %s" url url)))))
    (url-retrieve-synchronously (format "%s/%s" ntfy-server ntfy-topic))))

(defun ntfy--check-inputs (message &optional header tags)
  "Validates HEADER, MESSAGE, and TAGS for newlines before sending.

  HEADER, MESSAGE, and TAGS must be strings without newline characters.
  If any argument contains a newline, signals a user-error."
  ;; Check the 'message' string for a newline character
  ;; TODO Message checks?

  (when (string-match-p "\n" header)
    (user-error "Notification header cannot contain a newline"))

  ;; Regex will only match if every character is one of the following;
  ;;  - Any lowercase letter a-z OR
  ;;  - Any lowercase letter, a comma or underscore, followed by any lowercase letter
  (unless (string-match-p "^\\([a-z]\\|[a-z][,_][a-z]\\)+$" tags)
    (user-error "Notification Tags cannot contain anything other than the lower case
characters a-z, a comma, or an underscore")))


(provide 'ntfy)
;;; ntfy.el ends here
