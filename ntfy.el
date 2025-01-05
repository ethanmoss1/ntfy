;;; ntfy.el --- publish notification using ntfy.sh -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Shom

;; Author: Shom Bandopadhaya <shom@bandopadhaya.com>
;; Created: 2022-04-30
;; Modified: 2022-05-04
;; Version: 0.1.2
;; Keywords: ntfy notification push-notification pub-sub
;; Package-Requires: ((emacs "27.2") (url "22.1"))
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Interface to use the https://ntfy.sh service (or self-hosted version) to send notification from Emacs.
;;
;;; Code:
(require 'url)

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

(defun ntfy-send-message (message)
  "Send ad-hoc MESSAGE from mini-buffer as notification."
  (interactive "sEnter message:")
  (ntfy--publish-message message))

(defun ntfy-send-message-with-header (header message)
  "Send ad-hoc MESSAGE from mini-buffer with custom HEADER as notification."
  (interactive "sEnter header: \nsEnter message: ")
  (setq ntfy-header header)
  (ntfy--publish-message message header))

(defun ntfy-send-message-with-header-and-tags (tags header message)
  "Send ad-hoc MESSAGE from mini-buffer.
Custom HEADER and TAGS are set for the notification."
  (interactive "sEnter tags (emoji codes, comma separated no spaces): \nsEnter header: \nsEnter message: ")
  (ntfy--publish-message message header tags))

(defun ntfy--publish-message (message &optional header tags)
  "Publish message to server with Emacs Lib URL with MESSAGE.
Configured HEADER and TAGS are used unless specified."
  (let ((url-request-method "POST")
        (url-request-data message)
        (url-request-extra-headers `(("Title" . ,(or header ntfy-header))
                                     ("Tags" . ,(or tags ntfy-tags)))))
    (url-retrieve-synchronously (format "%s/%s" ntfy-server ntfy-topic))))

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

(provide 'ntfy)
;;; ntfy.el ends here
