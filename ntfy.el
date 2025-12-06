;;; ntfy.el --- publish notification using ntfy.sh -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Shom

;; Author: Shom Bandopadhaya <shom@bandopadhaya.com>
;; Version 0.3.0
;; Keywords: lisp
;; Package-Requires: ((emacs "27.2"))
;; SPDX-License-Identifier: MIT

;; This file is not part of GNU Emacs.

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
;; Interface to use the https://ntfy.sh service (or self-hosted version) to send
;; notification from Emacs.

;; TODO: Add a list of all the possible tags that result in emojis in the header
;; TODO: The doc strings need to be done.
;; TODO: Readme needs to be crafted

;; New features to add;
;; TODO: Using dired marked, make a ntfy to send the files?.

;;; Code:
(require 'url)  ; Built-in

;;;--- Variables
(defgroup ntfy ()
  "Notification publishing in Emacs")

(defcustom ntfy-server nil
  "Set server for ntfy.el to send notifications."
  :group 'ntfy
  :type 'string)

(defcustom ntfy-topic nil
  "Set ntfy topic/channel to send notifications."
  :group 'ntfy
  :type 'string)

(defcustom ntfy-title nil
  "Set the default title of the message for the notification."
  :group 'ntfy
  :type 'string)

(defcustom ntfy-tags nil
  "Set the emoji that'll appear before the header message.
Use comma separated string, see
https://ntfy.sh/docs/publish/#tags-emojis for details."
  :group 'ntfy
  :type '(repeat string))

(defcustom ntfy-priority 3
  "Set the message priority for the notification. This ranges from Minimum (1) to Urgent/Maximum (5)"
  :group 'ntfy
  :type '(choice (const :tag "urgent/max" 5)
                 (const :tag "high" 4)
                 (const :tag "default" 3)
                 (const :tag "low" 2)
                 (const :tag "min" 1)))

;;;--- User Functions
;;;###autoload
(defun ntfy-message (message)
  "A simple way of sending a notification message"
  (interactive "sMessage:")
  (ntfy--publish-message message))

;;;###autoload
(defun ntfy-message-with-title (title message)
  "A simple way of sending a notification message with a title"
  (interactive "sTitle: \nsMessage: ")
  (ntfy--publish-message message :title title))

;;;###autoload
(defun ntfy-message-with-title-and-tags (title message)
  "A simple way of sending a notification message with a title and tag(s)"
  (interactive "sTitle: \nsMessage: ")
  (let ((tags (ntfy--interactive-emoji-selector)))
    (ntfy--publish-message message :title title :tags tags)))

;;;###autoload
(defun ntfy-change-tags ()
  "Update the tags for ntfy messages interactively.

This allows you to select emojis that are valid to show in ntfy messages
and update the variable that holds them."
  (interactive)
  (let ((emojis (ntfy--interactive-emoji-selector)))
    (setq ntfy--tags-emojis emojis)
    (setq ntfy-tags (append emojis nil))))


;;;--- Internal Functions
(defun ntfy--interactive-emoji-selector ()
  "Interactively select multiple emojis and return them as a vector of
comma-separated tags."
  (interactive)
  (let* ((emoji-file (expand-file-name "emoji-list"))  ; TODO this is broken
         (emoji-list (if (file-exists-p emoji-file)
                         (with-temp-buffer
                           (insert-file-contents emoji-file)
                           (read (current-buffer)))
                       (error "Emoji list file not found at: %s" emoji-file)))
         (emojis-selected ())
         (selection nil))
    ;; This will continue until a blank selection is done or M-Ret is pressed.
    (while (not (string-empty-p selection))
      (setq selection (completing-read "Select an emoji (M-Ret to exit): " emoji-list))
      (unless (string-empty-p selection)
        (let* ((selected-pair (cl-find selection emoji-list
                                       :key 'car
                                       :test 'string-equal))
               (emoji-tag (cdr selected-pair)))
          (setq emojis-selected(append emojis-selected (list emoji-tag))))))
    ;; Return as a vector
    emojis-selected))

(defun ntfy--attach (attachment)
  "Check the options for the :attach property, and check if it is a local
or remote file.
NOTE: Currently, local files are not accepted as part of a limitation of
’publish as json’"
  (let* ((type (url-type (url-generic-parse-url attachment))))
    (cond ((member type '("http" "https")) attachment) ;; URL
          ((file-name-absolute-p attachment)  ; Local File
           (encode-coding-string (with-temp-buffer
                                   (insert-file-contents
                                    "Downloads/cute-cat.jpg")
                                   (buffer-string))
                                 'utf-8)))))

(defun ntfy--publish-message (message)
  ""
  (message message))

(provide 'ntfy)
;;; ntfy.el ends here
