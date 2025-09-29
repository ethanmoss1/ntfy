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

;; TODO: tags in most of the functions need converting to vector of strings as
;; well as a check to ensure they are
;; TODO: Add a list of all the possible tags that result in emojis in the header
;; TODO: The doc strings need to be done.
;; TODO: Readme needs to be crafted

;;; Code:
(require 'url)  ; Built-in
(require 'json)  ; Built-in

;;;--- Variables
(defgroup ntfy () "Notification publishing in Emacs")

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
https://ntfy.sh/docs/publish/#tags-emojis for details.

It is important to sent this via ’setopt’ or via the customise interface."
  :group 'ntfy
  :type '(repeat string)
  :set (lambda (sym defs)
         (custom-set-default sym defs)
         (setq ntfy--vector-tags (vconcat ntfy-tags))))

(defcustom ntfy-priority 3
  "Set the message priority for the notification. This ranges from Minimum (1) to Urgent/Maximum (5)"
  :group 'ntfy
  :type '(choice (const :tag "urgent/max" 5)
                 (const :tag "high" 4)
                 (const :tag "default" 3)
                 (const :tag "low" 2)
                 (const :tag "min" 1)))

(defvar ntfy--vector-tags []
  "This variable is set by the ntfy-tags customise variable when set.

It is a vector that contains strings that each are a tag sent to the
ntfy server.")

;; TODO: Is it worth doing this?
(defvar ntfy--tags-emojis nil
  "A list of all the possible tags that result in an emoji in the header")

;;;--- User Functions
;;;###autoload
(defun ntfy-message (message)
  "A simple way of sending a notification message"
  (interactive "sMessage:")
  (ntfy--generate-valid-plist `(:message ,message)))

;;;###autoload
(defun ntfy-message-with-title (title message)
  "A simple way of sending a notification message with a title"
  (interactive "sTitle: \nsMessage: ")
  (ntfy--generate-valid-plist `( :title ,title
                                 :message ,message)))

;;;###autoload
(defun ntfy-message-with-title-and-tags (title message tags)
  "A simple way of sending a notification message with a title and tag(s)"
  (interactive "sTitle: \nsMessage: \nsTags: ")
  (ntfy--generate-valid-plist `( :title ,title
                                 :message ,message
                                 :tags ,(vconcat tags))))

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
    (vconcat emojis-selected)))

(defun ntfy--generate-valid-plist (options)
  "This function takes a incomplete or complete plist to uses it to create
a complete and valid plist to be converted to JSON.

:topic  The topic to send the notification message.
If topic is supplied, this is the topic that the message is sent to. If
this is not sent, the fallback of the variable `ntfy-topic’ is used,
Otherwise the topic is set to ’emacs’.

:title The title of the notification
By def .....

:another1

...
"
  (unless (plistp options)
    (user-error "The input OPTIONS is not of type plist."))
  (let* ((valid-plist  ; Form a valid plist from the input plist provided
          `(;; TODO Check topic is valid?
            :topic ,(or (plist-get options :topic) ntfy-topic "emacs")
            ;; TODO function that checks title before.
            :title ,(or (plist-get options :title) ntfy-title "No Title")
            :tags ,(or (plist-get options :tags) (vconcat ntfy-title) ["link"])  ; (or (plist-get options :tags) ntfy-tags nil))
            :priority ,(or (plist-get options :priority) ntfy-priority 3)
            :message ,(or (plist-get options :message) "")
            :attach ,(or (plist-get options :attach) nil)
            :filename ,(or (plist-get options :filename) nil)
            :click ,(or (plist-get options :click) nil)
            :actions ,(or (plist-get options :actions) nil)
            :delay ,(or (plist-get options :delay) nil))))
    (ntfy--publish-message-plist valid-plist)))

(defun ntfy--publish-message-plist (valid-plist &optional server)
  "TODO
This uses the blah-blah function to validate a plist ready for encoding
to json. This is sent of to the ntfy server unless the optional argument
is given for a different server"
  (let ((url-request-method "POST")
        (url-request-data (json-encode valid-plist)))
    (message url-request-data)
    (url-retrieve-synchronously (or server ntfy-server))))

;;; ntfy-json.el ends here
