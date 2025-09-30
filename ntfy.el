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
;; TODO: Using dired marked, make a ntfy to send the files?. (not currenly possible)

;; issues:
;; NOTE: Publish as JSON currently doesnt support sending local attachments ...

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

It is important to sent this via ‘setopt’ or via ‘customise’."
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
(defun ntfy-message-with-title-and-tags (title message)
  "A simple way of sending a notification message with a title and tag(s)"
  (interactive "sTitle: \nsMessage: ")
  (let ((tags (ntfy--interactive-emoji-selector)))
    (ntfy--generate-valid-plist `( :title ,title
                                   :message ,message
                                   :tags ,tags))))

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
    (vconcat emojis-selected)))

(defun ntfy--generate-valid-plist (options)
  "This function takes a plist to uses it to createa complete and valid
plist to be converted to JSON.

It provides default values for all supported options if they are missing
from the OPTIONS plist.

:topic The topic to send the notification message.
If :topic is supplied, this is the topic that the message is sent to. If
this is not supplied, the fallback of the variable ‘ntfy-topic’ is used.
Otherwise, the topic is set to \"emacs\".

:title The title of the notification.
If sets title of the message to the string supplied. Otherwise, the
title is set to the value of `ntfy-title’. If that variable is not set,
it defaults to \"No Title\".

:tags Tags to apply to the notification.
Expects a list or vector of strings, that is converted to a vector, to
be used as tags for the notification. Tags are converted to emojis if
they are part of the following list;
https://ntfy.sh/docs/publish/#tags-emojis If no tags are supplied, it
uses the value of ‘ntfy--tags-emojis’, otherwise we use the tag
\"link\".

:priority The priority level of the notification.
An integer from 1 (lowest) to 5 (highest) to set the notification's
priority. if no value is given, ‘ntfy-priority’ is used, otherwise defaults
to 3.

:message The main body content of the notification.
The primary text to be displayed in the notification body. It defaults to
an empty string (\"\") if not provided in OPTIONS.

TODO: Rest of these vvv

:attach A URL to a file to attach to the notification.
Specifies a URL to an attachment that should be included with the
notification. It defaults to ‘nil’ which makes it do nothing.

:filename Filename of the attached local or remote file.

If nothing is supplied, the file in :attach it defaults to the original
filename.

:click The URL to open when the user clicks the notification.
A URL that the notification client (e.g., mobile app) should attempt to open
when the user interacts with the notification. It defaults to `nil`.

:actions A list of actions/buttons to display with the notification.
A list of action objects (as described in ntfy documentation) to add
interactive buttons to the notification. It defaults to `nil`.

:delay A duration to delay the delivery of the notification.
A string specifying a delay (e.g., \"10m\", \"1h\") before the notification
should be delivered to subscribers. It defaults to `nil`."
  (unless (plistp options)
    (user-error "The input OPTIONS is not of type plist."))
  (let* ((valid-plist  ; Form a valid plist from the input plist provided
          `(;; TODO Check topic is valid?
            :topic ,(or (plist-get options :topic)
                        ntfy-topic
                        "emacs")
            ;; TODO function that checks valid title before?
            :title ,(or (plist-get options :title)  ; does this contain new lines?
                        ntfy-title
                        "No Title")
            :tags ,(or (plist-get options :tags)
                       ntfy--tags-emojis
                       ["link"])
            :priority ,(or (plist-get options :priority)
                           ntfy-priority
                           3)
            :message ,(or (plist-get options :message)
                          "")
            :attach ,(ntfy--attach-function options)
            ;; Only use filename if :attach option is provided.
            :filename ,(ntfy--filename-function options)
            ;; TODO: the rest of these; vvv
            :click ,(or (plist-get options :click)
                        nil)
            :actions ,(or (plist-get options :actions)
                          nil)
            :delay ,(or (plist-get options :delay)
                        nil))))
    (ntfy--publish-message-plist valid-plist)))

(defun ntfy--attach-function (options)
  "Check the options for the :attach property, and check if it is a local
or remote file.
NOTE: Currently, local files are not accepted as part of a limitation of
’publish as json’"
  (let* ((attach (plist-get options :attach))
         (type (url-type (url-generic-parse-url attach))))

    (cond ((member type '("http" "https")) attach) ;; URL
          ((file-name-absolute-p attach)  ; Local File
           (user-error "Local files not supported via ’publish as JSON’ in ntfy")
           ;; ;; Current "implementation" of a feature that doesnt currently exist
           ;; (let ((file-size (file-attribute-size (file-attributes attach))))
           ;;   (if (>= file-size (* 15 1024 1024))
           ;;       (user-error "File is larger than 15MB. File is %sMBs"
           ;;                   (/ file-size 1024 1024))
           ;;     ( :name "attachment.jpg")
           ;;     (with-temp-buffer
           ;;       (insert-file-contents-literally attach)
           ;;       (buffer-string))))
           ))))

(defun ntfy--filename-function (options)
  ""
  (when (plist-get options :attach)
    (plist-get options :filename)))

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
