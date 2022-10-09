;;; b4.el --- A small library for wrapper functions around b4 -*- lexical-binding: t -*-
;;
;; Author: Felix Schlepper
;; Maintainer: Felix Schlepper
;; Copyright (C) 2022 Felix Schlepper
;; Created: Sun Oct  9 12:13:42 2022 (+0200)
;; URL: https://github.com/f3sch/b4.el
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: maint, mail, vc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  b4.el represent an assortment of wrapper functions for the b4
;;  command-line utility used by the Linux kernel community for either
;;  contributing patches or maintaining patches.
;;  See https://b4.docs.kernel.org/en/latest/index.html for more information.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'transient)

;;; Maintainer-oriented:

;; TODO
(defun b4--am (msgid)
  "Create an mbox file that is ready for git-am.
Argument MSGID The message-id.")

;;; Contributor-oriented:

;; TODO
;; Prep Command
;; Preparing a patch series. Either by creating a new branch org enrolling an existing one.

(defun b4--prep ()
  "(EXPERIMENTAL) prepare your series for submission."
  (interactive)
  (b4--prep-transient))

(transient-define-prefix b4--prep-transient ()
  "Prepare your patch series for submission."
  [["Arguments"
    ("-h" "Show help message and exit" "--help")
    ("-c" "Automatically populate cover letter trailers with To and Cc addresses" "--auto-to-cc")
    ("-p" "Output prep-tracked commits as patches" "--format-patch=")
    ("-e" "Edit the cover letter" "--edit-cover-letter")
    ("-s" "Show current series revision number" "--show-revision")
    ("-f" "Force revision to be this number instead" "--force-revision=")
    ("-m" "Mark current revision as send and reroll (requires cover letter msgid)" "--manual-reroll=")]]
  [["Create new branch"
    ("n" "Create a new branch for working on a patch series" (lambda () (interactive) (message "New branch")))]
   ["Enroll existing branch"
    ("e" "Enroll current branch, using the passed tag, branch, or commit as fork base" (lambda () (interactive) (message "Enroll current branch")))]])

;; TODO
(defun b4--send ()
  "(EXPERIMENTAL) send you series for review on distribution lists.")


;; TODO
(defun b4--trailers ()
  "(EXPERIMENTAL) retrieve and apply code-review trailers.")

;;; Utility Functions
;; These are split in either getting the message-id from a file or from an email.

;; Files
(defun b4--files-get-msgid ()
  "Get the message-id from a file.")

;; Mail
(defun b4--mail-get-msgid ()
  "Get the message-id from an email.")

(provide 'b4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; b4.el ends here
