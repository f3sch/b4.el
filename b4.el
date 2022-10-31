;;; b4.el --- A small library for wrapper functions around b4 -*- lexical-binding: t -*-
;;
;; Author: Felix Schlepper
;; Maintainer: Felix Schlepper
;; Copyright (C) 2022 Felix Schlepper
;; Created: Sun Oct  9 12:13:42 2022 (+0200)
;; URL: https://github.com/f3sch/b4.el
;; Version: 0.1
;; Package-Requires: ((emacs "28.1") (magit "3.0.0"))
;; Keywords: magit, mail, vc
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Required packages/libraries

;; For nice menus
(require 'transient)
;; Git stuff
(require 'magit)
(require 'vc-git)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization

(defgroup b4 nil
  "The b4 wrapper function library."
  :group 'convenience)

(defcustom b4-git-repo nil
  "Default directory for executing b4 in.
Should probably be your kernel git repository."
  :group 'b4
  :type '(directory))

(defun b4--git-repo-show ()
  "Show current repository."
  (concat "Repository at " (propertize (format "%s" b4-git-repo) 'face 'success)))

(defun b4--git-repo-choose ()
  "Choose git repository locations."
  (interactive)
  (setq b4-git-repo (expand-file-name (read-directory-name "Repository"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Maintainer-oriented:

;; TODO
(defun b4--am ()
  "Create a mailbox file that is ready for git-am.
Argument MSGID The message-id.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Contributor-oriented:

;; TODO
;; Prep Command
;; Preparing a patch series. Either by creating a new branch org enrolling an existing one.

;;;### autoload
(defun b4-prep ()
  "(EXPERIMENTAL) prepare your series for submission."
  (interactive)
  (b4--prep-transient))

(transient-define-prefix b4--prep-transient ()
  "Prepare your patch series for submission."
  :incompatible '(("n" "e"))
  [["General"
    ("R" b4--git-repo-choose :description b4--git-repo-show)
    ("q" transient-quit-one :description "Quit")]]
  [["Arguments"
    ("-h" "Show help message and exit" "--help")
    ("-c" "Automatically populate cover letter trailers with To and Cc addresses" "--auto-to-cc")
    ("-p" "Output prep-tracked commits as patches" "--format-patch=")
    ("-e" "Edit the cover letter" "--edit-cover-letter")
    ("-s" "Show current series revision number" "--show-revision")
    ("-f" "Force revision to be this number instead" "--force-revision=")
    ("-m" "Mark current revision as send and reroll (requires cover letter msgid)" "--manual-reroll=")]]
  [["Create new branch"
    (b4--prep-new)]
   ["Enroll existing branch"
    (b4--prep-enroll)]])

(transient-define-argument b4--prep-new ()
  "Create a new branch."
  :argument "new_series_name="
  :shortarg "n"
  :description "From a new branch"
  :class 'transient-option
  :reader 'magit-read-string-ns
  :prompt "New Branch: ")

(transient-define-argument b4--prep-enroll ()
  "Enroll existing branch as fork base."
  :argument "enroll_base="
  :shortarg "e"
  :description "From a branch, tag or commit"
  :class 'transient-option
  :choices (vc-git-branches))

(transient-define-suffix b4--prep-run (&optional args)
  "Run `b4 prep` with all provided arguments."
  (interactive (list (transient-args transient-current-command)))
  (let* ((buffer "*b4*")
         (default-directory b4-git-repo)
         (name "b4")
         (command "b4 prep"))
    (start-process name buffer command args)))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility

(provide 'b4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; b4.el ends here
