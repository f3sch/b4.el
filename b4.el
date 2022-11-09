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

(require 'transient)
(require 'magit nil t)
(require 'vc-git)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants

(defconst b4--magic-marker
  "'--- b4-submit-tracking ---'"
  "This is the b4 magic marker.

It marks for example the cover-letter commit.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customization

(defgroup b4 nil
  "The b4 wrapper function library."
  :group 'convenience
  :package-version '(b4 . "0.1")
  :link '(url-link "https://b4.docs.kernel.org/en/latest/index.html"))

(defcustom b4-repo-dir nil
  "Default directory for executing b4 in.
Should probably be your kernel git repository."
  :package-version '(b4 . "0.1")
  :type '(directory))

;; mode map
(defvar b4-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "Q") 'kill-buffer)
    map)
  "Keymap for `b4-mode'.")

;; major mode
(define-derived-mode b4-mode special-mode "b4"
  :group 'b4
  "Special mode for b4 buffers.

\\{b4-mode-map}"
  (buffer-disable-undo)
  (setq truncate-lines t))

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
;; Preparing a patch series. Either by creating a new branch or enrolling an existing one.

(transient-define-prefix b4-prep ()
  "(EXPERIMENTAL) prepare your series for submission."
  [["General"
    (b4--repo-pick-transient)
    (b4--show-help)
    ("q" transient-quit-one :description "Quit")]]
  ["Information"
   (b4--show-current-revision)]
  [["Arguments"
    ("-c" "Automatically populate cover letter trailers with To and Cc addresses" "--auto-to-cc")
    ("-f" "Force revision to be this number instead" "--force-revision=")
    ("-m" "Mark current revision as send and reroll (requires cover letter msgid)" "--manual-reroll=")]
   ["Actions"
    (b4--prep-edit-cover-letter)
    (b4--prep-output)]]
  [["Create new branch"
    (b4--prep-new)]
   ["Enroll existing branch"
    (b4--prep-enroll)]])

(transient-define-suffix b4--prep-new ()
  "Prepare a patch series from a new branch."
  :key "n"
  :description "From new Branch"
  (interactive)
  (let ((branch (magit-read-string-ns "Branch name: ")))
    (message "Branch Name: %s" branch)))

(transient-define-suffix b4--prep-edit-cover-letter ()
  "Edit the cover letter."
  :key "ae"
  :transient t
  :description "Edit the cover letter"
  (interactive)
  (let ((commit (b4--find-cover-commit)))
    (message "Cover-letter id is %s" commit)))

(transient-define-suffix b4--prep-output ()
  "Output prep-tracked commits as patches."
  :key "ap"
  :transient t
  :description "Output prep-tracked commits as patches"
  (interactive)
  (let* ((outdir (expand-file-name (read-directory-name "Output Directory: ")))
         (default-directory (if b4-repo-dir
                                b4-repo-dir
                              (user-error "No repository picked!")))
         (args (concat "prep --format-patch " outdir))
         (buffer (get-buffer-create "*b4*"))
         (b4--prep-output-name "b4-prep-output"))
    (start-process b4--prep-output-name buffer "b4" args)))

(transient-define-argument b4--prep-enroll ()
  "Enroll existing branch as fork base."
  :argument "enroll_base="
  :shortarg "e"
  :description "From a branch, tag or commit"
  :class 'transient-option
  :choices (vc-git-branches))

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

(defclass b4--view-variable (transient-variable)
  ((scope :initarg :scope)))

(transient-define-argument b4--show-current-revision ()
  "Show the current revision of the patch series."
  :class 'b4--view-variable
  :key ""
  :argument ""
  :description (lambda ()
                 (let ((command "b4 prep --show-revision")
                       (default-directory b4-repo-dir))
                   (if b4-repo-dir
                       (concat "Revision: " (propertize (shell-command-to-string command) 'face 'transient-argument))
                     "None"))))

(transient-define-argument b4--repo-pick-transient ()
  "Pick a repository to run b4 in."
  :class 'b4--view-variable
  :key "R"
  :variable 'b4-repo-dir
  :argument ""
  :description (lambda ()
                 (concat "Repository: " (propertize (pp-to-string b4-repo-dir) 'face 'transient-argument)))
  :prompt "Pick a repository: "
  :reader (lambda (prompt _initial-input _history)
            (let ((path (file-local-name (expand-file-name (read-directory-name prompt)))))
              (if (not (vc-git-root path))
                  (user-error "Not a git repository!"))
              (setq b4-repo-dir path))))

(defun b4--quit-buffer ()
  "Kill/Quit the `*b4*' buffer."
  (kill-buffer "*b4*"))

(defun b4--clear-buffer ()
  "Clear the `*b4*' buffer."
  (interactive)
  (save-excursion
    (let ((buffer "*b4*")
          (inhibit-read-only t))
      (set-buffer buffer)
      (delete-region 1 (+ 1 (buffer-size))))))

(defun b4--buffer-exists-p ()
  "Check if the `*b4*' buffer already exists."
  (not (equal (get-buffer "*b4*") nil)))

(transient-define-suffix b4--show-help ()
  "Show `b4 --help'."
  :key "h"
  :description "Show help"
  (interactive)
  (let ((buffer "*b4*"))
    (call-process "b4" nil buffer nil "--help")
    (set-buffer buffer)
    (b4-mode)
    (switch-to-buffer buffer)))

(defun b4--find-cover-commit ()
  "Find the cover commit with the help of the `b4--magic-marker'.

Return the commit id or raise an error if there is such a
commit cannot be found."
  (let* ((default-directory (if b4-repo-dir
                                b4-repo-dir
                              (user-error "No  repository set!")))
         (command (concat "git log -F --pretty=format:'%H' --max-count=1 --since=1.year --no-abbrev-commit --grep=" b4--magic-marker))
         (commit (shell-command-to-string command)))
    (if (string-empty-p commit)
        (user-error "No cover-letter commit found!"))
    commit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'b4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; b4.el ends here
