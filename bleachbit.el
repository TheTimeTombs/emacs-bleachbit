;;; bleachbit.el --- Run Bleachbit -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (ini) (shelly) (tablist))
;; Homepage: homepage
;; Keywords: processes


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; commentary

;;; Code:

(require 'ini)
(require 'shelly)
(require 'tablist)
(require 'transient)

(defgroup bleachbit nil
  "Package to interact with bleachbit.")

(defcustom bleachbit-config-save-on-run nil
  "If non-nil, save the updated options to `bleachbit.ini' after a run.

Customized options stored in the alist")

(defcustom bleachbit-cleaners-dir "/usr/share/bleachbit/cleaners/"
  "The path where bleachbit cleaner XML files are stored.")

(defcustom bleachbit-config-path "~/.config/bleachbit/bleachbit.ini"
  "The path to where the `bleachbit.ini' file is located.")

(defvar bleachbit--config-alist nil
  "An alist storing the parsed `bleacbit.ini' config.")

(defvar bleachbit--options-alist nil
  "An alist of bleachbit options in the form of `(OPTION . STRING)'.

OPTION must be an option to select from the bleachbit cleander definition.
STRING is a string that can be either `True' or `False'. `True' will mean the
option is active when bleachbit is run.")

(defun bleachbit--get-cleaners-list ()
  "Return the list of cleaners from ."
  (cl-remove-if (lambda (item)
                  (or (string-empty-p item)
                      (string-prefix-p "Gtk"item)))
                (split-string (shelly-command-to-string "bleachbit --list") "\n")))

(defun bleachbit--parse-cleaner-file (file-path)
  "Parse cleaner file from FILE-PATH with libxml."
  (if (libxml-available-p )
      (with-temp-buffer
        (insert-file-contents file-path)
        (libxml-parse-xml-region (point-min) (point-max)))
    (error "Emacs must be compliled with libxml2 support to parse bleachbit\
 cleaners.")))

(defun bleachbit--get-cleaner (parsed-file)
  "Restun the alist with key `cleaner' from the alist PARSED-FILE."
  (assoc 'cleaner parsed-file))

(defun bleachbit--get-cleaner-name (cleaner)
  "Get ."
  (or (nth 2 (assoc 'label cleaner))
      ""))

(defun bleachbit--get-cleaner-desc (cleaner)
  ""
  (or (nth 2 (assoc 'description cleaner))
      ""))

(defun bleachbit--options-to-entires (id cleaner-name cleaner-desc cleaner)
  ""
  (cl-loop for item in cleaner
           if (and (listp item)
                   (eql (car item) 'option))
           collect`(,(make-symbol (format "%s.%s" id (bleachbit--get-id item)))
                    [,(format "%s.%s" id (bleachbit--get-id item))
                     ,cleaner-name
                     ,cleaner-desc
                     ,(nth 2 (nth 2 item))
                     ,(nth 2 (nth 3 item))])))

(defun bleachbit--update-options-alist ()
  (setf bleachbit--options-alist
        (cl-loop for option in (tablist-get-marked-items)
                 collect `(,(aref (cdr option) 0) . "True"))))

(defun bleachbit-save-options ()
  "Save the updated options to `bleachbit.ini'."
  (interactive)
  (bleachbit--update-options-alist)
  (thread-first (assoc-delete-all "tree" bleachbit--config-alist)
                (append `(("tree" ,@bleachbit--options-alist)))
                (ini-store bleachbit-config-path nil t)))

(defun bleachbit--get-id (tree)
  "Get the id for the top-level of TREE."
  (cdr (assoc 'id (nth 1 tree))))

(defun bleachbit-options ()
  "Create a tablist of all the bleachbit cleaner options."
  (interactive)
  (with-current-buffer (get-buffer-create "*Bleachbit Options*")
    (switch-to-buffer "*Bleachbit Options*")
    (bleachbit-options-mode)
    (setf tabulated-list-format [("ID" 20 t)
                                 ("Cleaner" 15 t)
                                 ("Descrption" 20 t)
                                 ("Option" 15 t)
                                 ("Option Descrption" 8 t)])
    (tabulated-list-init-header)
    (setf tabulated-list-entries
          (cl-loop for file in (directory-files bleachbit-cleaners-dir t "[^.xml]")
                   do (setf cleaner (bleachbit--get-cleaner (bleachbit--parse-cleaner-file file)))
                   append (bleachbit--options-to-entires (bleachbit--get-id cleaner)
                                                         (bleachbit--get-cleaner-name cleaner)
                                                         (bleachbit--get-cleaner-desc cleaner)
                                                         cleaner)))
    (revert-buffer)
    (bleachbit--mark-options)))

(defun bleachbit--mark-options ()
  "Mark all cleaner options marked as `True' in `bleachbit--options-alist'."
  (bleachbit--parse-option-alist)
  (cl-loop for cons in bleachbit--options-alist
           if (string= (cdr cons) "True")
           do (tablist-mark-items-regexp "ID" (regexp-quote (car cons)))))

(defun bleachbit--parse-option-alist ()
  "Set `bleachbit--options-alist' from `tree' in `bleachbit.ini'."
  (thread-last (ini-decode bleachbit-config-path)
               (setf bleachbit--config-alist)
               (assoc-string "tree")
               (cdr)
               (setf bleachbit--options-alist)))

(defun bleachbit--list-to-string (lst)
  "Return a space-separated string containing all elements of LST."
  (mapconcat #'identity lst " "))

(defun bleachbit--execute (as-root &rest arguments)
  "Run bleachbit in an inferior shell with ARGUMENTS.

If AS-ROOT is non-nil, run the command as a root user."
  (thread-last
    (if (bound-and-true-p transient-current-command)
        (append arguments (transient-args transient-current-command))
      arguments)
    (bleachbit--list-to-string)
    (format "%sbleachbit %s" (if as-root "sudo " ""))
    (shelly-run-command)))

(transient-define-prefix bleachbit ()
  "Transient menu for bleachbit."
  :incompatible '(("--preview" "--clean"))
  ["Bleachbit"
   ("c" "set cleaner preset" bleachbit-options)
   ("c" "clean preset" bleachbit-clean)
   ("C" "clean (as root)" bleachbit-clean-as-root)
   ("p" "preview" bleachbit-preview)
   ("P" "preview (as root)" bleachbit-preview-as-root)
   ("s" "shred files/directories" bleachbit-shred)]
  ["Options"
   ("-a" "all but warning (overrides preset)" "--all-but-warning")
   ("-w" "wipe free space" "--wipe-free-space")
   ("-o" "overwrite files" "--overwrite")
   ])

(defvar bleachbit-options-mode-map
  (let ((map (make-sparse-keymap)))
    (when (fboundp #'bleachbit)
      (define-key map "p" #'bleachbit-preview)
      (define-key map "P" #'bleachbit-preview-as-root)
      (define-key map "c" #'bleachbit-clean)
      (define-key map "C" #'bleachbit-clean-as-root)
      (define-key map "s" #'bleachbit-save-options)
      )
    map)
  "Keymap for `bleachbit-options-mode'.")

(defun bleachbit-shred (filepath)
  "Prompt user for FILEPATH and pass to `bleachbit --shred'."
  (interactive "fFile or directory: ")
  (bleachbit--execute nil "--shred" filepath))

(defun bleachbit-preview ()
  "Run `bleachbit --preview'."
  (interactive)
  (bleachbit--execute nil "--clean" "--preview"))

(defun bleachbit-preview-as-root ()
  "Run `bleachbit --preview' as the root user."
  (interactive)
  (bleachbit--execute t "--clean" "--preview"))

(defun bleachbit-clean ()
  "Run `bleachbit --clean'."
  (interactive)
  (bleachbit--execute nil "--clean" "--preset"))

(defun bleachbit-clean-as-root ()
  "Run `bleachbit --clean' as the root user."
  (interactive)
  (bleachbit--execute t "--clean" "--preset"))

;;;###autoload
(define-derived-mode bleachbit-options-mode tablist-mode "Cleaners"
  "Major mode for interacting with a list of packages from APT."
  :keymap bleachbit-options-mode-map)

(provide 'bleachbit)

;;; bleachbit.el ends here
