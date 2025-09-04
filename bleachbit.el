;;; bleachbit.el --- Run Bleachbit -*- lexical-binding: t -*-

;; Author: Thomas Freeman
;; Maintainer: Thomas Freeman
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (ini) (shelly))
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

(require 'bui)
(require 'dom)
(require 'easymenu)
(require 'ini)
(require 'shelly)
(require 'transient)


;;; Customization Options
(defgroup bleachbit nil
  "Package to interact with bleachbit.")

(defcustom bleachbit-config-save-on-run nil
  "If non-nil, save the updated options to `bleachbit.ini' after a run.

Customized options stored in the alist")

(defcustom bleachbit-cleaners-dir "/usr/share/bleachbit/cleaners/"
  "The path where bleachbit cleaner XML files are stored.")

(defcustom bleachbit-config-path "~/.config/bleachbit/bleachbit.ini"
  "The path to where the `bleachbit.ini' file is located.")


;;; Variables

(defvar bleachbit--cleaners nil
  "A list of all parsed cleaners from bleachbit cleanerML files.")

(defvar bleachbit--config-alist nil
  "An alist storing the parsed `bleacbit.ini' config.")

(defvar bleachbit-current-host "localhost"
  "The current host to run bleachbit commands.")

(defvar bleachbit-options-list--buffer-name "*Bleachbit Options*"
  "Name of the `buffer-options-list-mode' buffer.")


;;; Utility functions

(defun bleachbit--get-cleaners-list ()
  "Return the list of cleaners from `bleachbit --list'."
  (cl-remove-if (lambda (item)
                  (or (string-empty-p item)
                      (string-prefix-p "Gtk"item)))
                (split-string (shelly-command-to-string "bleachbit --list") "\n")))

(defun bleachbit-select-host ()
  (interactive)
  (setf bleachbit-current-host (shelly-select-host)))

(defun bleachbit--list-to-string (lst)
  "Return a space-separated string containing all elements of LST."
  (mapconcat #'identity lst " "))


;;; Shell commands

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

(defun bleachbit-shred (filepath)
  "Prompt user for FILEPATH and pass to `bleachbit --shred'."
  (interactive "fFile or directory: ")
  (bleachbit--execute nil "--shred" filepath))

(defun bleachbit-preview ()
  "Run `bleachbit --preview'."
  (interactive)
  (when (string= (buffer-name (current-buffer)) bleachbit-options-list--buffer-name)
    (bleachbit-save-options))
  (bleachbit--execute nil "--preview" "--preset"))

(defun bleachbit-preview-as-root ()
  "Run `bleachbit --preview' as the root user."
  (interactive)
  (when (string= (buffer-name (current-buffer)) bleachbit-options-list--buffer-name)
    (bleachbit-save-options))
  (bleachbit--execute t "--preview" "--preset"))

(defun bleachbit-clean ()
  "Run `bleachbit --clean'."
  (interactive)
  (when (string= (buffer-name (current-buffer)) bleachbit-options-list--buffer-name)
    (bleachbit-save-options))
  (bleachbit--execute nil "--clean" "--preset"))

(defun bleachbit-clean-as-root ()
  "Run `bleachbit --clean' as the root user."
  (interactive)
  (when (string= (buffer-name (current-buffer)) bleachbit-options-list--buffer-name)
    (bleachbit-save-options))
  (bleachbit--execute t "--clean" "--preset"))


;;; Parsing cleanerML files

(defun bleachbit--parse-cleanerml-file (file-path)
  "Parse cleanerML file from FILE-PATH with libxml."
  (if (libxml-available-p)
      (with-temp-buffer
        (insert-file-contents file-path)
        (xml-remove-comments (point-min) (point-max))
        (libxml-parse-xml-region (point-min) (point-max)))
    (error "Emacs must be compliled with libxml2 support to parse bleachbit\
cleaners")))

(defun bleachbit--parse-cleaners ()
  "Convert the options of CLEANER to a list for `tabulated-list-entries'."
  (cl-loop for file in (directory-files bleachbit-cleaners-dir t "[^.xml]")
           do (setf parsed-file (bleachbit--parse-cleanerml-file file))
           collect `(,(file-name-base file) .
                     ,(dom-by-tag parsed-file 'cleaner))))


;;; BUI Bleachbit options entry type

(defun bleachbit--option->entry (search-term)
  (cl-destructuring-bind
      (cln opt) (split-string search-term "\\.")
    (let* ((cleaner (cdr (assoc cln bleachbit--cleaners))))
      `((id . ,search-term)
        (name .  ,cln)
        (description . ,(nth 2 (car (dom-by-tag cleaner 'description))))
        (option . ,opt)
        (option-description . ,(nth 2 (car (dom-by-tag (dom-by-id cleaner opt) 'description))))))))

(defun bleachbit--get-options (&optional search-type &rest search-values)
  "Generate the list of search terms for bleachbit options.
If SEARCH-TYPE is 'all, then return all bleachbit cleaner options.

If SEARCH-TYPE is 'id, then return the list specified by SEARCH-VALUES."
  (or search-type (setf search-type 'all))
  (cl-case search-type
    (all (bleachbit--get-cleaners-list))
    (id search-values)
    (t (error "Unknown search type: %S" search-type))))

(defun bleachbit--get-entries (&rest args)
  ""
  (mapcar #'bleachbit--option->entry
          (apply #'bleachbit--get-options args)))

(bui-define-entry-type bleachbit-options
  :get-entries-function #'bleachbit--get-entries)


;;; Bleachbit options info interface

(bui-define-interface bleachbit-options info
  :format '((id format (format))
            (name format (format))
            (description format (format))
            (option format (format))
            (option-description format (format))))


;;; Bleachbit options list interface

(defun bleachbit-set-preset ()
  (interactive)
  (setf bleachbit--cleaners (bleachbit--parse-cleaners))
  (bui-get-display-entries 'bleachbit-options 'list)
  (bleachbit--mark-options))

(bui-define-interface bleachbit-options list
  :mode-name "Bleachbit Options"
  :buffer-name bleachbit-options-list--buffer-name
  :get-entries-function #'bleachbit--get-entries
  :describe-function #'bleachbit-options-list--describe
  :format '((name nil 20 t)
            (description nil 20 t)
            (option nil 20 t)
            (option-description nil 20 t))
  :sort-key '(name))

(defun bleachbit-save-options ()
  "Save the updated options to `bleachbit.ini'."
  (interactive)
  (let ((marked-options
         (cl-loop for id in (bui-list-get-marked-id-list)
                  collect `(,id . "True"))))
    (thread-first
      (assoc-delete-all "tree" bleachbit--config-alist)
      (append `(("tree" ,@(reverse marked-options))))
      (ini-store bleachbit-config-path nil t))))

(defun bleachbit--parse-options ()
  "Set `bleachbit--options-alist' from `tree' in `bleachbit.ini'."
  (thread-last (ini-decode bleachbit-config-path)
               (setf bleachbit--config-alist)
               (assoc-string "tree")
               (cdr)))

(defun bleachbit--mark-options ()
  "Mark all cleaner options marked as `True' in `bleachbit--options-alist'."
  (when (get-buffer bleachbit-options-list--buffer-name)
    (let* ((selected-options (bleachbit--parse-options))
           (ids (cl-loop for item in selected-options
                         if (string= (cdr item) "True")
                         collect (car item))))
      (with-current-buffer bleachbit-options-list--buffer-name
        (save-excursion
          (point-min)
          (while (not (eobp))
            (if (member (bui-entry-id (bui-list-current-entry)) ids)
                (bui-list-mark)
              (next-line))))))))

(defun bleachbit-options-list--describe (&rest options)
  "Describe functin for each entry in `bleachbit-options-list-mode'.
OPTIONS is a list of option entry IDs that will be displayed using
`bleachbit-options-info-mode'."
  (bui-get-display-entries 'bleachbit-options 'info (cons 'id options)))

(let ((map bleachbit-options-list-mode-map))
  (define-key map (kbd "c") #'bleachbit-clean)
  (define-key map (kbd "C") #'bleachbit-clean-as-root)
  (define-key map (kbd "p") #'bleachbit-preview)
  (define-key map (kbd "P") #'bleachbit-preview-as-root)
  (define-key map (kbd "s") #'bleachbit-save-options)
  (define-key map (kbd "x") #'bleachbit))

(easy-menu-define bleachbit-options-list-mode-menu bleachbit-options-list-mode-map
  "Menu when `bleachbit-options-list-mode' is active."
  `("Bleachbit Options"
    ["Clean" bleachbit-clean
     :help "Save current options and run Bleachbit clean."]
    ["Clean (as root)" bleachbit-clean-as-root
     :help "Save current options and run Bleachbit clean as root user."]
    ["Preview" bleachbit-preview
     :help "Save current options and run Bleachbit preview."]
    ["Preview (as root)" bleachbit-preview-as-root
     :help "Save current options and run Bleachbit preview as root user."]))


;;; Transient menu

;;;###autoload
(transient-define-prefix bleachbit ()
  "Transient menu for bleachbit."
  :incompatible '(("--preview" "--clean"))
  ["Bleachbit"
   ("sp" "set cleaner preset" bleachbit-set-preset)
   ("c" "clean preset" bleachbit-clean)
   ("C" "clean (as root)" bleachbit-clean-as-root)
   ("p" "preview" bleachbit-preview)
   ("P" "preview (as root)" bleachbit-preview-as-root)
   ("sf" "shred files/directories" bleachbit-shred)]
  ["Options"
   ("-a" "all but warning (overrides preset)" "--all-but-warning")
   ("-w" "wipe free space" "--wipe-free-space")
   ("-o" "overwrite files" "--overwrite")]
  ["Host"
   ("H" "host" bleachbit-select-host
    :transient t
    :description (lambda () (format "Host: %s" bleachbit-current-host)))])

(provide 'bleachbit)

;;; bleachbit.el ends here
