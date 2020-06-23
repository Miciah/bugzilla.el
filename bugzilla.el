;;; bugzilla.el --- Query Bugzilla -*- lexical-binding: t -*-

;; Author: Miciah Dashiel Butler Masters
;; Maintainer: Miciah Dashiel Butler Masters
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (transient "20200601"))
;; Homepage: https://github.com/Miciah/bugzilla.el
;; Keywords: comm tools


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This package provides a UI for querying Bugzilla, based on transient and
;; tabulated-list-mode and using the python-bugzilla command-line tool.  Make
;; sure python-bugzilla is installed and logged in, and then use `M-x
;; bugzilla-list-transient' to get started.


;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'transient)


(defgroup bugzilla ()
  "UI for querying Bugzilla."
  :group 'bugzilla)

(defface bugzilla-id '((t (:foreground "DarkGrey") (:italic t)))
  "Face for Bugzilla ids.")
(defface bugzilla-product '((t ()))
  "Face for product name.")
(defface bugzilla-component '((t ()))
  "Face for component name.")
(defface bugzilla-assignee '((t (:italic t)))
  "Face for assignee.")
(defface bugzilla-blocks '((t (:foreground "yellow")))
  "Face for the `blocks' field.")
(defface bugzilla-depends '((t (:foreground "yellow")))
  "Face for the `depends_on' field.")
(defface bugzilla-creation-time '((t (:foreground "DarkGrey") (:italic t)))
  "Face for report creation time.")
(defface bugzilla-reporter '((t (:italic t)))
  "Face for reporter.")
(defface bugzilla-keywords '((t (:italic t)))
  "Face for keywords.")
(defface bugzilla-status-new '((t (:foreground "yellow")))
  "Face for `NEW' status.")
(defface bugzilla-status-assigned '((t (:foreground "green")))
  "Face for `ASSIGNED' status.")
(defface bugzilla-status-modified '((t (:foreground "blue")))
  "Face for `MODIFIED' status.")
(defface bugzilla-status-post '((t (:foreground "pink")))
  "Face for `POST' status.")
(defface bugzilla-status-on_qa '((t (:foreground "pink")))
  "Face for `ON_QA' status.")
(defface bugzilla-status-verified '((t (:foreground "orange")))
  "Face for `VERIFIED' status.")
(defface bugzilla-status-closed '((t (:foreground "DarkGrey")))
  "Face for `CLOSED' status.")
(defface bugzilla-resolution '((t ()))
  "Face for resolution.")
(defface bugzilla-priority-low '((t (:foreground "DarkGrey")))
  "Face for low priority.")
(defface bugzilla-priority-medium '((t ()))
  "Face for medium priority.")
(defface bugzilla-priority-high '((t (:foreground "yellow")))
  "Face for high priority.")
(defface bugzilla-priority-urgent '((t (:foreground "red") (:bold t)))
  "Face for urgent priority.")
(defface bugzilla-priority-unspecified '((t (:italic t)))
  "Face for unspecified priority.")
(defface bugzilla-severity-low '((t (:foreground "DarkGrey")))
  "Face for low severity.")
(defface bugzilla-severity-medium '((t ()))
  "Face for medium severity.")
(defface bugzilla-severity-high '((t (:foreground "yellow")))
  "Face for high severity.")
(defface bugzilla-severity-urgent '((t (:foreground "red") (:bold t)))
  "Face for urgent severity.")
(defface bugzilla-severity-unspecified '((t (:italic t)))
  "Face for unspecified severity.")
(defface bugzilla-summary '((t (:italic t)))
  "Face for report summary.")
(defface bugzilla-target-release '((t ()))
  "Face for target release.")
(defface bugzilla-version '((t ()))
  "Face for reported version.")

(defcustom bugzilla-host "bugzilla.redhat.com"
  "Host name of the Bugzilla instance.")

(defcustom bugzilla-list-mode-columns
  '(("ID"        7 id)
    ("Reporter" 10 creator)
    ("Priority" 11 priority)
    ("Version"   6 version)
    ("Assignee" 10 assigned_to)
    ("Severity" 11 severity)
    ("Target"    6 target_release)
    ("Status"    8 status)
    ("Summary"  50 summary)
    ("Keywords" 20 keywords))
  "List of columns displayed by `bugzilla-list-mode'."
  :type '(repeat (list :tag "Column"
                       (string  :tag "Header Label")
                       (integer :tag "Column Width")
                       (const   :tag "Field"
                                (choice
                                 (const :tag "Bugzilla number" id)
                                 (const :tag "Product" product)
                                 (const :tag "Component" component)
                                 (const :tag "Assignee" assigned_to)
                                 (const :tag "Blocks" blocks)
                                 (const :tag "Depends" depends_on)
                                 (const :tag "Creation time" creation_time)
                                 (const :tag "Reporter" creator)
                                 (const :tag "Keywords" keywords)
                                 (const :tag "Priority" priority)
                                 (const :tag "Severity" severity)
                                 (const :tag "Status" status)
                                 (const :tag "Resolution" resolution)
                                 (const :tag "Summary" summary)
                                 (const :tag "Version" version)
                                 (const :tag "Target release" target_release)))
                       (repeat  :tag "Properties"
                                (list (choice :tag "Property"
                                              (const :right-align)
                                              (const :pad-right)
                                              (symbol))
                                      (sexp   :tag "Value"))))))


(defun bugzilla-report-mode--get-report (id)
  "Query Bugzilla for Bugzilla report ID."
  (shell-command-to-string (concat "bugzilla query --full --id=" id)))

(defun bugzilla-report-show (id)
  "Show Bugzilla report ID."
  (interactive "sBugzilla id: ")
  (switch-to-buffer (format "*Bugzilla #%s*" id))
  (goto-char (point-min))
  (delete-region (point) (point-max))
  (insert (bugzilla-report-mode--get-report id)))


(defun bugzilla-list-mode--propertize (field value)
  "Propertize VALUE appropriately for FIELD."
  (cl-case field
    (id (propertize value 'face 'bugzilla-id))
    (product (propertize value 'face 'bugzilla-product))
    (component (propertize value 'face 'bugzilla-component))
    (assigned_to (propertize value 'face 'bugzilla-assignee))
    (blocks (propertize value 'face 'bugzilla-blocks))
    (depends_on (propertize value 'face 'bugzilla-depends))
    (creation_time (propertize value 'face 'bugzilla-creation-time))
    (creator (propertize value 'face 'bugzilla-reporter))
    (keywords (propertize value 'face 'bugzilla-keywords))
    (priority
     (pcase value
       ("low"    (propertize value 'face 'bugzilla-priority-low))
       ("medium" (propertize value 'face 'bugzilla-priority-medium))
       ("high"   (propertize value 'face 'bugzilla-priority-high))
       ("urgent" (propertize value 'face 'bugzilla-priority-urgent))
       (_ value)))
    (severity
     (pcase value
       ("low"    (propertize value 'face 'bugzilla-severity-low))
       ("medium" (propertize value 'face 'bugzilla-severity-medium))
       ("high"   (propertize value 'face 'bugzilla-severity-high))
       ("urgent" (propertize value 'face 'bugzilla-severity-urgent))
       (_ value)))
    (resolution (propertize value 'face 'bugzilla-resolution))
    (status
     (pcase value
       ("NEW"      (propertize value 'face 'bugzilla-status-new))
       ("ASSIGNED" (propertize value 'face 'bugzilla-status-assigned))
       ("POST"     (propertize value 'face 'bugzilla-status-post))
       ("MODIFIED" (propertize value 'face 'bugzilla-status-modified))
       ("ON_QA"    (propertize value 'face 'bugzilla-status-on_qa))
       ("VERIFIED" (propertize value 'face 'bugzilla-status-verified))
       ("CLOSED"   (propertize value 'face 'bugzilla-status-closed))
       (_ value)))
    (summary (propertize value 'face 'bugzilla-summary))
    (version (propertize value 'face 'bugzilla-version))
    (target_release (propertize value 'face 'bugzilla-target-release))
    (otherwise value)))

(defun bugzilla-list-mode--propertize-entry (entry)
  "Propertize the fields of ENTRY."
  (mapcar* #'bugzilla-list-mode--propertize
           (mapcar #'caddr bugzilla-list-mode-columns)
           entry))

(defvar bugzilla-list-query--args nil "Arguments to `bugzilla query'.")

(defun bugzilla-list-mode--get-entries ()
  "Query Bugzilla and return entries for `tabulated-list-entries'."
  (when (null bugzilla-list-query--args)
    (error "Bugzilla query arguments are not set"))
  ;; `bugzilla-list-query--args' is buffer-local, so we need to store it to a
  ;; local variable before switching to a temporary buffer.
  (let ((args bugzilla-list-query--args))
    (with-temp-buffer
      (apply #'call-process "bugzilla" nil t nil "query"
             (concat "--outputformat="
                     (mapconcat (lambda (column)
                                  (format "%%{%s}" (caddr column)))
                                (cons '(nil nil id) bugzilla-list-mode-columns)
                                "::"))
             args)
      (let ((entries))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (point) (line-end-position))))
            (unless (string-empty-p line)
              (push (cl-destructuring-bind (id &rest rest)
                        (split-string line "::")
                      `(,id [,@(bugzilla-list-mode--propertize-entry rest)]))
                    entries)))
          (forward-line 1))
        entries))))

(transient-define-argument bugzilla-list-transient:--id ()
  :description "ID"
  :class 'transient-option
  :shortarg "-b"
  :argument "--id=")
(transient-define-argument bugzilla-list-transient:--product ()
  :description "Product"
  :class 'transient-option
  :shortarg "-p"
  :argument "--product=")
(transient-define-argument bugzilla-list-transient:--version ()
  :description "Version"
  :class 'transient-option
  :shortarg "-v"
  :argument "--version=")
(transient-define-argument bugzilla-list-transient:--component ()
  :description "Component"
  :class 'transient-option
  :shortarg "-c"
  :argument "--component=")
(transient-define-argument bugzilla-list-transient:--comment ()
  :description "Comment"
  :class 'transient-option
  :shortarg "-l"
  :argument "--comment=")
(transient-define-argument bugzilla-list-transient:--sub-component ()
  :description "Subcomponent"
  :class 'transient-option
  :shortarg "=c"
  :argument "--sub-component=")
(transient-define-argument bugzilla-list-transient:--summary ()
  :description "Summary"
  :class 'transient-option
  :shortarg "-t"
  :argument "--summary=")
(transient-define-argument bugzilla-list-transient:--os ()
  :description "Operating system"
  :level 5
  :class 'transient-option
  :shortarg "-o"
  :argument "--os=")
(transient-define-argument bugzilla-list-transient:--arch ()
  :description "Architecture"
  :level 5
  :class 'transient-option
  :shortarg "=a"
  :argument "--arch=")
(transient-define-argument bugzilla-list-transient:--severity ()
  :description "Severity"
  :class 'transient-option
  :shortarg "-x"
  :argument "--severity=")
(transient-define-argument bugzilla-list-transient:--priority ()
  :description "Priority"
  :class 'transient-option
  :shortarg "-z"
  :argument "--priority=")
(transient-define-argument bugzilla-list-transient:--status ()
  :description "Status"
  :class 'transient-option
  :shortarg "=s"
  :argument "--status=")
(transient-define-argument bugzilla-list-transient:--url ()
  :description "URL field"
  :level 5
  :class 'transient-option
  :shortarg "-u"
  :argument "--url=")
(transient-define-argument bugzilla-list-transient:--target_milestone ()
  :description "Target milestone"
  :level 5
  :class 'transient-option
  :shortarg "-m"
  :argument "--target_milestone=")
(transient-define-argument bugzilla-list-transient:--target_release ()
  :description "Target release"
  :class 'transient-option
  :shortarg "=t"
  :argument "--target_release=")
(transient-define-argument bugzilla-list-transient:--blocked ()
  :description "Blocked"
  :class 'transient-option
  :shortarg "=b"
  :argument "--blocked=")
(transient-define-argument bugzilla-list-transient:--dependson ()
  :description "Depends on"
  :class 'transient-option
  :shortarg "=d"
  :argument "--dependson=")
(transient-define-argument bugzilla-list-transient:--keywords ()
  :description "Keywords"
  :class 'transient-option
  :shortarg "=k"
  :argument "--keywords=")
(transient-define-argument bugzilla-list-transient:--keywords_type ()
  :description "Keywords type"
  :class 'transient-option
  :shortarg "=K"
  :argument "--keywords_type="
  :choices '(allwords anywords nowords regexp notregexp))
(transient-define-argument bugzilla-list-transient:--groups ()
  :description "Groups"
  :level 5
  :class 'transient-option
  :shortarg "=g"
  :argument "--groups=")
(transient-define-argument bugzilla-list-transient:--cc ()
  :description "Cc"
  :class 'transient-option
  :shortarg "=C"
  :argument "--cc=")
(transient-define-argument bugzilla-list-transient:--assigned_to ()
  :description "Assignee"
  :class 'transient-option
  :shortarg "-a"
  :argument "--assigned_to=")
(transient-define-argument bugzilla-list-transient:--reporter ()
  :description "Reporter"
  :class 'transient-option
  :shortarg "-r"
  :argument "--reporter=")
(transient-define-argument bugzilla-list-transient:--qa_contact ()
  :description "QA contact"
  :level 7
  :class 'transient-option
  :shortarg "-q"
  :argument "--qa_contact=")
(transient-define-argument bugzilla-list-transient:--flag ()
  :description "Flag (e.g., `needinfo?')"
  :level 7
  :class 'transient-option
  :shortarg "-f"
  :argument "--flag=")
(transient-define-argument bugzilla-list-transient:--tags ()
  :description "Tags"
  :level 7
  :class 'transient-option
  :shortarg "=T"
  :argument "--tags=")
(transient-define-argument bugzilla-list-transient:--whiteboard ()
  :description "Whiteboard"
  :level 6
  :class 'transient-option
  :shortarg "-w"
  :argument "--whiteboard=")
(transient-define-argument bugzilla-list-transient:--devel_whiteboard ()
  :description "Devel whiteboard"
  :level 6
  :class 'transient-option
  :shortarg "=w"
  :argument "--devel_whiteboard=")
(transient-define-argument bugzilla-list-transient:--internal_whiteboard ()
  :description "Internal whiteboard"
  :level 6
  :class 'transient-option
  :shortarg "=W"
  :argument "--internal_whiteboard=")
(transient-define-argument bugzilla-list-transient:--qa_whiteboard ()
  :description "QA whiteboard"
  :level 6
  :class 'transient-option
  :shortarg "=q"
  :argument "--qa_whiteboard=")
(transient-define-argument bugzilla-list-transient:--fixed_in ()
  :description "Fixed in"
  :class 'transient-option
  :shortarg "-F"
  :argument "--fixed_in=")
(transient-define-argument bugzilla-list-transient:--field ()
  :description "Custom field"
  :level 7
  :class 'transient-option
  :shortarg "=f"
  :argument "--field=")
(transient-define-argument bugzilla-list-transient:--savedsearch ()
  :description "Saved search"
  :level 7
  :class 'transient-option
  :shortarg "=v"
  :argument "--savedsearch=")
(transient-define-argument bugzilla-list-transient:--savedsearch-sharer-id ()
  :description "Saved search sharer id"
  :level 7
  :class 'transient-option
  :shortarg "=V"
  :argument "--savedsearch-sharer-id=")
(transient-define-argument bugzilla-list-transient:--from-url ()
  :description "From URL"
  :level 7
  :class 'transient-option
  :shortarg "=u"
  :argument "--from-url=")

(transient-define-prefix bugzilla-list-transient ()
  "Query and list Bugzilla reports.

Query Bugzilla using the `bugzilla query' shell command, and list
the results using `bugzilla-list-mode'."
  ["Arguments"
   (bugzilla-list-transient:--id)
   (bugzilla-list-transient:--product)
   (bugzilla-list-transient:--version)
   (bugzilla-list-transient:--component)
   (bugzilla-list-transient:--comment)
   (bugzilla-list-transient:--sub-component)
   (bugzilla-list-transient:--summary)
   (bugzilla-list-transient:--os)
   (bugzilla-list-transient:--arch)
   (bugzilla-list-transient:--severity)
   (bugzilla-list-transient:--priority)
   (bugzilla-list-transient:--status)
   (bugzilla-list-transient:--url)
   (bugzilla-list-transient:--target_milestone)
   (bugzilla-list-transient:--target_release)
   (bugzilla-list-transient:--blocked)
   (bugzilla-list-transient:--dependson)
   (bugzilla-list-transient:--keywords)
   (bugzilla-list-transient:--keywords_type)
   (bugzilla-list-transient:--groups)
   (bugzilla-list-transient:--cc)
   (bugzilla-list-transient:--assigned_to)
   (bugzilla-list-transient:--reporter)
   (bugzilla-list-transient:--qa_contact)
   (bugzilla-list-transient:--flag)
   (bugzilla-list-transient:--tags)
   (bugzilla-list-transient:--whiteboard)
   (bugzilla-list-transient:--devel_whiteboard)
   (bugzilla-list-transient:--internal_whiteboard)
   (bugzilla-list-transient:--qa_whiteboard)
   (bugzilla-list-transient:--arch)
   (bugzilla-list-transient:--field)
   (bugzilla-list-transient:--savedsearch)
   (bugzilla-list-transient:--savedsearch-sharer-id)
   (bugzilla-list-transient:--from-url)]
  ["Actions"
   ("l" "Query Bugzilla and list results" bugzilla-list-query)])

(defun bugzilla-list-query (&optional args)
  "Execute `bugzilla query' with ARGS and show the results.

Query Bugzilla by executing `bugzilla query' with the provided
query ARGS, and show the results using `bugzilla-list-mode'."
  (interactive (list (transient-args 'bugzilla-list-transient)))
  (with-current-buffer (get-buffer-create "*Bugzilla*")
    (bugzilla-list-mode)
    (setq-local bugzilla-list-query--args args)
    (bugzilla-list-mode-refresh)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(defun bugzilla-list-mode-show-entry ()
  "Show the current entry in `bugzilla-list-mode'."
  (interactive)
  (bugzilla-report-show (tabulated-list-get-id)))

(defun bugzilla-list-mode-browse-entry ()
  "Browse to the current entry in `bugzilla-list-mode'."
  (interactive)
  (browse-url (format "https://%s/show_bug.cgi?id=%s"
                      bugzilla-host (tabulated-list-get-id))))

(defvar bugzilla-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'bugzilla-list-mode-show-entry)
    (define-key map (kbd "RET") #'bugzilla-list-mode-browse-entry)
    map))

(defun bugzilla-list-mode-refresh ()
  "Refresh the current `bugzilla-list-mode' buffer."
  (setq tabulated-list-entries (bugzilla-list-mode--get-entries)))

(define-derived-mode bugzilla-list-mode tabulated-list-mode "Bugzilla"
  "Major mode for listing Bugzilla reports.

See `bugzilla-list-transient'."
  (setq tabulated-list-format `[,@(mapcar (lambda (column)
                                            `(,(car column) ,(cadr column) t))
                                          bugzilla-list-mode-columns)])
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook 'bugzilla-list-mode-refresh nil t))

(defalias 'bugzilla-list #'bugzilla-list-transient
  "Query and list Bugzilla reports.

Alias for `bugzilla-list-transient'.")


(provide 'bugzilla)

;;; bugzilla.el ends here
