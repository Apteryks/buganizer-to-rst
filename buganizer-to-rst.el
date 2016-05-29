;;; buganizer-to-rst.el --- Generate ReST changelog from Buganizer data

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: "Maxim Cournoyer" <maxim.cournoyer@gmail.com>
;; Version: 0.1
;; Keywords: convenience

;; buganizer-to-rst is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; buganizer-to-rst is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with buganizer-to-rst.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a method which can be used to generate a ReST
;; formatted changelog given a csv file exported from Buganizer. The main
;; function is `buganizer-to-rst', which requests a CSV file interactively and
;; inserts the generated output at the current pointer location. The current
;; library relies on `rst-mode' (builtin as of Emacs 24.4) and `csv.el' (must
;; be installed manually, distributed along `buganizer-to-rst.el'.

;;; Example:

;; Given a Buganizer exported CSV file (~/0.1.csv) which contains the following:

;; "PRIORITY","TARGETED_TO_VERSION","TYPE","TITLE","ASSIGNEE","STATUS","ISSUE_ID","MODIFIED_TIME (UTC)"
;; "P2","0.1","INTERNAL_CLEANUP","Some cleanup task","Greg","INTENDED_BEHAVIOR","28729891","2016-05-14 18:09:25"
;; "P2","0.1","PROCESS","Update document X","Paul","FIXED","28729401","2016-05-17 12:56:12"
;; "P2","0.1","FEATURE_REQUEST","Add support for Y","Amy","FIXED","28602095","2016-05-20 14:46:48"
;; "P2","0.1","BUG","Fix crash Z","Paul","FIXED","28430975","2016-05-03 00:45:18"
;; "P3","0.1","INTERNAL_CLEANUP","Refactor W","Amy","FIXED","27882497","2016-05-28 10:16:53"

;; Calling buganizer-to-rst interactively (M-x buganizer-to-rst) on this file
;; would generate the following itemized list:

;; * Update document X. (Issue #28729401)
;; * Add support for Y. (Issue #28602095)
;; * Bugfix: Fix crash Z. (Issue #28430975)
;; * Cleanup: Refactor W. (Issue #27882497)

;; Long lines are wrapped according to your `fill-column' value, and entries
;; which do not have their status set to "FIXED" are skipped. Currently, it is
;; left to the user to filter the items for a particular release in Buganizer
;; prior to exporting the CSV file.

(require 'rst)
(require 'csv)  ; Depends on Ulf Jasper's csv.el module.

(defun parse-csv-file (file)
  "Parse a csv file using the `csv-parse-buffer' method of `csv.el'."
  (interactive (list (read-file-name "CSV file: ")))
  (with-temp-buffer
    (insert-file-contents file)
    (csv-parse-buffer t (current-buffer))))

(defun validate-data (parsed-csv-data)
  "Verify that the CSV parsed data contains the required columns."
  (let ((required-columns '("TYPE" "TITLE" "STATUS" "ISSUE_ID"))
        (first-row (car parsed-csv-data)))
    (dolist (column-header required-columns)
      (if (not (assoc-default column-header first-row))
          (error "%s" (concat "Column missing in csv file: "
                              column-header))
        ))))

(defun buganizer-to-rst (file)
  "Generate an ReST formatted changelog from a Buganizer CSV exported file."
  (interactive (list (read-file-name "CSV file: ")))
  (let ((parsed-csv-data (parse-csv-file file))
        prefix issue-type issue-title issue-id issue-status
        generated-content)
    (validate-data parsed-csv-data)
    ;; Format entries and build a string with them.
    (with-temp-buffer
      (rst-mode) ; set buffer major mode
      (dolist (row parsed-csv-data)
        ;; Retrieve relevant issue information.
        (setq issue-status (assoc-default "STATUS" row))
        (setq issue-type (assoc-default "TYPE" row))
        (setq issue-title (assoc-default "TITLE" row))
        (setq issue-id (assoc-default "ISSUE_ID" row))
        (if (equal issue-status "FIXED")
            (progn
              (cond  ; Programmatically define a prefix for the entry.
               ((string-equal "BUG" issue-type)
                (setq prefix "Bugfix: "))
               ((string-equal "INTERNAL_CLEANUP" issue-type)
                (setq prefix "Cleanup: "))
               (t
                (setq prefix nil)))  ; Default prefix value.
              ;; Form and insert item string.
              (insert
               (concat prefix issue-title ". (Issue #" issue-id ")\n")))
          (warn (concat "Skipping issue #" issue-id
                        " because its status is not \"FIXED\""))))
      ;; Bulletized newly inserted paragraphs.
      ;; The third argument is the command prefix argument which must be set to
      ;; non-nil since we want to process each line.
      (rst-bullet-list-region (point-min) (point-max) t)
      (fill-region (point-min) (point-max))
      (setq generated-content (buffer-string)))
    (insert generated-content)))
