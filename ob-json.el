;;; ob-json.el --- Org babel for JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ob-json
;; Version: 0.1.0
;; Keywords: outlines
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; org-babel functions for json evaluation

;;; Code:




(require 'ob)

(add-to-list 'org-babel-tangle-lang-exts '("json" . "json"))



(defvar json-object-type)
(defvar json-array-type)
(defvar json-null)
(defvar json-false)
(declare-function json-read-from-string "json")

(defvar org-babel-default-header-args:json '((:object-type . "alist")
                                             (:array-type . "vector")
                                             (:null-object . ":null")
                                             (:false-object . ":false"))
  "Default header arguments for JSON code blocks in Org Babel.
:object-type TYPE -- use TYPE to represent JSON objects.
  TYPE can be `alist' (the default), `hash-table', or `plist'.
  If an object has members with the same key, `hash-table'keeps only
  the last value of such keys, while `alist'and `plist'keep all the
  members.
:array-type TYPE -- use TYPE to represent JSON arrays.
  TYPE can be `vector' (the default) or `list'. `array' is the alias of `vector'
:null-object OBJ -- use OBJ to represent a JSON null value.
  It defaults to `:null'.
:false-object OBJ -- use OBJ to represent a JSON false value.
  It defaults to `:false'.")

(defun ob-json--parse-string (str &optional object-type array-type null-object
                                  false-object)
  "Parse STR with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array' `vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-string str
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'array))
                         :null-object (or null-object :null)
                         :false-object (or false-object :false))
    (require 'json)
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read-from-string str))))

(defun ob-json--symbol-name-safe (symb)
  "Return the symbol name if SYMB is a symbol, otherwise return SYMB.

Argument SYMB is the symbol or string to be checked and converted."
  (if (symbolp symb)
      (symbol-name symb)
    symb))

(defun ob-json--flatten-object (obj &optional parent-key)
  "Flatten a nested alist, plist, hash-table, or vector OBJ.
PARENT-KEY is used for constructing keys."
  (let ((result nil)
        (parent-key (or parent-key "")))
    (cond ;; Handle alist
     ((and (listp obj)
           (not (null obj))
           (consp (car obj)))
      (dolist (pair obj)
        (let* ((key (car pair))
               (value (cdr pair))
               (new-key
                (if (string= parent-key "")
                    (ob-json--symbol-name-safe key)
                  (concat parent-key "." (ob-json--symbol-name-safe key))))
               (flattened (ob-json--flatten-object value new-key)))
          (setq result (append result flattened)))))
     ;; Handle plist
     ((and (listp obj)
           (not (null obj)))
      (while obj
        (let* ((sym (pop obj))
               (key (ob-json--symbol-name-safe sym))
               (value (pop obj))
               (new-key (if (string= parent-key "")
                            key
                          (concat parent-key
                                  (unless (keywordp sym) ".")
                                  key)))
               (flattened (ob-json--flatten-object value new-key)))
          (setq result (append result flattened)))))
     ;; Handle hash table
     ((hash-table-p obj)
      (maphash (lambda (key value)
                 (let* ((key (ob-json--symbol-name-safe key))
                        (new-key (if (string= parent-key "")
                                     key
                                   (concat parent-key "." key)))
                        (flattened (ob-json--flatten-object value new-key)))
                   (setq result (append result flattened))))
               obj))
     ;; Handle vector
     ((vectorp obj)
      (cl-loop for i from 0 to (1- (length obj))
               do
               (let* ((value (aref obj i))
                      (new-key (concat parent-key "[" (number-to-string i)
                                       "]"))
                      (flattened (ob-json--flatten-object value new-key)))
                 (setq result (append result flattened)))))
     ;; Handle atomic values
     (t
      (push (cons parent-key obj) result)))
    result))

(defun ob-json--elisp-json-to-flatten-table (data)
  "Convert flattened JSON DATA into a table format.

Argument DATA is the JSON data to be converted into a table."
  (let ((result (ob-json--flatten-object data)))
    (mapcar (lambda (it)
              (print it)
              (cond ((or (atom it)
                         (and (consp it)
                              (listp (cdr it))))
                     it)
                    ((consp it)
                     (list (car it)
                           (cdr it)))))
            (ob-json--stringify result))))

(defun ob-json--stringify (item)
  "Convert various data types to their string representations recursively.

Argument ITEM is the object to be stringified; it can be a string, number,
vector, list, cons cell, symbol, or any other type."
  (pcase item
    ((pred not)
     item)
    ((pred stringp)
     (prin1-to-string item))
    ((pred numberp)
     item)
    ((pred vectorp)
     (apply #'vector (mapcar #'ob-json--stringify (append item nil))))
    ((pred proper-list-p)
     (mapcar #'ob-json--stringify item))
    ((guard (and (consp item)
                 (atom (cdr item))))
     (cons (car item)
           (ob-json--stringify (cdr item))))
    ((guard (and item (symbolp item)))
     item)
    (_ item)))


(defun org-babel-expand-body:json (body params &optional _processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (org-babel-expand-body:generic body params))

(defun ob-json--read-results (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-read
   (if (and (stringp results)
            (string-prefix-p "[" results)
            (string-match-p "\\][\s\t\n]*;?$" results))
       (let ((res (string-trim results)))
         (org-babel-read
          (concat "'"
                  (replace-regexp-in-string
                   "\\[" "(" (replace-regexp-in-string
                              "\\]" ")" (replace-regexp-in-string
                                         ",[[:space:]]" " "
                                         (replace-regexp-in-string
                                          "'" "\"" res)))))))
     results)))
(defun org-babel-execute:json (body params)
  "Return the JSON BODY as is without any modifications.

Argument BODY is the JSON data to be executed.

Optional argument _PARAMS is a placeholder for additional parameters."
  (let* ((expanded-body (org-babel-expand-body:json body params))
         (result-params (cdr (assq :result-params params)))
         (wrap (cdr (assq :wrap params)))
         (tablep (member "table" result-params))
         (result
          (cond ((or tablep
                     (member "code" result-params)
                     (equal "src emacs-lisp" wrap)
                     (equal "src elisp" wrap))
                 (let* ((json-params (mapcar (pcase-lambda (`(,k . ,v))
                                               (cons k
                                                     (if (stringp v)
                                                         (intern
                                                          v)
                                                       v)))
                                             (seq-filter
                                              (pcase-lambda (`(,k . ,_v))
                                                (memq k
                                                      '(:object-type
                                                        :array-type
                                                        :false-object
                                                        :null-object)))
                                              params)))
                        (json-args (mapcar (lambda (key)
                                             (cdr (assq key json-params)))
                                           '(:object-type :array-type
                                             :null-object
                                             :false-object)))
                        (res (apply #'ob-json--parse-string
                                    expanded-body json-args)))
                   (unless (member "discard" result-params)
                     (if
                         (or
                          (member "scalar" result-params)
                          (member "verbatim" result-params)
                          (member "html" result-params)
                          (member "code" result-params)
                          (member "pp" result-params)
                          (member "file" result-params)
                          (and
                           (or
                            wrap
                            (member "output" result-params)
                            (member "raw" result-params)
                            (member "org" result-params)
                            (member "drawer" result-params))
                           (not
                            (member "table" result-params))))
                         (format "'%s" (ob-json--stringify res))
                       (ob-json--elisp-json-to-flatten-table res)))))
                (t
                 (org-babel-result-cond result-params
                   (ob-json--read-results expanded-body)
                   expanded-body)))))
    result))


(provide 'ob-json)
;;; ob-json.el ends here