;;; ob-json.el --- Org babel for JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/ob-json
;; Version: 0.1.0
;; Keywords: outlines
;; Package-Requires: ((emacs "24.1"))
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
(require 'json)

(add-to-list 'org-babel-tangle-lang-exts '("json" . "json"))


(defvar org-babel-default-header-args:json nil)

(defun org-babel-execute:json (body _params)
  "Return the JSON BODY as is without any modifications.

Argument BODY is the JSON data to be executed.

Optional argument _PARAMS is a placeholder for additional parameters."
  body)


(provide 'ob-json)
;;; ob-json.el ends here