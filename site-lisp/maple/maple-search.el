;;; maple-search.el ---  search engine configuration.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/maple-emacs

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; search engine configuration.
;;

;;; Code:
(defvar maple-search:alist
  '(("Google" . "http://www.google.com/search?q=")
    ("GitHub" . "https://github.com/search?q=")
    ("Google Image" . "https://google.com/images?q=%s")))

(defun maple-search:action (query-url prompt)
  "Open the QUERY-URL.PROMPT set the `read-string prompt."
  (browse-url
   (concat query-url
           (url-hexify-string
            (if (region-active-p)
                (buffer-substring (region-beginning) (region-end))
              (read-string prompt))))))

(defmacro maple-search:define (name url)
  "Define search engine with NAME and URL."
  (let ((engine-name (replace-regexp-in-string " " "-" (downcase name)))
        (engine-prompt (concat name ": ")))
    `(defun ,(intern (format "maple-search:%s" engine-name)) ()
       ,(format "Search %s with a query or region if any." engine-name)
       (interactive)
       (maple-search:action ,url ,engine-prompt))))

;;;###autoload
(defun maple-search-init ()
  "Search macro init."
  (dolist (item maple-search:alist)
    (eval `(maple-search:define ,(car item) ,(cdr item)))))

(provide 'maple-search)
;;; maple-search.el ends here
