;;; maple-keybind.el --- define keybind.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/emacs-maple-modeline

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
;; define keybind.
;;
;; (maple-keybind
;;  :bind
;;  ("im" . maple/capture-screenshot)
;;  ("em" . maple/org-md-export-to-markdown)
;;  (("eh" . maple/org-html-export-to-html)
;;   ("eh" . maple/org-html-export-to-html))
;;  (:map python-mode-map
;;        ("im" . maple/capture-screenshot)
;;        ("em" . maple/org-md-export-to-markdown))
;;  :evil-leader
;;  ("im" . maple/capture-screenshot)
;;  ("em" . maple/org-md-export-to-markdown)
;;  (("eh" . maple/org-html-export-to-html)
;;   ("eh" . maple/org-html-export-to-html))
;;  (:mode python-mode
;;         ("im" . maple/capture-screenshot)
;;         ("em" . maple/org-md-export-to-markdown))
;;  :evil-bind
;;  (:state normal :map python-mode-map
;;          ("em" . maple/org-md-export-to-markdown)
;;          ("eh" . maple/org-html-export-to-html))
;;  (:state (insert normal)
;;          ("em" . maple/org-md-export-to-markdown)
;;          ("eh" . maple/org-html-export-to-html)))

;;; Code:
(defun maple-keybind/keys(args)
  "Cons convert to keybinds with ARGS."
  (mapcan
   (lambda(arg)
     (let ((key (car arg)))
       (list (if (char-or-string-p key) (kbd key) key) `',(cdr arg))))
   args))

(defun maple-keybind/plist-get (plist prop)
  "Get the values associated PLIST to PROP, a modified plist."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun maple-keybind/evil-bind (binds)
  "Evil keybinds with BINDS."
  (let* ((p (maple-keybind/plist-get binds :state))
         (x (maple-keybind/plist-get binds :map))
         (map (or (car x) ''global)))
    `((evil-define-key ',(car p) ,map ,@(maple-keybind/keys (if x (cdr x) (cdr p)))))))

(defun maple-keybind/evil-leader (binds)
  "Evil-leader keybinds with BINDS."
  (cond ((keywordp (car binds))
         (let ((p (maple-keybind/plist-get binds :mode)))
           `((evil-leader/set-key-for-mode ',(car p) ,@(maple-keybind/keys (cdr p))))))
        (t `((evil-leader/set-key ,@(maple-keybind/keys (if (consp (car binds)) binds (list binds))))))))

(defun maple-keybind/bind (binds)
  "Global keybinds with BINDS."
  (cond ((keywordp (car binds))
         (let* ((p (maple-keybind/plist-get binds :map))
                (map (car p))
                (binds (cdr p)))
           `(progn ,@(cl-loop for i in binds collect `(define-key ,map ,(car i) ',(cdr i))))))
        ((consp (car binds))
         (mapcan 'maple-keybind/bind binds))
        (t `((global-set-key ,(car binds) ',(cdr binds))))))

(defmacro maple-keybind(&rest args)
  "Define keybinds with ARGS."
  (declare (indent defun))
  (let ((binds (maple-keybind/plist-get args :bind))
        (evil-binds (maple-keybind/plist-get args :evil-bind))
        (evil-leaders (maple-keybind/plist-get args :evil-leader))
        forms)
    (when binds
      (push `,@(mapcan 'maple-keybind/bind binds) forms))
    (when evil-binds
      (push `(with-eval-after-load 'evil
               ,@(mapcan 'maple-keybind/evil-bind evil-binds))
            forms))
    (when evil-leaders
      (push `(with-eval-after-load 'evil-leader
               ,@(mapcan 'maple-keybind/evil-leader evil-leaders))
            forms))
    `(progn ,@forms)))

(provide 'maple-keybind)
;;; maple-keybind.el ends here
