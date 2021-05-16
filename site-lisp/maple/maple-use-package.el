;;; maple-use-package.el --- define evil bind with use-package.	-*- lexical-binding: t -*-

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
;; define evil bind with use-package.
;;
;; (use-package package-name
;;   :evil-bind
;;   (:state normal :map python-mode-map
;;           ("C-c" . run-python))
;;   (:state (normal insert) :map python-mode-map
;;           ("C-s" . run-python))
;;   :evil-leader
;;   ("C-c" . run-python)
;;   (("C-c" . run-python)
;;    ("C-s" . run-python))
;;   (:mode python-mode
;;          ("C-s" . run-python)
;;          ("C-c" . run-python))
;;   :evil-state
;;   (comint-mode . insert)
;;   (sql-interactive-mode . insert)
;;   :evil
;;   (normal :map python-mode-map
;;           ("C-c" . run-python))
;;   (:bind
;;    (:state normal :map python-mode-map
;;            ("C-c" . run-python)))
;;   (:state (comint-mode . insert)
;;           (sql-interactive-mode . insert))
;;   (:leader
;;    ("C-c" . run-python)
;;    (("C-c" . run-python)
;;     ("C-s" . run-python))
;;    (:mode python-mode
;;           ("C-s" . run-python)
;;           ("C-c" . run-python)))
;;   :custom
;;   (:mode org-mode
;;          company-tooltip-align-annotations nil)
;;   (:mode markdown-mode
;;          company-tooltip-align-annotations nil))
;;

;;; Code:
(require 'use-package)
(require 'maple-keybind)
(require 'maple-language)

(defalias 'use-package-normalize/:evil 'use-package-normalize-forms)
(defalias 'use-package-normalize/:hydra 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-bind 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-leader 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-state 'use-package-normalize-forms)
(defalias 'use-package-normalize/:dependencies 'use-package-normalize-forms)

(defun maple-use-package/set-keyword (keyword &optional position refer)
  "Execute KEYWORD forms before or after REFER POSITION."
  (unless (member keyword use-package-keywords)
    (setq use-package-keywords
          (let* ((pos  (cl-position (or refer :init) use-package-keywords))
                 (pos  (cond ((eq position 'before) pos)
                             ((eq position 'after)  (+ pos 1))))
                 (head (cl-subseq use-package-keywords 0 pos))
                 (tail (nthcdr pos use-package-keywords)))
            (append head (list keyword) tail)))))

(defun maple-use-package/plist-get (plist prop)
  "Get the values associated PLIST to PROP, a modified plist."
  (let ((tail plist)
        common
        result)
    (while (and (consp tail) (not (keywordp (car tail))))
      (when (not common) (setq common (list nil)))
      (push (pop tail) common))

    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (append (cl-remove-if nil (list (nreverse common) (nreverse result)))
            ;; maybe define prop multi times
            (when tail (maple-use-package/plist-get tail prop)))))

(defun maple-use-package/hydra(args)
  "Evil bind ARGS."
  `((defhydra ,@args)))

(defun maple-use-package/custom-keyword(args)
  "Custom variable with ARGS."
  (pcase (car args)
    (:face `((custom-set-faces ,@(cdr args))))
    (:function `((progn ,@(cdr args))))
    (:variable `((setq ,@(apply 'append (cdr args)))))
    (:default `((setq-default ,@(apply 'append (cdr args)))))
    (:mode (let ((mode (cadr args)))
             (unless (featurep 'mode-local) (require 'mode-local))
             (cl-loop for i in (if (listp mode) mode (list mode)) collect
                      `(setq-mode-local ,i ,@(cddr args)))))
    (:language `((maple-language ,@(cdr args))))
    (:window (with-eval-after-load 'shackle
               (cl-loop for i in (cdr args) collect `(push ,i shackle-rules))))))

(defun maple-use-package/custom(args)
  "Custom variable with ARGS."
  (if (keywordp (car args))
      (maple-use-package/custom-keyword args)
    (let ((variable (nth 0 args))
          (value (nth 1 args))
          (comment (nth 2 args)))
      (unless (and comment (stringp comment))
        (setq comment (format "Customized %s with use-package" variable)))
      `((customize-set-variable (quote ,variable) ,value ,comment)))))

(defun maple-use-package/evil(args)
  "Set evil bind or state with ARGS."
  (if (keywordp (car args))
      (pcase (car args)
        (:bind (mapcan 'maple-keybind/evil-bind (cdr args)))
        (:state (maple-use-package/evil-state (cdr args)))
        (:leader (mapcan 'maple-keybind/evil-leader (cdr args))))
    (maple-use-package/evil (list :bind `(:state ,@args)))))

(defun maple-use-package/evil-state(args)
  "Evil bind ARGS."
  (cl-loop for i in args collect `(evil-set-initial-state ',(car i) ',(cdr i))))

(defun use-package-handler/:evil (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'evil
       ,@(mapcan 'maple-use-package/evil args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-state (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'evil
       ,@(maple-use-package/evil-state args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-bind (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'evil
       ,@(mapcan 'maple-keybind/evil-bind args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-leader (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'evil-leader
       ,@(mapcan 'maple-keybind/evil-leader args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:custom (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   (mapcan 'maple-use-package/custom args)
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:hydra (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'hydra
       ,@(mapcan 'maple-use-package/hydra args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:dependencies (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `(,@(mapcan (lambda(body) `((use-package ,@body))) args))
   (use-package-process-keywords name rest state)))

(defun use-package-normalize/:quelpa (name keyword args)
  "NAME KEYWORD ARGS."
  (use-package-only-one (symbol-name keyword) args
    (lambda (_label arg)
      (pcase arg
        ((or `nil `t)   (list name))
        ((pred symbolp) (list arg))
        ((pred listp)
         (cond
          ((listp (car arg)) arg)
          ((keywordp (car arg)) (list (append (list name) arg)))
          ((symbolp (car arg)) (list arg))))
        (_ nil)))))

(defun use-package-handler/:quelpa (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((unless (or (locate-library ,(format "%s" name))
                 (package-installed-p ',(pcase (car args)
                                          ((pred listp)   (caar args))
                                          ((pred symbolp) (car args)))))
       (apply 'quelpa ',args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:quelpa-ensure (func name keyword ensure rest state)
  "FUNC NAME KEYWORD ENSURE REST STATE."
  (let ((ensure (if (plist-member rest :quelpa) nil ensure)))
    (funcall func name keyword ensure rest state)))

(advice-add 'use-package-handler/:ensure :around 'use-package-handler/:quelpa-ensure)

(maple-use-package/set-keyword :evil         'after :init)
(maple-use-package/set-keyword :evil-bind    'after :init)
(maple-use-package/set-keyword :evil-leader  'after :init)
(maple-use-package/set-keyword :evil-state   'after :init)
(maple-use-package/set-keyword :hydra        'after :init)
(maple-use-package/set-keyword :dependencies 'after :init)
(maple-use-package/set-keyword :quelpa       'after :unless)

(provide 'maple-use-package)
;;; maple-use-package.el ends here
