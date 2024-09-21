;;; maple-use-package.el --- define evil bind with use-package.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 lin.jiang

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
;;   :keybind
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
;;   :custom
;;   (:mode org-mode
;;          company-tooltip-align-annotations nil)
;;   (:mode markdown-mode
;;          company-tooltip-align-annotations nil))
;;

;;; Code:
(require 'use-package)

(defalias 'use-package-normalize/:hydra 'use-package-normalize-forms)
(defalias 'use-package-normalize/:transient 'use-package-normalize-forms)
(defalias 'use-package-normalize/:keybind 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-leader 'use-package-normalize-forms)
(defalias 'use-package-normalize/:evil-state 'use-package-normalize-forms)
(defalias 'use-package-normalize/:language 'use-package-normalize-forms)
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
            (append head (if (listp keyword) keyword (list keyword)) tail)))))

(defun maple-use-package/keys(args)
  "Cons convert to keybinds with ARGS."
  (mapcan
   (lambda(arg)
     (let ((key (car arg)))
       (list (if (char-or-string-p key) (kbd key) key) `',(cdr arg))))
   args))

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
    (append (cl-remove-if nil (append (nreverse common) (nreverse result)))
            ;; maybe define prop multi times
            (when tail (maple-use-package/plist-get tail prop)))))

(defun maple-use-package/custom-keyword(args)
  "Custom variable with ARGS."
  (pcase (car args)
    (:face `((apply 'custom-set-faces ',(cdr args))))
    (:mode (let ((mode (cadr args)))
             (cl-loop for i in (if (listp mode) mode (list mode)) collect
                      `(progn (unless (featurep 'mode-local) (require 'mode-local))
                              (setq-mode-local ,i ,@(apply 'append (cddr args)))))))
    (:default  `((setq-default ,@(apply 'append (cdr args)))))
    (:variable `((setq ,@(apply 'append (cdr args)))))
    (:function `((progn ,@(cdr args))))
    (:language `((maple-language-define ',(cadr args) ,@(cddr args))))))

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

(defun maple-use-package/hydra(args)
  "Use hydra with ARGS."
  `((unless (featurep ',(car args))
      (autoload ',(car args) "hydra" nil t))
    (with-eval-after-load 'hydra
      (defhydra ,@args))))

(defun maple-use-package/transient(args)
  "Use transient with ARGS."
  `((unless (featurep ',(car args))
      (autoload ',(car args) "transient" nil t))
    (with-eval-after-load 'transient
      (transient-define-prefix ,@args))))

(defun maple-use-package/evil-state(args)
  "Evil bind ARGS."
  (cl-loop for i in args collect `(evil-set-initial-state ',(car i) ',(cdr i))))

(defun maple-use-package/evil-leader(args)
  "Evil bind ARGS."
  (cond ((keywordp (car args))
         (let ((p (maple-use-package/plist-get args :mode)))
           `((evil-leader/set-key-for-mode ',(car p) ,@(maple-use-package/keys (cdr p))))))
        (t `((evil-leader/set-key ,@(maple-use-package/keys (if (consp (car args)) args (list args))))))))

(defun use-package-handler/:keybind (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `(,@(mapcar (lambda(body) `(apply 'maple-define-key '(:package ,name ,@body))) args))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-state (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'evil
       ,@(maple-use-package/evil-state args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:evil-leader (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load 'evil-leader
       ,@(mapcan 'maple-use-package/evil-leader args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:custom (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `((with-eval-after-load ',name
       ,@(mapcan 'maple-use-package/custom args)))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:hydra (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `(,@(mapcan 'maple-use-package/hydra args))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:transient (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `(,@(mapcan 'maple-use-package/transient args))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:language (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `(,@(mapcar (lambda(body) `(maple-language-define ',(car body) ,@(cdr body))) args))
   (use-package-process-keywords name rest state)))

(defun use-package-handler/:dependencies (name _keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   `(,@(mapcar (lambda(body) `(use-package ,@body)) args))
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

(maple-use-package/set-keyword
 '(:hydra
   :transient
   :keybind
   :evil-state
   :evil-leader
   :language
   :dependencies)
 'after :init)
(maple-use-package/set-keyword
 :quelpa
 'after :unless)

(provide 'core-use-package)
;;; core-use-package.el ends here
