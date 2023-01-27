;;; core/autoload/keybind.el ---  keybind function.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2022 lin.jiang

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
;; keybind functions.
;;
;; (maple-define-key
;;   :states 'normal
;;   :prefix ","
;;   :keymaps 'python-mode-map
;;   "t"  '(:ignore t :desc "test")
;;   "t1" 'test
;;   (kbd "t1") '(test :package package-name)
;;   [f5] '(test :states '(insert visual) :if (featurep 'evil))
;;   [remap keyboard-quit] 'test
;;   :--
;;   :keymaps 'go-mode-map
;;   "b1" 'test1)

;;; Code:
(declare-function evil-define-key 'evil)

(defvar maple-keybind-prefix nil
  "Default prefix for keybind.")

(defvar maple-keybind-preset-alist nil
  "Preset alist.")

(defun maple-keybind-normalize(key &optional prefix)
  "Concat PREFIX and KEY."
  (when (and (not prefix) maple-keybind-prefix)
    (setq prefix maple-keybind-prefix))
  (when (and prefix (stringp prefix))
    (setq prefix (string-to-vector (kbd prefix))))
  (when (stringp key)
    (setq key (string-to-vector (kbd key))))
  (if (or (not prefix) (and (vectorp key) (memq (aref key 0) '(t remap)))) key
    (vconcat prefix key)))

;;;###autoload
(defun maple-keybind-preset(preset &rest args)
  "Define PRESET as ARGS.

If define :leader as
    (maple-keybind-preset :leader
     :prefix \",\"
     :keymaps 'global-map
     :states 'normal)
Then
    (maple-keybind
     :leader
     KEY DEF)
is equivalent to
    (maple-keybind
     :prefix \",\"
     :keymaps 'global-map
     :states 'normal
     KEY DEF)"
  (setf (alist-get preset maple-keybind-preset-alist) args))

(defun maple-keybind-def(keymap &rest keys)
  "Define keybind with KEYMAP &rest KEYS."
  (let (forms evil-forms which-key-forms)
    (dolist (keycons keys)
      (let ((key (car-safe keycons))
            (args (cdr-safe keycons))
            def condition)
        (unless (keywordp (car-safe args))
          (setq def (pop args)))
        (when (stringp key) (setq key (string-to-vector key)))
        (when (stringp def) (setq def (string-to-vector def)))
        (cl-destructuring-bind (&key if ignore states package desc) args
          (when (and package (symbolp def))
            (push `(unless (fboundp ',def)
                     (autoload #',def ,(if (stringp package) package (symbol-name package))))
                  forms))
          (when desc
            (push `(which-key-add-key-based-replacements
                     (key-description ,key) ,desc)
                  which-key-forms))
          (unless ignore
            (setq condition `(and (or (not ,if)
                                      (and (booleanp ,if) ,if)
                                      (and (not (booleanp ,if)) ,if))))
            (if states
                (push `(when ,condition (evil-define-key ',states ,keymap ,key ',def)) evil-forms)
              (push `(when ,condition (define-key ,keymap ,key ',def)) forms))))))

    (let ((func `(progn (with-eval-after-load 'which-key ,@which-key-forms)
                        (with-eval-after-load 'evil ,@evil-forms)
                        ,@forms)))
      (if (boundp keymap) (eval func)
        (let ((run-once (cl-gensym "maple-keybind-run-once")))
          (fset run-once `(lambda(&rest _)
                            (when (boundp ',keymap)
                              (remove-hook 'after-load-functions #',run-once)
                              (makunbound ',run-once)
                              ,func)))
          (put run-once 'permanent-local-hook t)
          (add-hook 'after-load-functions run-once t))))))

;;;###autoload
(defun maple-keybind(&rest args)
  "Define keybinds with ARGS."
  (declare (indent defun))
  (let ((first (pop args))
        keywords keymaps prefix key def keys)
    (while first
      (setq key nil def nil)
      (pcase first
        (:prefix
         (setq prefix (pop args)))
        ((or :map :keymaps)
         (setq keymaps (pop args)))
        ((or :if :states :package)
         (let ((arg (pop args)))
           (when (memq (car-safe arg) '(quote))
             (setq arg (cdr-safe arg)))
           (setf (plist-get keywords first) arg)))
        ((or :reset :--)
         (setq prefix nil
               keymaps nil
               keywords nil))
        ((pred keywordp)
         (let ((preset (alist-get first maple-keybind-preset-alist)))
           (when preset (setq args (append preset args)))))
        (_
         (if (consp first)
             (setq key (car-safe first)
                   def (cdr-safe first))
           (setq key first
                 def (pop args)))

         ;; check list or lambda function
         (when (or (not (listp def)) (functionp def))
           (setq def (list def)))

         (push (cons (maple-keybind-normalize key prefix) (append def keywords)) keys)))

      (setq first (pop args))

      (when (or (not first) (and (keywordp first) keys))
        ;; normalize keymaps
        (when (memq (car-safe keymaps) '(quote))
          (setq keymaps (cdr-safe keymaps)))
        (if (not keymaps)
            (setq keymaps '(global-map))
          (unless (listp keymaps)
            (setq keymaps (list keymaps))))
        (dolist (keymap keymaps)
          (apply 'maple-keybind-def (append (list keymap) keys)))
        (setq keys nil)))))

;;;###autoload
(defalias 'maple-define-key #'maple-keybind)

;;;###autoload
(defalias 'maple-define-key-preset #'maple-keybind-preset)

;;; keybind.el ends here
