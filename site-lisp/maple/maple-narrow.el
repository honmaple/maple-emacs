;;; maple-narrow.el --- Initialize file configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2020 lin.jiang

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
;; file configurations.
;;

;;; Code:
(defun maple-narrow:block-in-web()
  "Narrow block in web-mode."
  (interactive)
  (when (and (get-text-property (point) 'block-side)
             (cdr (web-mode-block-is-control (point))))
    (let ((beg (web-mode-block-beginning-position (point)))
          (end (when (web-mode-block-match)
                 (1+ (web-mode-block-end-position (point))))))
      (when (and beg end) (narrow-to-region beg end)))))

(defun maple-narrow:comment-in-web()
  "Narrow comment in web-mode."
  (interactive)
  (when (eq (get-text-property (point) 'tag-type) 'comment)
    (narrow-to-region (web-mode-tag-beginning-position) (web-mode-tag-end-position))))

(defun maple-narrow:html-in-web()
  "Narrow html in web-mode."
  (interactive)
  (when (or (member (get-text-property (point) 'tag-type) '(start end))
            (web-mode-element-parent))
    (let (beg end)
      (when (not (web-mode-element-is-collapsed (point)))
        (web-mode-tag-beginning)
        (when (eq (get-text-property (point) 'tag-type) 'end)
          (web-mode-tag-match))
        (setq beg (point))
        (when (web-mode-tag-match)
          (web-mode-tag-end)
          (setq end (point))))
      (when (and beg end) (narrow-to-region beg end)))))

(defun maple-narrow:script-in-web()
  "Narrow script in web-mode."
  (interactive)
  (let* ((ctx (web-mode-point-context (point)))
         (beg (plist-get ctx :reg-beg)))
    (unless (= beg 1)
      (goto-char beg)))
  )

(defvar-local maple-narrow--indent 0)

(defmacro maple-narrow:without-undo (&rest forms)
  "Excute FORMS."
  `(let* ((buffer-undo-list)
          (modified (buffer-modified-p))
          (inhibit-read-only t))
     (unwind-protect (progn ,@forms)
       (set-buffer-modified-p modified))))

(defun maple-narrow:reindent(start end &optional restore)
  (maple-narrow:without-undo
   (if restore
       (progn (indent-rigidly start end maple-narrow--indent)
              (setq maple-narrow--indent 0))
     (setq maple-narrow--indent (indent-rigidly--current-indentation start end))
     (indent-rigidly start end (- maple-narrow--indent)))))

(defun maple-narrow:to-defun(func &optional include-comments)
  (if (buffer-narrowed-p) (widen)
    (funcall func include-comments)
    (maple-narrow:reindent (point-min) (point-max))))

(defun maple-narrow:to-region(func start end)
  (if (buffer-narrowed-p) (widen)
    (funcall func start end)
    (maple-narrow:reindent start end)))

(defun maple-narrow:widen(&rest _)
  (when (buffer-narrowed-p)
    (maple-narrow:reindent (point-min) (point-max) t)))

;;;###autoload
(define-minor-mode maple-narrow-mode
  "maple narrow mode"
  :group      'maple-narrow
  :global     t
  (if maple-narrow-mode
      (progn
        (advice-add 'narrow-to-region  :around 'maple-narrow:to-region)
        (advice-add 'narrow-to-defun   :around 'maple-narrow:to-defun)
        (advice-add 'narrow-to-page    :around 'maple-narrow:to-defun)
        (advice-add 'widen             :before 'maple-narrow:widen)
        (advice-add 'basic-save-buffer :before 'maple-narrow:widen))
    (advice-remove 'narrow-to-region  'maple-narrow:to-region)
    (advice-remove 'narrow-to-defun   'maple-narrow:to-defun)
    (advice-remove 'narrow-to-page    'maple-narrow:to-defun)
    (advice-remove 'widen             'maple-narrow:widen)
    (advice-remove 'basic-save-buffer 'maple-narrow:widen)))

(provide 'maple-narrow)
;;; maple-narrow.el ends here
