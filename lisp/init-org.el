;;; init-org.el --- Org Mode configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2025 lin.jiang

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
;; mine org-mode configurations.
;;

;;; Code:
(use-package ox-confluence
  :ensure org-contrib
  :commands (org-confluence-export-as-confluence))

(use-package org-faces
  :ensure nil
  :config
  (when (and (display-graphic-p)
             (find-font (font-spec :name "Inconsolata")))
    (set-face-attribute 'org-table nil :font "Inconsolata 12")))

(use-package org
  :ensure nil
  :init
  ;; markup记号前后允许中文, 必须在(require 'org)前
  (setq org-emphasis-regexp-components
        (list (concat "- \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["   "[:nonascii:]")
              " \t\r\n,\"'" "." 1))
  :custom
  (org-imenu-depth 5)
  (org-image-actual-width '(300))
  (org-export-with-sub-superscripts '{})
  (org-export-with-broken-links t)
  (org-export-use-babel nil) ;; 禁止在导出时执行src代码
  (org-descriptive-links nil) ;; 不要锁定连接，保持原样
  (org-src-window-setup 'split-window-right)
  (org-adapt-indentation t)
  (org-edit-src-content-indentation 0) ;; org src中不要缩进
  (org-log-done t)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
     (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
     (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
  :config

  (advice-add 'org-todo :after 'org-save-all-org-buffers)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (defun maple/org-as-html()
    (interactive)
    (unless (featurep 'ox-html)
      (require 'ox-html))
    (let ((org-html-head-include-scripts nil)
          (org-html-head-include-default-style nil))
      (org-html-export-as-html nil nil nil t)))

  (when (string> (org-version) "9.5")
    (add-to-list 'org-modules 'org-tempo t)
    ;; disable auto add comma prepended
    (fset 'org-escape-code-in-region 'ignore))
  :keybind
  (:states normal :map org-mode-map
           ("RET" . org-open-at-point)
           ("t" . org-todo))
  (:states (normal insert) :map org-mode-map
           ([tab] . org-cycle))
  :dependencies
  (htmlize))

(use-package org-crypt
  :ensure nil
  :after-call org-mode-hook
  :custom
  (org-crypt-tag-matcher "secret")
  (org-tags-exclude-from-inheritance '("secret"))
  (org-crypt-key "21305E7E")
  :config
  (org-crypt-use-before-save-magic))

(use-package ob-python
  :ensure nil
  :config
  (add-to-list 'org-babel-default-header-args:python '(:results . "output")))

(use-package ox-md
  :ensure nil
  :config
  (defun maple/org-md-example-block (example-block _contents info)
    (format "```%s\n%s\n```"
            (org-element-property :language example-block)
            (org-remove-indentation
             (org-export-format-code-default example-block info))))
  (advice-add 'org-md-example-block :override 'maple/org-md-example-block))

(use-package org-capture
  :ensure nil
  :config
  (maple-org/capture-init))

(use-package org-agenda
  :ensure nil
  :custom
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-inhibit-startup t)   ;; ~50x speedup
  (org-agenda-use-tag-inheritance nil)
  :config
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)

  (maple-org/agenda-init)
  :keybind (:map org-agenda-mode-map
                 ("j" . org-agenda-next-line)
                 ("k" . org-agenda-previous-line)
                 ("M-j" . org-agenda-next-item)
                 ("M-k" . org-agenda-previous-item)
                 ("M-h" . org-agenda-earlier)
                 ("M-l" . org-agenda-later)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("①" "②" "③" "④" "⑤")))

(use-package org-download
  :commands
  (org-download-image
   org-download-delete
   org-download-screenshot
   maple/org-download-yank)
  :custom
  (:default
   (org-download-image-dir "images/")
   (org-download-heading-lvl nil)
   (org-download-display-inline-images nil)
   (org-download-screenshot-method
    (if maple-system-is-mac "screencapture -i %s" "scrot -s %s")))
  :config
  (defun maple/org-download-yank()
    (interactive)
    (let* ((file  org-download-screenshot-file)
           (tiff  (format "%s.tiff" file))
           (image (get-text-property 0 'display (current-kill 0))))
      (if (not (eq (car-safe image) 'image)) (org-download-yank)
        (with-temp-buffer
          (insert (plist-get (cdr image) :data))
          (write-region (point-min) (point-max) tiff))
        (when (memq (plist-get (cdr image) :type) '(image-io))
          (shell-command (format "tifftopnm %s | pnmtopng > %s" tiff file)))
        (when (file-exists-p tiff) (delete-file tiff t))
        (when (file-exists-p file) (org-download-image file) (delete-file file t))))))

(provide 'init-org)
;;; init-org.el ends here
