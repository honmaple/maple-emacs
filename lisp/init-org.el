;;; init-org.el --- Org Mode configurations.	-*- lexical-binding: t -*-

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
;; mine org-mode configurations.
;;

;;; Code:
(use-package org-faces
  :ensure org-plus-contrib
  :config
  (when (display-graphic-p)
    ;; (set-face-attribute 'org-table nil :font "-Misc-Fixed-normal-normal-normal-*-18-*-*-*-c-90-iso10646-1")
    ;; (set-face-attribute 'org-table nil :font "-jis-fixed-medium-r-normal--16-*-75-75-c-160-jisx0208.1983-0")
    ;; (set-face-attribute 'org-table nil :font "-Sony-Sony Fixed-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    (set-face-attribute 'org-table nil :font "Inconsolata 12")))

(use-package ox-confluence
  :ensure nil
  :commands (org-confluence-export-as-confluence))

(use-package org
  :ensure nil
  :init
  ;; markup记号前后允许中文, 必须在(require 'org)前
  (setq org-emphasis-regexp-components
        (list (concat "- \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["   "[:nonascii:]")
              " \t\r\n,\"'" "." 1))
  :config
  (setq org-imenu-depth 5
        org-image-actual-width '(300)
        org-export-with-sub-superscripts '{}
        org-export-with-broken-links t
        org-descriptive-links nil ;; 不要锁定连接，保持原样
        org-src-window-setup 'split-window-right
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
          (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
          (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))

  (advice-add 'org-todo :after 'org-save-all-org-buffers)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (defun maple/org-as-html()
    (interactive)
    (let ((org-html-head-include-scripts nil)
          (org-html-head-include-default-style nil))
      (org-html-export-as-html nil nil nil t)))

  (when (version< "9.1.4" (org-version))
    (add-to-list 'org-modules 'org-tempo)
    ;; disable auto add comma prepended
    (fset 'org-escape-code-in-region 'ignore)
    (maple/add-hook 'org-mode-hook
      (setq electric-pair-inhibit-predicate
            (lambda (c)
              (if (char-equal c ?<) t (electric-pair-default-inhibit c))))))

  :evil-bind
  (:state normal :map org-mode-map
          ("RET" . org-open-at-point)
          ("t" . org-todo)
          ("TAB" . org-cycle)
          ("<tab>" . org-cycle)))

(use-package org-crypt
  :ensure nil
  :after org :demand
  :config
  (org-crypt-use-before-save-magic)
  (setq org-crypt-tag-matcher "secret"
        org-tags-exclude-from-inheritance '("secret")
        org-crypt-key "21305E7E"))

(use-package ob
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (dot . t)
     (octave . t)
     (sqlite . t)
     (perl . t)
     (C . t)))
  (use-package ob-python
    :ensure nil
    :config
    (add-to-list 'org-babel-default-header-args:python
                 '(:results . "output"))))

(use-package maple-org
  :ensure nil :demand)

(use-package org-capture
  :ensure nil)

(use-package org-agenda
  :ensure nil
  :config
  (setq org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window
        org-agenda-inhibit-startup t   ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        org-log-done t)
  (setq org-agenda-custom-commands
        '(("b"  "博客" tags-todo "Blog")
          ("p"  "项目" tags-todo "@Office")
          ("w" "Weekly Review"
           ((stuck "") (tags-todo "Project")))))
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)
  :bind (:map org-agenda-mode-map
              ("j" . org-agenda-next-line)
              ("k" . org-agenda-previous-line)
              ("M-j" . org-agenda-next-item)
              ("M-k" . org-agenda-previous-item)
              ("M-h" . org-agenda-earlier)
              ("M-l" . org-agenda-later)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("①" "②" "③" "④" "⑤")))

(use-package org-download
  :commands
  (org-download-image
   org-download-delete
   org-download-screenshot
   maple/org-download-yank)
  :config
  (setq-default org-download-image-dir "images/"
                org-download-heading-lvl nil
                org-download-screenshot-method
                (if maple-system-is-mac "screencapture -i %s" "scrot -s %s"))

  (defun maple/org-download-yank()
    (interactive)
    (let ((file  org-download-screenshot-file)
          (image (get-text-property 0 'display (current-kill 0))))
      (if (or (not (consp image)) (not (eq (car image) 'image)))
          (org-download-yank)
        (with-temp-buffer
          (insert (plist-get (cdr image) :data))
          (let ((coding-system-for-write 'no-conversion))
            (write-region nil nil file)))
        (when (file-exists-p file)
          (org-download-image file)
          (delete-file file))))))

(provide 'init-org)
;;; init-org.el ends here
