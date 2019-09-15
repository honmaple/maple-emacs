;;; init-org.el --- Org Mode configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2019 lin.jiang

;; Author: lin.jiang <mail@honmaple.com>
;; URL: https://github.com/honmaple/dotfiles/tree/master/emacs.d

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
(use-package org-plus-contrib)

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
        org-image-actual-width '(600)
        org-export-with-sub-superscripts '{}
        org-export-with-broken-links t
        org-descriptive-links nil ;; 不要锁定连接，保持原样
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
          (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
          (sequence "WAITING(w@/!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))

  (when (display-graphic-p)
    ;; (set-face-attribute 'org-table nil :font "-Misc-Fixed-normal-normal-normal-*-18-*-*-*-c-90-iso10646-1")
    (set-face-attribute 'org-table nil :font "Inconsolata 12")
    ;; (set-face-attribute 'org-table nil :font "-jis-fixed-medium-r-normal--16-*-75-75-c-160-jisx0208.1983-0")
    ;; (set-face-attribute 'org-table nil :font "-Sony-Sony Fixed-normal-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    )

  (when (version< "9.1.4" (org-version))
    (add-to-list 'org-modules 'org-tempo)
    ;; disable auto add comma prepended
    (fset 'org-escape-code-in-region 'ignore)
    (maple/add-hook 'org-mode-hook
      (setq electric-pair-inhibit-predicate
            (lambda (c)
              (if (char-equal c ?<) t (electric-pair-default-inhibit c))))))

  (use-package org-crypt
    :ensure nil
    :demand
    :config
    (org-crypt-use-before-save-magic)
    (setq org-crypt-tag-matcher "secret"
          org-tags-exclude-from-inheritance (quote ("secret"))
          org-crypt-key "21305E7E"))

  :evil-bind
  (:state normal :map org-mode-map
          ("RET" . org-open-at-point)
          ("t" . org-todo)
          ("TAB" . org-cycle)
          ("<tab>" . org-cycle)))

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


(use-package org-capture
  :ensure nil
  :config

  (use-package maple-org
    :ensure nil
    :demand
    :defines maple/org-capture-templates
    :config
    (setq org-capture-templates maple/org-capture-templates)

    (maple/org-capture-snip "sp" "python" '("Tool" "Flask" "Tornado"))
    (maple/org-capture-snip "sl" "lua" '("Tool" "Nginx"))
    (maple/org-capture-snip "sg" "golang" '("Tool")))

  (setq org-refile-targets
        (quote (("~/org-mode/gtd.org" :level . 1)
                ("~/org-mode/summary.org" :maxlevel . 4))))
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-todo :after 'org-save-all-org-buffers))

(use-package org-agenda
  :ensure nil
  :config
  (setq org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window
        org-agenda-inhibit-startup t   ;; ~50x speedup
        org-agenda-use-tag-inheritance nil ;; 3-4x speedup
        org-log-done t
        org-agenda-files '("~/org-mode" )
        org-default-notes-file "~/org-mode/gtd.org")
  (setq org-agenda-custom-commands
        '(("b"  "博客" tags-todo "blog")
          ("p"  "项目" tags-todo "@Office")
          ("w" "Weekly Review"
           ((stuck "")
            (tags-todo "project")))))
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

(provide 'init-org)

;;; init-org.el ends here
