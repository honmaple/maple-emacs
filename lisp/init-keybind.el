;;; init-keybind.el --- Keybind configurations.	-*- lexical-binding: t -*-
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
;; Keybind configurations.
;;

;;; Code:
(defalias 'maple/projectile-grep 'maple/consult-project-grep)
(defalias 'maple/projectile-find-file 'maple/project-find-file)

(defalias 'find-file-jump 'maple/find-file)

(defalias 'maple-file/grep 'maple/consult-grep)
(defalias 'maple-file/grep-prompt 'maple/consult-prompt-grep)

;; (defalias 'maple/projectile-grep 'counsel-projectile-rg)
;; (defalias 'maple/projectile-find-file 'counsel-git)

;; (defalias 'find-file-jump 'counsel-file-jump)

;; (defalias 'maple-file/grep 'maple/counsel-grep)
;; (defalias 'maple-file/grep-prompt 'maple/counsel-grep-dir)

(maple-define-key
  :prefix ","
  :states '(normal motion)
  "b" '(:ignore t :desc "buffer")
  "bk" 'kill-this-buffer
  "bb" 'switch-to-buffer
  "be" 'maple-buffer/safe-erase
  "bh" 'maple-buffer/switch-to-scratch
  "bK" 'maple-buffer/kill-others
  "bR" 'maple-buffer/safe-revert
  "bP" 'maple-buffer/copy-clipboard
  "bY" 'maple-buffer/copy-to-clipboard
  "TAB" 'maple-buffer/switch-to-previous
  "bm" 'bookmark-set
  "bj" 'bookmark-jump
  "bs" 'bookmark-save
  "bw" 'read-only-mode
  "bp" 'evil-prev-buffer
  "bn" 'evil-next-buffer
  "bl" 'maple-note
  "bi" 'maple-explorer-file

  ;; "e"  '(:ignore t :desc "flycheck")
  ;; "el" 'flycheck-list-errors
  ;; "ec" 'flycheck-clear
  ;; "es" 'flycheck-select-checker
  ;; "en" 'flycheck-next-error
  ;; "ep" 'flycheck-previous-error

  "e"  '(:ignore t :desc "flymake")
  "el" 'consult-flymake
  "eL" 'maple/consult-project-flymake
  "ec" 'flymake-proc-stop-all-syntax-checks
  "en" 'flymake-goto-next-error
  "ep" 'flymake-goto-prev-error

  "f"  '(:ignore t :desc "file")
  "fj"  'dired-jump
  "fl"  'find-file-literally
  "fS"  'evil-write-all
  "fs"  'save-buffer
  "fb"  'bookmark-jump
  "fei" 'maple-file/open-init
  "fek" 'maple-file/open-keybind
  "feg" 'maple-file/open-gtd
  "fet" 'maple-file/open-test
  "fo"  'maple-file/open-in-external-app
  "fE"  'maple-file/sudo-edit
  "fy"  'maple-file/show-and-copy-buffer-filename
  "fY"  'maple-file/copy-buffer-filename
  "fCd" 'maple-file/unix2dos
  "fCu" 'maple-file/dos2unix
  "fD"  'maple-file/delete
  "fR"  'maple-file/rename
  "ff"  'find-file
  "fF"  'find-file-jump
  "fr"  'recentf-open-files
  "fw"  'maple-file/grep
  "fW"  'maple-file/grep-prompt

  "i" '(:ignore t :desc "insert icon")
  "ii" 'all-the-icons-insert
  "if" 'all-the-icons-insert-faicon
  "im" 'all-the-icons-insert-material

  "in" '(:ignore t :desc "insert nerd icon")
  "ini" 'nerd-icons-insert
  "inf" 'nerd-icons-insert-faicon
  "inm" 'nerd-icons-insert-mdicon

  :package 'magit
  "g" '(:ignore t :desc "git")
  "gi" 'magit-init
  "gs" 'magit-status
  "gb" 'magit-branch
  "gd" 'magit-diff-dwim
  "gD" 'magit-branch-delete
  "gc" 'magit-branch-or-checkout
  "gf" 'magit-file-dispatch
  "gt" 'magit-blob-previous

  :package nil
  "n" '(:ignore t :desc "narrow")
  "nr" 'narrow-to-region
  "np" 'narrow-to-page
  "nf" 'narrow-to-defun
  "nw" 'widen

  "o" '(:ignore t :desc "org")
  "oa" 'org-agenda
  "oc" 'org-capture
  "ob" 'org-switchb
  "od" 'maple-translate
  "oD" 'maple-translate+
  "op" 'browse-at-remote

  "p" '(:ignore t :desc "project")
  "pb"  'projectile-switch-to-buffer
  "pp"  'projectile-switch-project
  "pr"  'projectile-recentf
  "pi"  'projectile-invalidate-cache
  "pc"  'projectile-cleanup-known-projects
  "pa"  'projectile-add-known-project
  "pd"  'projectile-find-dir
  "pf"  'maple/projectile-find-file
  "pw"  'maple/projectile-grep

  "s"  '(:ignore t :desc "search and replace")
  "se" 'maple-iedit-match-all
  "ss" 'anzu-query-replace-regexp
  "sd" 'delete-matching-lines
  "sD" 'delete-non-matching-lines
  "sp" 'flyspell-correct-word-generic
  "sh" 'shell-command
  "sj" 'imenu

  "sq" '(:ignore t :desc "sql")
  "sql" 'sql-sqlite
  "sqp" 'sql-postgres
  "sqm" 'sql-mysql

  "t" '(:ignore t :desc "toggle mode")
  "tn" 'display-line-numbers-mode
  "tV" 'visual-line-mode
  "th" 'maple-theme/switch

  "w" '(:ignore t :desc "window")
  ;; "ww" 'split-window-horizontally-instead
  ;; "wo" 'find-file-other-window
  ;; ;; 关闭其他窗口
  "wC"  'delete-other-windows
  "wc"  'delete-window
  "wH"  'evil-window-move-far-left
  "wh"  'evil-window-left
  "wJ"  'evil-window-move-very-bottom
  "wj"  'evil-window-down
  "wK"  'evil-window-move-very-top
  "wk"  'evil-window-up
  "wL"  'evil-window-move-far-right
  "wl"  'evil-window-right
  "wo"  'other-frame
  "ws"  'split-window-below
  "wU"  'winner-redo
  "wu"  'winner-undo
  "wv"  'split-window-right
  "ww"  'other-window
  "w="  'balance-windows
  "w0"  'select-window-0
  "w1"  'select-window-1
  "w2"  'select-window-2
  "w3"  'select-window-3
  "w4"  'select-window-4
  "w5"  'select-window-5

  "c"  '(:ignore t :desc "comment")
  "cc" 'maple-language-comment
  "cC" 'maple-language-comment-and-copy
  "="  'maple-language-format
  "'"  'maple/shell
  "rc" 'recentf-cleanup
  "rr" 'maple-run
  "u"  'undo-tree-visualize
  "h"  'help
  ","  'execute-extended-command
  "/"  'isearch-backward

  "d"  '(:ignore t :desc "diff")
  "dn" 'maple-diff:next-sign
  "dp" 'maple-diff:previous-sign

  ;; 字体大小写
  "vU" 'upcase-dwim
  "vu" 'downcase-dwim

  "qr" 'restart-emacs
  "qk" 'kill-emacs

  :map 'web-mode-map
  "ra" 'web-mode-element-clone
  "rd" 'web-mode-element-vanish
  "rk" 'web-mode-element-kill
  "rr" 'web-mode-element-rename
  "rw" 'web-mode-element-wrap
  "gb" 'web-mode-element-beginning
  "gc" 'web-mode-element-child
  "gp" 'web-mode-element-parent
  ;; "gs" 'web-mode-element-sibling-next

  :map 'python-mode-map
  "va" 'pyenv-mode-set
  "vc" 'pyenv-mode-unset
  "rI" 'py-isort-buffer

  :map 'go-mode-map
  "ri" 'go-import-add
  "rd" 'godef-describe
  "rt" 'go-add-tags

  :map 'org-mode-map
  "oe" 'org-export-dispatch
  "ot" 'org-set-tags
  "o." 'org-time-stamp
  "or" 'org-decrypt-entry
  "ow" 'org-refile
  "td" 'org-deadline
  "ts" 'org-schedule
  "tm" 'org-toggle-inline-images
  "im" 'maple-org/capture-screenshot
  )

(maple-define-key
  [remap keyboard-quit] 'maple-escape
  [f5] 'maple-language-run
  [f6] 'maple-language-format

  :states '(normal motion)
  "M-J" 'evil-window-move-very-bottom
  "M-K" 'evil-window-move-very-top
  "M-L" 'evil-window-move-far-right
  "M-H" 'evil-window-move-far-left
  "M-j" 'evil-window-down
  "M-k" 'evil-window-up
  "M-l" 'evil-window-right
  "M-h" 'evil-window-left
  "H" "^"
  "L" "$"
  "U" 'undo-tree-redo
  "za" 'maple-language-fold
  "gd" 'maple-language-find-definition

  :states 'insert
  "C-h" 'left-char
  "C-l" 'right-char
  "C-j" 'next-line
  "C-k" 'previous-line

  :states 'visual
  "H" "^"
  "L" (lambda ()
        (interactive)
        (end-of-line))
  "<" (lambda ()
        (interactive)
        (call-interactively 'evil-shift-left)
        (evil-normal-state)
        (evil-visual-restore))
  ">" (lambda ()
        (interactive)
        (call-interactively 'evil-shift-right)
        (evil-normal-state)
        (evil-visual-restore)))

(when maple-system-is-mac
  (maple-define-key
    (kbd "s-c")  'copy-region-as-kill
    (kbd "s-x")  'kill-region
    (kbd "s-v")  'yank
    (kbd "s-s")  'save-buffer
    (kbd "s-z")  'undo
    (kbd "s-a")  'mark-whole-buffer
    (kbd "s-q")  'save-buffer-kill-emacs))

(provide 'init-keybind)
;;; init-keybind.el ends here
