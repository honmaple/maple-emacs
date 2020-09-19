;;; init-keybind.el --- Keybind configurations.	-*- lexical-binding: t -*-

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
;; Keybind configurations.
;;

;;; Code:

(with-eval-after-load 'evil-leader
  (evil-leader/set-key
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
    )

  (evil-leader/set-key
    "cc" 'maple-language:comment
    "cC" 'maple-language:copy-and-comment
    "u"  'undo-tree-visualize
    "'" 'maple/shell
    "=" 'maple-language:call-format
    "se" 'maple-iedit-match-all
    "ss" 'anzu-query-replace-regexp
    "sd" 'delete-matching-lines
    "sD" 'delete-non-matching-lines
    "sp" 'flyspell-correct-word-generic
    "sh" 'shell-command
    "Sg" 'maple/search-google
    "Sh" 'maple/search-github
    )

  (evil-leader/set-key
    "el" 'flycheck-list-errors
    "ec" 'flycheck-clear
    "es" 'flycheck-select-checker
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    )

  ;; 字体大小写
  (evil-leader/set-key
    "vU" 'upcase-dwim
    "vu" 'downcase-dwim
    )

  (evil-leader/set-key
    "h"  'help
    "rc" 'recentf-cleanup
    "rr" 'maple-run
    )

  (evil-leader/set-key
    "tg" 'golden-ratio-mode
    "tS" 'flyspell-mode
    "ts" 'flycheck-mode
    "tc" 'company-mode
    "tf" 'fci-mode
    "tn" 'display-line-numbers-mode
    "tv" 'smooth-scrolling-mode
    "tV" 'visual-line-mode
    "th" 'maple-theme:switch/body
    )

  (evil-leader/set-key
    "gi" 'magit-init
    "gs" 'magit-status
    "gb" 'magit-branch
    "gd" 'magit-diff-dwim
    "gD" 'magit-branch-delete
    "gc" 'magit-branch-or-checkout
    "gt" 'git-timemachine
    )

  (evil-leader/set-key
    ;; "ww" 'split-window-horizontally-instead
    ;; "wo" 'find-file-other-window
    ;; ;; 关闭其他窗口
    "wC"  'delete-other-windows
    "wc"  'delete-window
    "wH"  'evil-window-move-far-left
    "wh"  'evil-window-left
    "w <left>"  'evil-window-left
    "wJ"  'evil-window-move-very-bottom
    "wj"  'evil-window-down
    "w <down>"  'evil-window-down
    "wK"  'evil-window-move-very-top
    "wk"  'evil-window-up
    "w <up>"  'evil-window-up
    "wL"  'evil-window-move-far-right
    "wl"  'evil-window-right
    "w <right>"  'evil-window-right
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
    )

  (evil-leader/set-key
    "pb"  'projectile-switch-to-buffer
    "pw"  'projectile-ag
    "pd"  'projectile-find-dir
    "pf"  'projectile-find-file
    "pp"  'projectile-switch-project
    "pr"  'projectile-recentf
    "pi"  'projectile-invalidate-cache
    "pc"  'projectile-cleanup-known-projects
    "pa"  'projectile-add-known-project
    )

  (evil-leader/set-key
    "TAB" 'maple-buffer/switch-to-previous
    "bk" 'kill-this-buffer
    "be" 'maple-buffer/safe-erase
    "bh" 'maple-buffer/switch-to-scratch
    "bK" 'maple-buffer/kill-others
    "bR" 'maple-buffer/safe-revert
    "bP" 'maple-buffer/copy-clipboard
    "bY" 'maple-buffer/copy-to-clipboard
    "bm" 'bookmark-set
    "bj" 'bookmark-jump
    "bs" 'bookmark-save
    "bw" 'read-only-mode
    "bp" 'evil-prev-buffer
    "bn" 'evil-next-buffer
    "bl" 'maple-note
    "bi" 'maple-explorer-file
    )

  (evil-leader/set-key
    "nr" 'narrow-to-region
    "np" 'narrow-to-page
    "nf" 'narrow-to-defun
    "nw" 'widen)

  (evil-leader/set-key
    "sql" 'sql-sqlite
    "sqp" 'sql-postgres
    "sqm" 'sql-mysql
    )

  (evil-leader/set-key
    "oa" 'org-agenda
    "oc" 'org-capture
    "ob" 'org-switchb
    "od" 'youdao-dictionary-search-at-point+
    "oD" 'youdao-dictionary-search-at-point
    "op" 'browse-at-remote
    )

  (evil-leader/set-key
    "qr" 'restart-emacs
    "qR" 'maple/restart-emacs
    "qk" 'kill-emacs
    "qc" 'maple/reload-user-init-file
    "qQ" 'maple/kill-emacs
    )

  (evil-leader/set-key-for-mode 'web-mode
    "ra" 'web-mode-element-clone
    "rd" 'web-mode-element-vanish
    "rk" 'web-mode-element-kill
    "rr" 'web-mode-element-rename
    "rw" 'web-mode-element-wrap
    "gb" 'web-mode-element-beginning
    "gc" 'web-mode-element-child
    "gp" 'web-mode-element-parent
    ;; "gs" 'web-mode-element-sibling-next
    )

  (evil-leader/set-key-for-mode 'python-mode
    "va" 'pyenv-mode-set
    "vc" 'pyenv-mode-unset
    "rI" 'py-isort-buffer
    )

  (evil-leader/set-key-for-mode 'go-mode
    "ri" 'go-goto-imports
    "rI" 'go-remove-unused-imports
    "ra" 'go-import-add
    "rd" 'godef-describe
    "rt" 'go-add-tags
    )

  (evil-leader/set-key-for-mode 'org-mode
    "oe" 'org-export-dispatch
    "ot" 'org-set-tags
    "o." 'org-time-stamp
    "op" 'org-priority
    "oP" 'org-pomodoro
    "or" 'org-decrypt-entry
    "ow" 'org-refile
    "td" 'org-deadline
    "ts" 'org-schedule
    "tm" 'org-toggle-inline-images
    "im" 'maple/capture-screenshot
    "em" 'maple/org-md-export-to-markdown
    "eh" 'maple/org-html-export-to-html)

  ;; ivy
  (evil-leader/set-key
    "," 'counsel-M-x
    "/" 'counsel-grep-or-swiper
    "ff" 'find-file
    "fF" 'counsel-file-jump
    "fr" 'counsel-recentf
    "fw" 'counsel-ag
    "fW" 'maple/counsel-ag-directory
    "sj" 'counsel-semantic-or-imenu
    "bb" 'ivy-switch-buffer  ;;显示缓冲区(已经打开的文件)
    "ph" 'counsel-projectile ;;在工程内查找
    "pw" 'counsel-projectile-ag
    "pb" 'counsel-projectile-switch-to-buffer))

(with-eval-after-load 'evil
  (maple/define-key evil-normal-state-map
    (kbd "M-J") 'evil-window-move-very-bottom
    (kbd "M-K") 'evil-window-move-very-top
    (kbd "M-L") 'evil-window-move-far-right
    (kbd "M-H") 'evil-window-move-far-left
    (kbd "M-j") 'evil-window-down
    (kbd "M-k") 'evil-window-up
    (kbd "M-l") 'evil-window-right
    (kbd "M-h") 'evil-window-left
    (kbd "H") (kbd "^")
    (kbd "L") (kbd "$")
    (kbd "U") 'undo-tree-redo
    ;; (kbd "RET") 'maple-evil/insert-line-below
    (kbd "S-<return>") 'maple-evil/insert-line-above
    (kbd "gd") 'maple-language:call-definition
    (kbd "za") 'maple-language:call-fold)

  (maple/define-key evil-insert-state-map
    (kbd "C-h") 'left-char
    (kbd "C-l") 'right-char
    (kbd "C-j") 'next-line
    (kbd "C-k") 'previous-line)

  (maple/define-key evil-visual-state-map
    (kbd "H") (kbd "^")
    (kbd "L") (lambda ()
                (interactive)
                (evil-end-of-line))
    (kbd "<") (lambda ()
                (interactive)
                (call-interactively 'evil-shift-left)
                (evil-normal-state)
                (evil-visual-restore))
    (kbd ">") (lambda ()
                (interactive)
                (call-interactively 'evil-shift-right)
                (evil-normal-state)
                (evil-visual-restore))))

(defun maple/escape ()
  "Run maple/escape."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         (abort-recursive-edit))
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] 'maple/escape)
(global-set-key [f5] 'maple-language:call-run)
(global-set-key [f6] 'maple-language:call-format)
(global-set-key [tab] 'maple/company-or-indent)

(when maple-system-is-mac
  (maple/define-key (current-global-map)
    (kbd "s-c")  'copy-region-as-kill
    (kbd "s-x")  'kill-region
    (kbd "s-v")  'yank
    (kbd "s-s")  'save-buffer
    (kbd "s-z")  'undo
    (kbd "s-a")  'mark-whole-buffer
    (kbd "s-q")  'save-buffer-kill-emacs))

(provide 'init-keybind)
;;; init-keybind.el ends here
