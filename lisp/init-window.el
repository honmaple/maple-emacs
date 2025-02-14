;;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-

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
;; Window configurations.
;;

;;; Code:

(use-package window
  :ensure nil
  :init
  (setq split-width-threshold 100
        window-combination-resize t))

(use-package winner
  :ensure nil
  :hook (maple-init . winner-mode)
  :config
  (add-to-list 'winner-boring-buffers-regexp "[^\s]\\*.*\\*"))

(use-package window-numbering
  :hook (maple-theme . window-numbering-mode)
  :config
  (fset 'window-numbering-install-mode-line 'ignore))

(use-package zoom
  :hook (maple-init . zoom-mode)
  :custom
  (zoom-size '(0.618 . 0.618))
  (zoom-ignored-major-modes
   '(term-mode shell-mode ediff-mode))
  (zoom-ignored-buffer-names '(" *Org todo*"))
  :config
  (defun maple/balance-windows(func &optional window-or-frame)
    (unless (zoom--window-ignored-p)
      (let ((pre-redisplay-functions nil)
            (window-configuration-change-hook nil))
        (funcall func window-or-frame))))

  (advice-add 'balance-windows :around 'maple/balance-windows))

(use-package shackle
  :hook (maple-init . shackle-mode)
  :config
  (advice-add 'shackle-match :filter-return
              (lambda(x) (when x (append x '(:align 'below :autoclose t)))))
  :custom
  (shackle-default-size 0.3)
  (shackle-default-rule nil)
  (shackle-rules
   '((("*compilation*" "*Completions*" "*ert*" "*Warnings*" "*Messages*"))
     (("*Help*" "*Backtrace*" " *undo-tree*")
      :select t)
     (("^\\*.*Shell Command.*\\*$" "\\*[Wo]*Man.*\\*")
      :regexp t)

     ((term-mode comint-mode inferior-python-mode)
      :select t :popup t)
     ((process-menu-mode)
      :select t))))

(provide 'init-window)
;;; init-window.el ends here
