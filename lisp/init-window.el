;;; init-window.el --- Initialize window configurations.	-*- lexical-binding: t -*-

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
  :hook (maple-init . winner-mode))

(use-package window-numbering
  :hook (maple-theme . window-numbering-mode)
  :config
  (fset 'window-numbering-install-mode-line 'ignore))

(use-package zoom
  :hook (maple-init . zoom-mode)
  :config
  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-major-modes
        '(term-mode shell-mode flycheck-error-list-mode ediff-mode)
        zoom-ignored-buffer-names '(" *Org todo*"))

  (defun maple/balance-windows(func &optional window-or-frame)
    (unless (zoom--window-ignored-p)
      (let ((pre-redisplay-functions nil)
            (window-configuration-change-hook nil))
        (funcall func window-or-frame))))

  (advice-add 'balance-windows :around 'maple/balance-windows))

(use-package shackle
  :hook (maple-init . shackle-mode)
  :config
  (setq shackle-default-size 0.3
        shackle-default-alignment 'below
        shackle-default-rule nil
        shackle-rules
        '((("*compilation*" "*Completions*" "*ert*" "*Warnings*" "*Messages*")
           :align 'below :autoclose t)
          (("*Help*" "*Backtrace*")
           :select t :align 'below :autoclose t)
          (("^\\*.*Shell Command.*\\*$" "\\*[Wo]*Man.*\\*")
           :regexp t :align 'below :autoclose t)
          (" *undo-tree*"
           :select t :autoclose t)

          (flycheck-error-list-mode :select t :align 'below :autoclose t)
          (inferior-python-mode :select t)
          (comint-mode :align 'below)
          (term-mode :align 'below :select t :size 15)
          (process-menu-mode :select t :align 'below :autoclose t))))

(provide 'init-window)
;;; init-window.el ends here
