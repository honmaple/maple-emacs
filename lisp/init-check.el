;;; init-check.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2015-2023 lin.jiang

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
;; flymake or flycheck configurations.
;;

;;; Code:
(pcase maple-syntax-checker
  ('flymake
   (use-package flymake
     :ensure nil
     :hook ((prog-mode . flymake-mode))
     :config
     (setq flymake-no-changes-timeout 0.8)
     (fset 'flymake-eldoc-function 'ignore)
     (maple-evil-map flymake-diagnostics-buffer-mode-map))

   (use-package flymake-popon
     :hook (flymake-mode . flymake-popon-mode)
     :custom
     (flymake-popon-method (if (display-graphic-p) 'posframe 'popon))))
  ('flycheck
   (use-package flycheck
     :diminish flycheck-mode "ⓢ"
     :hook ((prog-mode . flycheck-mode)
            (flycheck-error-list-mode . maple-truncate-lines))
     :custom
     (flycheck-idle-change-delay 0.8)
     (flycheck-checker-error-threshold 9999)
     (flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change mode-enabled))
     :config
     (when (fboundp 'define-fringe-bitmap)
       (define-fringe-bitmap 'maple/flycheck-fringe-indicator
         (vector #b00000000
                 #b00000000
                 #b00000000
                 #b00000000
                 #b00000000
                 #b00000000
                 #b00000000
                 #b00011100
                 #b00111110
                 #b00111110
                 #b00111110
                 #b00011100
                 #b00000000
                 #b00000000
                 #b00000000
                 #b00000000
                 #b00000000))

       (flycheck-redefine-standard-error-levels nil 'maple/flycheck-fringe-indicator))
     :keybind
     (:states normal :map flycheck-error-list-mode-map
              ("q" . quit-window)
              ("j" . flycheck-error-list-next-error)
              ("k" . flycheck-error-list-previous-error)
              ("K" . evil-previous-line)
              ("J" . evil-next-line)
              ("RET" . flycheck-error-list-goto-error)))

   ;; 显示tooltip
   (use-package flycheck-pos-tip
     :hook (flycheck-mode . flycheck-pos-tip-mode)
     :custom
     (pos-tip-internal-border-width 5))))

(provide 'init-check)
;;; init-check.el ends here
