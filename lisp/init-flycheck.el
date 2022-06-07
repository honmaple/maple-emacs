;;; init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

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
;; flycheck configurations.
;;

;;; Code:
(use-package flycheck
  :diminish flycheck-mode "ⓢ"
  :hook (prog-mode . flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-buffer-switch idle-change mode-enabled)
        flycheck-idle-change-delay 0.8)

  (let ((govet (flycheck-checker-get 'go-vet 'command)))
    (when (equal (cadr govet) "tool")
      (setf (cdr govet) (cddr govet))))

  (when (and (fboundp 'define-fringe-bitmap))
    (define-fringe-bitmap 'maple-flycheck-fringe-indicator
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
              #b00000000)))

  (let ((bitmap 'maple-flycheck-fringe-indicator))
    (flycheck-define-error-level 'error
      :severity 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-error)
    (flycheck-define-error-level 'warning
      :severity 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-warning)
    (flycheck-define-error-level 'info
      :severity 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap bitmap
      :fringe-face 'flycheck-fringe-info))
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
  :config
  (setq pos-tip-background-color (face-attribute 'default :background nil t)
        pos-tip-foreground-color (face-attribute 'default :foreground nil t)
        pos-tip-internal-border-width 5
        flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
