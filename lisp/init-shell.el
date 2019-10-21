;;; init-shell.el --- Initialize shell configurations.	-*- lexical-binding: t -*-

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
;; Shell configurations.
;;

;;; Code:

(use-package multi-term
  :config
  (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab)))

(use-package shell-pop
  :init
  (setq shell-pop-window-position "bottom"
        shell-pop-term-shell "/bin/bash"
        shell-pop-full-span t
        shell-pop-shell-type (if maple-system-is-windows
                                 '("eshell" "*eshell*" (lambda () (eshell)))
                               '("ansi-term" "*ansi-term*"
                                 (lambda () (ansi-term shell-pop-term-shell)))))
  (maple/add-hook 'term-mode-hook 'maple/close-process)
  (defun term-send-tab ()
    "Send tab in term mode."
    (interactive)
    (term-send-raw-string "\t"))
  :evil-bind
  (:state normal :map term-raw-map
          ("p" . term-paste))
  (:state insert :map term-raw-map
          ("C-c C-d" . term-send-eof)
          ("C-c C-z" . term-stop-subjob)
          ("C-y" . term-paste)
          ("<tab>" . term-send-tab)
          ("C-k" . term-send-up)
          ("C-j" . term-send-down)))

(use-package xterm-color
  :hook (comint-preoutput-filter-functions . xterm-color-filter)
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

(provide 'init-shell)
;;; init-shell.el ends here
