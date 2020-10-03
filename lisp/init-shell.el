;;; init-shell.el --- Initialize shell configurations.	-*- lexical-binding: t -*-

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
;; Shell configurations.
;;

;;; Code:
(use-package comint
  :ensure nil
  :hook (comint-mode . maple/process-exit)
  :config
  (setq comint-prompt-read-only t)
  :bind
  (:map comint-mode-map
        ("<up>" . comint-previous-input)
        ("<down>" . comint-next-input)
        ("<escape>" . (lambda() (interactive) (goto-char (cdr comint-last-prompt))))))

(use-package xterm-color
  :hook (comint-preoutput-filter-functions . xterm-color-filter)
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions)))

(use-package term
  :ensure nil
  :commands (maple/shell)
  :hook (term-mode . maple/process-exit)
  :config
  (defun term-send-tab ()
    "Send tab in term mode."
    (interactive)
    (term-send-raw-string "\t"))

  (defun term-kill-ring()
    (interactive)
    (if (use-region-p)
        (call-interactively 'copy-region-as-kill)
      (term-send-raw-string "y")))

  (defun maple/shell()
    (interactive)
    (cl-letf (((symbol-function 'switch-to-buffer) 'pop-to-buffer))
      (term "/bin/bash")))
  :evil
  (insert :map term-raw-map
          ("C-c C-d" . term-send-eof)
          ("C-c C-z" . term-stop-subjob)
          ("C-y" . term-paste)
          ("y" . term-kill-ring)
          ("<tab>" . term-send-tab)
          ("C-k" . term-send-up)
          ("C-j" . term-send-down)))

(use-package multi-term
  :config
  (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab)))

(provide 'init-shell)
;;; init-shell.el ends here
