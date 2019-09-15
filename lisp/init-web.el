;;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-

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
;; Web configurations.
;;

;;; Code:

(use-package web-mode
  :mode ("\\.\\(vue\\|html?\\)$")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-enable-auto-closing t ; enable auto close tag in text-mode
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-indentation nil
        web-mode-enable-css-colorization nil
        web-mode-engines-alist '(("django" . "\\.\\(vue\\|html?\\)$"))
        web-mode-engines-auto-pairs '(("django" . (("{{ " . " }")
                                                   ("{% " . " %")
                                                   ("{%-" . " | %")
                                                   ("{%=" . " | %")
                                                   ("{{-" . " | }")
                                                   ("{{{" . " | }}")
                                                   ("{# " . " #")
                                                   ("<% " . " %>")))))
  (fset 'maple/put-text-property (symbol-function 'put-text-property))
  (defun maple/web-mode-put-text(p q prop value)
    (if (and (eq prop 'invisible) value) (hs-make-overlay p q 'code)
      (maple/put-text-property p q prop value)))
  (defun maple/web-mode-fold-or-unfold()
    (interactive)
    (cl-letf (((symbol-function 'put-text-property) 'maple/web-mode-put-text))
      (web-mode-fold-or-unfold)))
  (maple/add-hook 'web-mode-hook
    (setq electric-pair-pairs '((?\' . ?\'))))
  :custom
  (:language
   "web-mode"
   :run      'browse-url-of-file
   :fold     'maple/web-mode-fold-or-unfold
   :complete '(company-web-html company-css company-tern :with company-yasnippet)))

(use-package company-web)

(use-package web-beautify
  :commands (web-beautify-html web-beautify-css web-beautify-js))

(use-package emmet-mode
  :diminish emmet-mode
  :hook ((html-mode sgml-mode web-mode) . emmet-mode)
  :config
  (defun maple/emmet-expand ()
    (interactive)
    (if (bound-and-true-p yas-minor-mode)
        (call-interactively 'emmet-expand-yas)
      (call-interactively 'emmet-expand-line)))
  :evil-bind
  (:state insert :map emmet-mode-keymap
          ([tab] . maple/emmet-expand)))

(use-package css-mode
  :config
  (setq css-indent-offset 4)
  :custom
  (:language
   "css-mode"
   :complete '(company-css :with company-yasnippet)))

(use-package sass-mode
  :mode ("\\.sass\\'" . sass-mode))

(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :config (setq scss-compile-at-save nil))

(use-package less-css-mode
  :mode ("\\.less\\'" . less-css-mode))

(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :hook (js2-mode . js2-imenu-extras-mode)
  :config
  (setq js2-basic-offset 4
        js-indent-level 4
        js2-bounce-indent-p nil
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)

  (use-package tern
    :diminish tern-mode
    :hook (js2-mode . tern-mode)
    :config (add-to-list 'tern-command "--no-port-file" 'append))

  (use-package company-tern)
  :custom
  (:language
   "js2-mode"
   :complete '(company-tern)))

(use-package coffee-mode
  :mode ("\\.coffee\\.erb\\'" . coffee-mode)
  :config
  (setq coffee-tab-width 4))

(use-package npm-mode
  :hook (js2-mode . npm-mode))

(provide 'init-web)

;;; init-web.el ends here
