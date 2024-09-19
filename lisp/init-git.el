;;; init-git.el --- Initialize vcs configurations.	-*- lexical-binding: t -*-

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
;; Vcs configurations.
;;
;; git-timemachine 由 magit-blob-previous 代替

;;; Code:
(use-package magit
  :commands (magit-status)
  :custom
  (magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (magit-process-popup-time 3)
  (magit-diff-refine-hunk t)
  (magit-show-long-lines-warning nil)
  (magit-section-visibility-indicator nil)
  :config
  (with-eval-after-load 'evil
    (add-hook 'magit-blob-mode-hook 'evil-normalize-keymaps))

  ;; https://github.com/syl20bnr/spacemacs/issues/15448
  (with-eval-after-load 'evil-surround
    (add-hook 'magit-status-mode-hook #'turn-off-evil-surround-mode))

  (define-advice magit-blob-visit (:around (orig-fun &rest args) kill-all-blob-after-quit)
    (let ((prev-buffer (current-buffer)))
      (apply orig-fun args)
      (unless (and magit-buffer-file-name (equal magit-buffer-file-name (buffer-file-name prev-buffer)))
        (kill-buffer prev-buffer))))

  :keybind
  (:map magit-mode-map
        ("<tab>" . magit-section-cycle)
        ("C-<tab>" . magit-section-toggle)
        :states (normal visual)
        ("F" . magit-pull))
  (:map magit-blob-mode-map
        ("n" . magit-blob-next)
        ("p" . magit-blob-previous)))

(use-package git-modes)

(use-package blamer
  :commands (blamer-mode))

(use-package browse-at-remote
  :commands (browse-at-remote)
  :config
  ;; github.com.cnpmjs.org -> github.com
  (add-to-list 'browse-at-remote-remote-type-regexps '(:host "^github\\.com.*$" :type "github")))

(provide 'init-git)
;;; init-git.el ends here
