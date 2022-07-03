;;; init-git.el --- git stuff

;;; Commentary:
;;


;;; Code:
(use-package magit
  :commands (magit-status)
  :custom
  (magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (magit-process-popup-time 3)
  (magit-diff-refine-hunk t)
  (magit-section-visibility-indicator nil)
  :config

  ;; https://github.com/syl20bnr/spacemacs/issues/15448
  (with-eval-after-load 'evil-surround
    (add-hook 'magit-status-mode-hook #'turn-off-evil-surround-mode))

  :keybind
  (:map magit-mode-map
        ("<tab>" . magit-section-cycle)
        ("C-<tab>" . magit-section-toggle)))

(use-package git-modes)

(use-package git-commit
  :hook (git-commit-mode . goto-address-mode))

(use-package git-timemachine
  :config
  (maple-evil-map git-timemachine-mode-map)

  ;; force update evil keymaps after git-timemachine-mode loaded
  (with-eval-after-load 'evil
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))
  :keybind
  (:states normal :map git-timemachine-mode-map
           ("gg" . evil-goto-first-line)))

(use-package blamer
  :commands (blamer-mode))

(use-package browse-at-remote
  :commands (browse-at-remote)
  :config
  ;; github.com.cnpmjs.org -> github.com
  (add-to-list 'browse-at-remote-remote-type-regexps '("^github\\.com.*$" . "github")))

(use-package maple-diff
  :quelpa (:fetcher github :repo "honmaple/emacs-maple-diff")
  :hook (maple-init . global-maple-diff-mode)
  :config
  (define-fringe-bitmap 'maple-diff:added-fringe
    [24] nil nil '(center repeated))

  (define-fringe-bitmap 'maple-diff:deleted-fringe
    [24] nil nil '(center repeated))

  (define-fringe-bitmap 'maple-diff:changed-fringe
    [24] nil nil '(center repeated)))

(provide 'init-git)
;;; init-git.el ends here
