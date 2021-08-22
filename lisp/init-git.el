;;; init-git.el --- git stuff

;;; Commentary:
;;


;;; Code:
(use-package magit
  :commands (magit-status)
  :config
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")
        magit-process-popup-time 3
        magit-diff-refine-hunk t
        magit-section-visibility-indicator nil)

  ;; https://github.com/magit/transient/issues/18
  (use-package transient
    :config
    (transient-bind-q-to-quit)
    (setq transient-mode-line-format nil))

  :bind
  (:map magit-mode-map
        ("<tab>" . magit-section-cycle)
        ("C-<tab>" . magit-section-toggle)))

(use-package git-commit
  :hook (git-commit-mode . goto-address-mode))

(use-package git-timemachine
  :config
  (maple/evil-map git-timemachine-mode-map)
  (with-eval-after-load 'evil
    ;; force update evil keymaps after git-timemachine-mode loaded
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))
  :evil
  (normal :map git-timemachine-mode-map
          ("gg" . evil-goto-first-line)))

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

(use-package browse-at-remote
  :commands (browse-at-remote)
  :config
  ;; github.com.cnpmjs.org -> github.com
  (add-to-list 'browse-at-remote-remote-type-regexps '("^github\\.com.*$" . "github")))

(provide 'init-git)
;;; init-git.el ends here
