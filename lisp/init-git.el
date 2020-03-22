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

  (use-package evil-magit
    :demand)
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
  :evil-bind
  (:state normal :map git-timemachine-mode-map
          ("gg" . evil-goto-first-line)))

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :hook (maple-init . global-git-gutter-mode)
  :init
  (with-eval-after-load 'git-gutter
    (require 'git-gutter-fringe))
  (setq git-gutter-fr:side 'right-fringe)
  :config
  ;; (setq-default fringes-outside-margins t)
  ;; custom graphics that works nice with half-width fringes
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XX")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XX")
  (fringe-helper-define 'git-gutter-fr:deleted '(center repeated)
    "XX"))

(use-package browse-at-remote
  :commands (browse-at-remote))

(provide 'init-git)
;;; init-git.el ends here
