;;; packages.el --- elfeed Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq myelfeed-packages
      '(elfeed
        elfeed-org
        ))

(defun myelfeed/init-elfeed ()
  (use-package elfeed
    :defer t
    :init (spacemacs/set-leader-keys "af" 'elfeed)
    :config
    (progn
      (evilified-state-evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :eval-after-load elfeed-search
        :bindings
        "c" 'elfeed-db-compact
        "e" 'elfeed-update
        "E" 'elfeed-search-update--force
        "u" 'elfeed-unjam
        "o" 'elfeed-load-opml
        "q" 'elfeed
        )
      (evilified-state-evilify-map elfeed-show-mode-map
        :mode elfeed-show-mode
        :eval-after-load elfeed-show
        :bindings
        (kbd "q") 'elfeed
        (kbd "n") 'elfeed-show-next
        (kbd "p") 'elfeed-show-prev))))

(defun myelfeed/init-elfeed-org ()
  (use-package elfeed-org
    :defer t
    :if (boundp 'rmh-elfeed-org-files)
    :init (spacemacs|use-package-add-hook elfeed
            :pre-config (elfeed-org))))

