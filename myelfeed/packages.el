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
        avy
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
        (kbd "o") 'ace-link-elfeed
        (kbd "n") 'elfeed-show-next
        (kbd "p") 'elfeed-show-prev))))

(defun myelfeed/init-elfeed-org ()
  (use-package elfeed-org
    :defer t
    :if (boundp 'rmh-elfeed-org-files)
    :init (spacemacs|use-package-add-hook elfeed
            :pre-config (elfeed-org))))

(defun myelfeed/post-init-avy ()
  (use-package avy
    :ensure t
    :config
    (progn
      (defun ace-link--elfeed-collect ()
        "Collect the positions of visible links in `elfeed' buffer."
        (let (candidates pt)
          (save-excursion
            (save-restriction
              (narrow-to-region
               (window-start)
               (window-end))
              (goto-char (point-min))
              (setq pt (point))
              (while (progn (shr-next-link)
                            (> (point) pt))
                (setq pt (point))
                (when (plist-get (text-properties-at (point)) 'shr-url)
                  (push (point) candidates)))
              (nreverse candidates)))))


      (defun ace-link--elfeed-action  (pt)
        (goto-char pt)
        (shr-browse-url))

      (defun ace-link-elfeed ()
        "Open a visible link in `elfeed' buffer."
        (interactive)
        (let ((pt (avy-with ace-link-elfeed
                    (avy--process
                     (ace-link--elfeed-collect)
                     #'avy--overlay-pre))))
          (ace-link--elfeed-action pt)))
      )
    )
  )
