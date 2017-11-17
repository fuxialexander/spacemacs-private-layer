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
    :init
    (spacemacs/set-leader-keys "af" 'elfeed)
    (setq
     elfeed-search-title-max-width 200
     elfeed-search-title-min-width 70
     elfeed-search-trailing-width 30

          )
    :config
    (progn
      (defun elfeed-search-print-entry--default (entry)
        "Print ENTRY to the buffer."
        (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
               (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
               (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
               (feed (elfeed-entry-feed entry))
               (feed-title
                (when feed
                  (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
               (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
               (tags-str (mapconcat
                          (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                          tags "  "))
               (title-width (- (window-width) 10 elfeed-search-trailing-width))
               (title-column (elfeed-format-column
                              title (elfeed-clamp
                                     elfeed-search-title-min-width
                                     title-width
                                     elfeed-search-title-max-width)
                              :left)))
          (insert (propertize date 'face 'elfeed-search-date-face) "  ")
          (insert (propertize title-column 'face title-faces 'kbd-help title) "  ")
          (when feed-title
            (insert (propertize feed-title 'face 'elfeed-search-feed-face) "  "))

          (when tags
            (insert "  " tags-str "  "))))


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
