;;; packages.el --- mybibtex layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Xi Fu <xfu@Xis-MacBook-Pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `mybibtex-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `mybibtex/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `mybibtex/pre-init-PACKAGE' and/or
;;   `mybibtex/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(setq mybibtex-packages
      '(
        auctex
        org
        (org-ref-ivy :location local)
        (ivy-bibtex :depends ivy)
        biblio
        biblio-core
        )
      )

(defun mybibtex/post-init-auctex ()
  (with-eval-after-load 'auctex
    (spacemacs/set-leader-keys-for-major-mode 'latex-mode "ic" 'org-ref-ivy-insert-cite-link)
    )
  )

(defun mybibtex/post-init-org ()
  (with-eval-after-load 'org
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "ic" 'org-ref-ivy-insert-cite-link)
    ;; (defun org-mac-papers-open (papers-link)
    ;;   (start-process "open papers" nil "/usr/bin/open" (concat "papers3:" papers-link )))
    ;; (org-add-link-type "papers3" 'org-mac-papers-open)
    )
  )

(defun mybibtex/init-org-ref-ivy ()
  (use-package org-ref-ivy
    :commands (org-ref-bibtex-next-entry
               org-ref-bibtex-previous-entry
               org-ref-doi-utils-get-bibtex-entry-pdf
               org-ref-ivy-insert-cite-link
               org-ref-open-in-browser
               org-ref-open-bibtex-notes
               org-ref-open-bibtex-pdf
               org-ref-bibtex-hydra/body
               org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
               org-ref-sort-bibtex-entry
               arxiv-add-bibtex-entry
               arxiv-get-pdf-add-bibtex-entry
               doi-utils-add-bibtex-entry-from-doi
               isbn-to-bibtex
               pubmed-insert-bibtex-from-pmid)
    :init
    (progn
      (evil-define-key 'normal bibtex-mode-map
        (kbd "C-j") 'org-ref-bibtex-next-entry
        (kbd "C-k") 'org-ref-bibtex-previous-entry
        "gj" 'org-ref-bibtex-next-entry
        "gk" 'org-ref-bibtex-previous-entry)

      (spacemacs/set-leader-keys-for-major-mode 'bibtex-mode
        ;; Navigation
        "j" 'org-ref-bibtex-next-entry
        "k" 'org-ref-bibtex-previous-entry

        ;; Open
        "b" 'org-ref-open-in-browser
        "n" 'org-ref-open-bibtex-notes
        "p" 'org-ref-open-bibtex-pdf

        ;; Misc
        "h" 'org-ref-bibtex-hydra/body
        "i" 'org-ref-bibtex-hydra/org-ref-bibtex-new-entry/body-and-exit
        "s" 'org-ref-sort-bibtex-entry

        ;; Lookup utilities
        "la" 'arxiv-add-bibtex-entry
        "lA" 'arxiv-get-pdf-add-bibtex-entry
        "ld" 'doi-utils-add-bibtex-entry-from-doi
        "li" 'isbn-to-bibtex
        "lp" 'pubmed-insert-bibtex-from-pmid))
    :config
    (progn
      (require 'org-ref-pdf)
      ;; (setf (cdr (assoc "p" org-ref-ivy-cite-actions)) '(ivy-bibtex-open-papers "Open in Papers"))
      (setq org-ref-completion-library 'org-ref-ivy-cite
            org-ref-default-bibliography '("/Users/xfu/Dropbox/org/Bibliography.bib")
            org-ref-bibliography-notes "/Users/xfu/Dropbox/org/ref.org"
            org-ref-pdf-directory "/Users/xfu/Documents/Bookends/Attachments"
            org-ref-note-title-format "** %y - %t
 :PROPERTIES:
  :Custom_ID: %k
  :AUTHOR: %9a
  :JOURNAL: %j
  :YEAR: %y
  :VOLUME: %v
  :PAGES: %p
  :DOI: %D
  :URL: %U
 :END:
"
            ))))

(defun mybibtex/init-ivy-bibtex ()
  (use-package ivy-bibtex
    :config
    (with-eval-after-load 'ivy-bibtex
;;; bibtex-completion customize function
;;       (defun bibtex-completion-find-pdf-in-library (key-or-entry)
;;         "Searches the directories in `bibtex-completion-library-path' for a
;; PDF whose names is composed of the BibTeX key plus \".pdf\".  The
;; path of the first matching PDF is returned."
;;         (interactive)
;;         (let* ((key (if (stringp key-or-entry)
;;                         key-or-entry
;;                       (bibtex-completion-get-value "=key=" key-or-entry)))
;;                (key (s-replace ":" "" key))
;;                (path (-first 'f-file?
;;                              (--map (f-join it (s-concat key ".pdf"))
;;                                     (-flatten (list bibtex-completion-library-path))))))
;;           (when path (list path))))

      (defun bibtex-completion-open-uri (keys)
        "Open the associated URL or DOI in a browser."
        (dolist (key keys)
          (let* ((entry (bibtex-completion-get-entry key))
                 (uri (bibtex-completion-get-value "uri" entry))
                 (uri (s-replace "\\url{" "" uri))
                 (uri (s-replace "}" "" uri))
                 )
            (start-process "open papers" nil "/usr/bin/open" uri)
            )))

      (defun bibtex-completion-quicklook (keys)
        "Open the associated URL or DOI in a browser."
        (dolist (key keys)
          (let* ((entry (bibtex-completion-get-entry key))
                 (pdf (car (bibtex-completion-find-pdf entry)))
                 )
            (start-process "open papers" nil "/usr/bin/qlmanage" "-p" pdf)
            )))


;;; Ivy-bibtex customize function
      (ivy-bibtex-ivify-action bibtex-completion-open-uri ivy-bibtex-open-papers)
      (ivy-bibtex-ivify-action bibtex-completion-quicklook ivy-bibtex-quicklook)
      (setq ivy-bibtex-default-action 'ivy-bibtex-insert-key)
      (ivy-add-actions
       'ivy-bibtex '(
                     ("P" ivy-bibtex-open-papers "Open in Papers")
                     ("SPC" ivy-bibtex-quicklook "Quick look")
                     ))

      (setq bibtex-completion-format-citation-functions
            '((org-mode      . bibtex-completion-format-citation-pandoc-citeproc)
              (default       . bibtex-completion-format-citation-default))

            bibtex-completion-bibliography "/Users/xfu/Dropbox/org/Bibliography.bib"
            bibtex-completion-library-path "/Users/xfu/Documents/Bookends/Attachments"
            bibtex-completion-notes-path "/Users/xfu/Dropbox/org/ref.org"
            bibtex-completion-pdf-field "file"
            bibtex-completion-pdf-open-function (lambda (fpath) (start-process "open" "*open*" "open" fpath))
            ;; bibtex-completion-pdf-open-function 'find-file
            )

      )

    )
  )

(defun mybibtex/init-biblio ()
  )
(defun mybibtex/init-biblio-core ())

;;; packages.el ends here
