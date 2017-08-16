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
  ;; (spacemacs/set-leader-keys-for-major-mode 'latex-mode "ic" 'org-ref-ivy-insert-cite-link)
  ;; (spacemacs/set-leader-keys-for-major-mode 'latex-mode "ib" 'ivy-bibtex)
  )

(defun mybibtex/post-init-org ()
  ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "ic" 'org-ref-ivy-insert-cite-link)
  ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "ib" 'ivy-bibtex)
  )

(defun mybibtex/init-org-ref-ivy ()
  (use-package org-ref-ivy
    :defer t
    :commands (org-ref-bibtex-next-entry
               org-ref-bibtex-previous-entry
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
        "lp" 'pubmed-insert-bibtex-from-pmid))))

(defun mybibtex/setup-org-ref-ivy ()
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  (require 'org-ref-ivy)
  )
(defun mybibtex/post-init-org-ref-ivy ()
  (add-hook 'org-mode-hook 'mybibtex/setup-org-ref-ivy))

(defun mybibtex/post-init-markdown-mode ()
  ;; (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "ic" 'org-ref-ivy-insert-cite-link)
  )

(defun mybibtex/init-ivy-bibtex ()
  (use-package ivy-bibtex
    :defer t
    )
  )
(defun mybibtex/init-biblio ())
(defun mybibtex/init-biblio-core ())

;;; packages.el ends here
