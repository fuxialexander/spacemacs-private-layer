;;; packages.el --- langtools layer packages file for Spacemacs.
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
;; added to `langtools-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `langtools/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `langtools/pre-init-PACKAGE' and/or
;;   `langtools/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:
(setq languagetool-packages
  '((langtool :toggle (spacemacs//languagetool-detect))))

(defun languagetool/init-langtool ()
  (use-package langtool
    :defer t
    :init
    (progn
      ;; The whitespace rules give a lot of false positives when linting rich
      ;; text.
      (setq-default langtool-disabled-rules '("WHITESPACE_RULE"))
      (spacemacs/set-leader-keys
        "Sl" 'spacemacs/languagetool-toggle
        "SL" 'langtool-correct-buffer)
      (define-key evil-normal-state-map (kbd "[ a")
        'spacemacs/languagetool-previous-error)
      (define-key evil-normal-state-map (kbd "] a")
        'spacemacs/languagetool-next-error))))
