;;; Personal Layer

(setq personal-packages
      '(
        (outline-ivy :location local)
        ;; (dash-functional :location (recipe :fetcher github :repo "magnars/dash.el"))
        outshine
        ))

;;; Dash-functional
(defun macros/init-dash-functional ()
  (use-package dash-functional))

;;; Outline-ivy

(defun personal/init-outline-ivy ()
  (use-package outline-ivy
    :after ivy outshine
    :config
    (global-set-key (kbd "C-j") 'counsel-oi)))

(defun personal/init-outshine ()
  (use-package outshine
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "nn" 'outshine-narrow-to-subtree
        "nw" 'widen)
      (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
      (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
      (let ((kmap outline-minor-mode-map))
        (define-key kmap (kbd "M-RET") 'outshine-insert-heading)
        (define-key kmap (kbd "<backtab>") 'outshine-cycle-buffer)))

    :config
    (progn
      ;; Narrowing works within the headline rather than requiring to be on it
      (advice-add 'outshine-narrow-to-subtree :before
                  (lambda (&rest args) (unless (outline-on-heading-p t)
                                         (outline-previous-visible-heading 1))))
      )))
