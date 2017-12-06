;;; Personal Layer

(setq personal-packages
      '(
        (outline-ivy :location local)
        ;; (dash-functional :location (recipe :fetcher github :repo "magnars/dash.el"))
        outshine
        pythonic
        ))

;;; Dash-functional
(defun macros/init-dash-functional ()
  (use-package dash-functional))

;;; Outline-ivy

(defun personal/init-outline-ivy ()
  (use-package outline-ivy
    :after ivy outshine
    ))

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

(defun personal/post-init-pythonic ()
  (setq anaconda-ob-ipython-interpreter python-shell-interpreter)
  (defun pythonic-tramp-connection ()
    "Tramp connection string or nil."
    (-when-let* ((vars (--filter (tramp-tramp-file-p it)
                                 (list
                                  pythonic-environment
                                  anaconda-ob-ipython-interpreter
                                  )))
                 (var (car vars)))
      (substring var 0 (- (length var) (length (pythonic-file-name var))))))
  (defun pythonic-executable ()
    "Python executable."
    (let* ((windowsp (eq system-type 'windows-nt))
           (python (if windowsp "pythonw" "python"))
           (bin (if windowsp "Scripts" "bin")))
      (if pythonic-environment
          (f-join (pythonic-file-name pythonic-environment) bin python)
        (pythonic-file-name anaconda-ob-ipython-interpreter))))
  (defun pythonic-remote-p ()
    "Determine remote or local virtual environment."
    (if pythonic-environment
        (tramp-tramp-file-p pythonic-environment)
      (tramp-tramp-file-p anaconda-ob-ipython-interpreter)))
  )

