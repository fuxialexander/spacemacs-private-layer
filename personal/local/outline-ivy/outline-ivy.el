;; (require 'dash)
;; (require 'ivy)
;; (require 'outshine)

(provide 'outline-ivy)

;;; Config

(defvar oi-height 20
  "Number of outlines to display, overrides ivy-height.")


(defface oi-face-1 '((t :inherit org-level-1)) "Ivy outline face for level 1")
(defface oi-face-2 '((t :inherit org-level-2)) "Ivy outline face for level 2")
(defface oi-face-3 '((t :inherit org-level-3)) "Ivy outline face for level 3")
(defface oi-face-4 '((t :inherit org-level-4)) "Ivy outline face for level 4")
(defface oi-face-5 '((t :inherit org-level-5)) "Ivy outline face for level 5")
(defface oi-face-6 '((t :inherit org-level-6)) "Ivy outline face for level 6")
(defface oi-face-7 '((t :inherit org-level-7)) "Ivy outline face for level 7")
(defface oi-face-8 '((t :inherit org-level-8)) "Ivy outline face for level 8")

;;; Outline Jump

;;;###autoload
(defun oi-get-heading ()
  (save-excursion
    (outline-back-to-heading t)
    (let ((case-fold-search nil))
      (looking-at (cadar outshine-imenu-preliminary-generic-expression))
      (let (
	          (headline (pcase (match-string 1)
			                  (`nil "")
			                  (h h))))
        (mapconcat #'identity
		               (delq nil (list headline))
		               " ")
	      )))
  )


;;;###autoload
(defun counsel-oi--get-headlines ()
  "Get all headlines from the current org buffer."
  (save-excursion
    (let (entries
          start-pos
          stack
          (stack-level 0))
      (goto-char (point-min))
      (setq start-pos (or (and (outline-on-heading-p)
                               (point))
                          (outline-next-heading)))
      (while start-pos
        (let ((name (oi-get-heading))
              level)
          (search-forward " ")
          (setq level
                (- (length (buffer-substring-no-properties start-pos (point)))
                   3))
          (setq name (concat (make-string (* 2 level) ? ) (nth (- level 1) org-bullets-bullet-list) "  " name))
          (setq name (counsel-org-goto--add-face name level))
          (push (cons name (point-marker)) entries))
        (setq start-pos (outline-next-heading)))
      (nreverse entries))))
;;;###autoload

(defun counsel-oi()
  "Prompt fontified, hierarchal outlines for jump."
  (interactive)
  (let ((entries (counsel-oi--get-headlines)))
    (ivy-read "Outline: "
              entries
              :history 'counsel-oi-history
              :action (-lambda ((_ . marker))
                        (with-ivy-window
                          (-> marker marker-position goto-char)
                          (recenter 2))))))

;;; Projectile Integration

;; Not working yet, in progress

;; (defun oi--use-file? (FILE)
;;   "Add outlines in FILE to current prompt?"
;;   (let ((exts '("el")))
;;     (or
;;      (member (file-name-extension FILE) exts)
;;      (string= FILE ".spacemacs"))))

;; (defun oi-projectile-files ()
;;   "Collect files for oi-projectile-jump."
;;   (with-dir (projectile-project-root)
;;             (-filter 'oi--use-file?
;;                      (projectile-get-repo-files))))

;; (defun oi-projectile-collect-outlines ()
;;   "Collect all outlines in repo files."
;;   ;; (--mapcat
;;   ;;  (with-temp-buffer
;;   ;;    (setq oi--current-buffer it)
;;   ;;    (insert-file-contents it)
;;   ;;    (emacs-lisp-mode)
;;   ;;    (oi-collect-outlines)))
;;   ;;  (oi-projectile-files))
;;   (save-window-excursion
;;       (--mapcat
;;        (progn
;;          (find-file it)
;;          (oi-collect-outlines))
;;        (oi-projectile-files))))

;; (defun oi-projectile-jump ()
;;   (interactive)
;;   (let ((ivy-height oi-height))
;;     (ivy-read "Outline " (oi-projectile-collect-outlines)
;;               :preselect (oi--preselect)
;;               :update-fn 'oi--remap-ivy-match-face
;;               :action (-lambda ((_ . marker))
;;                         (with-ivy-window
;;                           (-> marker marker-buffer switch-to-buffer)
;;                           (goto-char marker)
;;                           (recenter 2))))))

;; (oi-projectile-jump)
