(provide 'pretty-outlines)

;;; Config

(defvar pretty-outline-bullets-bullet-list '("" "" "" "" "" "" "" "")
  "An implemention of `org-bullets-bullet-list' for outlines")

;; ###autoload
(defun pretty-outline-set-display-table ()
  (setf buffer-display-table pretty-outline-display-table))

;;; Outline-bullets

;;;###autoload
(defun pretty-outline--add-font-locks (FONT-LOCK-ALIST)
  "Put text property for FONT-LOCK-ALIST for var-width replacements."
  (font-lock-add-keywords
   nil (--map (-let (((rgx uni-point) it))
             `(,rgx (0 (progn
                         (put-text-property
                          (match-beginning 1) (match-end 1)
                          'display
                          ,uni-point)
                         nil))))
           FONT-LOCK-ALIST)))

;;;###autoload
(defun pretty-outline--bullets-rgx-at-level (LEVEL)
  "Calculate regex or outline-bullets at LEVEL."
  (concat "\\(^"
          (-> LEVEL
             outshine-calc-outline-string-at-level
             s-trim-right)
          "\\) "))

;;;###autoload
(defun pretty-outline--propertize-bullet (LEVEL BULLET)
  "Add LEVEL-dependent face to BULLET."
  (with-face BULLET
             (pcase LEVEL
               (0 '(:inherit outline-1 :underline nil))
               (1 '(:inherit outline-2 :underline nil))
               (2 '(:inherit outline-3 :underline nil))
               (3 '(:inherit outline-4 :underline nil))
               (4 '(:inherit outline-5 :underline nil))
               (5 '(:inherit outline-6 :underline nil))
               (6 '(:inherit outline-7 :underline nil))
               (7 '(:inherit outline-8 :underline nil))
               (_ nil))))

;;;###autoload
(defun pretty-outline-add-bullets ()
  "Use with `add-hook' to enable pretty-outline-bullets-bullet-list for mode."
  (pretty-outline--add-font-locks
   (--map-indexed
    (list
     (pretty-outline--bullets-rgx-at-level (+ 1 it-index))
     (concat
      (s-repeat it-index " ")
      (pretty-outline--propertize-bullet it-index it)))
    (-take 8 (-cycle pretty-outline-bullets-bullet-list)))))
