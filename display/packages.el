;;; Display Layer

(setq display-packages
      '(
        all-the-icons
        (prettify-utils :location (recipe :fetcher github :repo "Ilazki/prettify-utils.el"))
        (pretty-magit :location local)
        ))

;;; Locals
;;;; Pretty-code

;; (defun display/init-pretty-code ()
;;   (use-package pretty-code
;;     :after python
;;     :config
;;     (progn
;;       (global-prettify-symbols-mode 1)
;;       (setq python-pretty-pairs
;;             (pretty-code-get-pairs
;;              '(:lambda "lambda" :def "def"
;;                        :null "None" :true "True" :false "False"
;;                        :int "int" :float "float" :str "str" :bool "bool"
;;                        :not "not" :for "for" :in "in" :not-in "not in"
;;                        :return "return" :yield "yield"
;;                        :and "and" :or "or"
;;                        :tuple "Tuple"
;;                        :pipe "tz-pipe"
;;                        )))

;;       (pretty-code-set-pairs `(
;;                                (python-mode-hook ,python-pretty-pairs))))))

;;;; Pretty-magit

(defun display/init-pretty-magit ()
  (use-package pretty-magit
    :after magit
    :config
    (progn
      (pretty-magit "Feature" ? (:foreground "slate gray" :height 1.0 :family "FontAwesome"))
      (pretty-magit "Add"     ? (:foreground "#375E97" :height 1.0 :family "FontAwesome"))
      (pretty-magit "Fix"     ? (:foreground "#FB6542" :height 1.0 :family "FontAwesome"))
      (pretty-magit "Clean"   ? (:foreground "#FFBB00" :height 1.0 :family "FontAwesome"))
      (pretty-magit "Docs"    ? (:foreground "#3F681C" :height 1.0 :family "FontAwesome"))
      (pretty-magit "master"  ? (:height 1.0 :family "github-octicons") t)
      (pretty-magit "origin"  ? (:height 1.0 :family "github-octicons") t)
      )))

;;; Core Packages
;;;; All-the-icons

(defun display/init-all-the-icons ()
  (use-package all-the-icons
    ))

;;;; Prettify-utils

(defun display/init-prettify-utils ()
  (use-package prettify-utils))
