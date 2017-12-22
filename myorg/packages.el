;;; packages.el --- Org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq myorg-packages
      '(
        company
        org-bookmark-heading
        org-web-tools
        evil-org
        evil-surround
        htmlize
        ;; ob, org and org-agenda are installed by `org-plus-contrib'
        (ob :location built-in)
        (org :location built-in)
        (org-agenda :location built-in)
        (org-brain :toggle (version<= "25" emacs-version))
        (org-expiry :location built-in)
        (org-journal :toggle org-enable-org-journal-support)
        org-download
        ;; org-mime is installed by `org-plus-contrib'
        org-mime
        ;; (ob-ipython :location local)
        ob-ipython
        org-pomodoro
        org-present
        org-super-agenda
        (org-projectile :requires projectile)
        (ox-twbs :toggle org-enable-bootstrap-support)
        (ox-reveal :toggle org-enable-reveal-js-support)
        (ox-hugo :toggle org-enable-hugo-support)))

(defun myorg/post-init-company ()
  ;; (require 'predictive nil t)
  ;; (load "dict-english")
  ;; (defvar company-predictive-syntax-table
  ;;   (let ((tbl (copy-syntax-table (standard-syntax-table))))
  ;;     (modify-syntax-entry ?\- "w" tbl)
  ;;     tbl)
  ;;   "Make character `-' have syntax word.")
  ;; (defun company-predictive-grab-word ()
  ;;   (with-syntax-table company-predictive-syntax-table
  ;;     (company-grab-word)))
  ;; (defun company-predictive (command &optional arg &rest ignored)
  ;;   "A predictive-like `company-mode' completion back-end."
  ;;   (interactive (list 'interactive))
  ;;   (case command
  ;;     ('interactive (company-begin-backend 'company-predictive))
  ;;     ('prefix (company-predictive-grab-word))
  ;;     ('candidates (predictive-complete arg))
  ;;     ('ignore-case t)
  ;;     ('sorted nil)
  ;;     ('duplicates t)))
  (spacemacs|add-company-backends :backends company-capf :modes org-mode)
  ;; (spacemacs|add-company-backends :backends company-predictive :modes org-mode)
  )


;;;; Org-bookmark-heading
(defun myorg/init-org-bookmark-heading ()
  (use-package org-bookmark-heading
    :after org
    :config (setq org-bookmark-jump-indirect t)
    ))

;;;; Org-web-tools
(defun myorg/init-org-web-tools ()
  (use-package org-web-tools
    ))


(defun myorg/init-org-super-agenda ()
  (use-package org-super-agenda
    :init
    :config
    (setq
     org-super-agenda-groups
     '((:name "Log "
              :log t)  ; Automatically named "Log"
       (:name "Schedule "
              :time-grid t)
       (:name "Today "
              :scheduled today)
       (:name "Habits "
              :habit t)
       (:name "Due today "
              :deadline today)
       (:name "Overdue "
              :deadline past)
       (:name "Due soon "
              :deadline future)
       ;; (:name "Unimportant" :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH") :order 100)
       (:name "Waiting "
              :todo "WAIT"
              :order 98)
       (:name "Scheduled earlier "
              :scheduled past))
     )
    (org-super-agenda-mode)
    ))

(defun myorg/init-evil-org ()
  (use-package evil-org
    :defer t
    :init
    (progn
      (add-hook 'org-mode-hook 'spacemacs//evil-org-mode)
      (setq evil-org-key-theme `(
                                 textobjects
                                 navigation
                                 insert
                                 additional
                                 todo
                                 leader
                                 )))
    :config
    (spacemacs|diminish evil-org-mode " ⓔ" " e")))
(defun myorg/init-ob-ipython ()
  (use-package ob-ipython
    :defer t
    :init
    :config
    (add-hook 'org-src-mode-hook 'ob-ipython-mode)
    ;; (spacemacs|add-company-backends :backends company-ob-ipython :modes org-src-mode)

    (define-key org-src-mode-map (kbd "H-p") 'company-ob-ipython)
    (define-key ob-ipython-mode-map (kbd "<f1>") 'ob-ipython-inspect)
    ;; (add-to-list 'company-backends 'company-ob-ipython)
    ))

(defun myorg/post-init-evil-surround ()
  (defun spacemacs/add-org-surrounds ()
    (push '(?: . spacemacs//surround-drawer) evil-surround-pairs-alist)
    (push '(?# . spacemacs//surround-code) evil-surround-pairs-alist))
  (add-hook 'org-mode-hook 'spacemacs/add-org-surrounds))

(defun myorg/init-htmlize ()
  (use-package htmlize
    :defer t))

(defun myorg/init-ob ()
  (use-package ob
    :defer t
    :init
    (progn
      (defun spacemacs//org-babel-do-load-languages ()
        "Load all the languages declared in `org-babel-load-languages'."
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages))
      (add-hook 'org-mode-hook 'spacemacs//org-babel-do-load-languages)
      ;; Fix redisplay of inline images after a code block evaluation.
      (add-hook 'org-babel-after-execute-hook 'spacemacs/ob-fix-inline-images))))

(defun myorg/init-org ()
  (use-package org
    :defer t
    :commands (orgtbl-mode)
    :init
    (progn

      (defface org-todo-keyword-todo '((t ())) "org-todo" :group 'org)
      (defface org-todo-keyword-kill '((t ())) "org-kill" :group 'org)
      (defface org-todo-keyword-outd '((t ())) "org-outd" :group 'org)
      (defface org-todo-keyword-wait '((t ())) "org-wait" :group 'org)
      (defface org-todo-keyword-done '((t ())) "org-done" :group 'org)
      (defface org-todo-keyword-habt '((t ())) "org-habt" :group 'org)


      (setq org-clock-persist-file (concat spacemacs-cache-directory "org-clock-save.el")
            org-id-locations-file (concat spacemacs-cache-directory ".org-id-locations")
            org-publish-timestamp-directory (concat spacemacs-cache-directory ".org-timestamps/")
            org-startup-with-inline-images nil
            org-blank-before-new-entry nil
            org-fontify-whole-heading-line t
            org-fontify-done-headline t
            org-fontify-quote-and-verse-blocks t
            org-ellipsis " + "
            org-columns-ellipses " + "
            org-bullets-bullet-list '("" "" "" "" "" "" "" "")
            org-src-fontify-natively t
            org-preview-latex-default-process 'dvisvgm
            org-highlight-latex-and-related '(latex)
            org-modules
            (quote
             (org-bibtex org-docview org-habit org-info org-protocol org-mac-iCal org-mac-link org-notmuch))
            org-imenu-depth 8)
;;;;; Org-ID
      (defun my-org-add-ids-to-headlines-in-file ()
        "Add CUSTOM_ID properties to all headlines in the current file"
        (interactive)
        (save-excursion
          (widen)
          (goto-char (point-min))
          (org-map-entries 'org-id-get-create)))

      (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)
      (add-hook 'org-mode-hook
                (lambda ()
                  (add-hook 'before-save-hook 'my-org-add-ids-to-headlines-in-file nil 'local)))

;;;;; Org-TODO
      (setq org-directory "/Users/xfu/Dropbox/org/"
            org-capture-templates (quote (
                                          ("SA" "Skim Annotation" entry
                                           (file+function org-ref-bibliography-notes my-org-move-point-to-capture-skim-annotation)
                                           "* %^{Logging for...} :skim_annotation:read:literature:
:PROPERTIES:
:Created: %U
:CITE: cite:%(my-as-get-skim-bibtex-key)
:SKIM_NOTE: %(my-org-mac-skim-get-page)
:SKIM_PAGE: %(int-to-string (my-as-get-skim-page))
:END:
%i
%?" :prepend f :empty-lines 1)
                                          ("t" "Todo" entry
                                           (file "~/Dropbox/org/inbox.org")
                                           "* %^{Logging for...}
:PROPERTIES:
:Created: %U
:END:
%i
%?" :prepend f :empty-lines 2 :created t)
                                          ("ic" "Idea from Chrome" entry
                                           (file "~/Dropbox/org/idea.org")
                                           "* %^{Logging for...} :idea:
:PROPERTIES:
:Created: %U
:Linked: %(org-as-mac-chrome-get-frontmost-url)
:END:
%i
%?" :prepend f :empty-lines 2 :created t)
                                          ("j" "Journal" entry
                                           (function my-org-move-point-to-capture)
                                           "* %^{Logging for...} :logs:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?" :empty-lines 2 :clock-in t :created t)
                                          ("M" "Meeting" entry
                                           (file+olp+datetree "~/Dropbox/org/meeting.org")
                                           "* %^{Logging for...} :logs:communication:
%^{Effort}p
%^T

- Things to discuss:

%i
%?" :empty-lines 2 :clock-in t :created t)
                                          ("m" "Meeting Minutes" entry
                                           (function my-org-move-point-to-capture)
                                           "* %^{Logging for...} :logs:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?" :empty-lines 2 :clock-in t :created t)
                                          ("u" "Write-up" entry
                                           (function my-org-move-point-to-capture)
                                           "* %^{Logging for...} :writeup:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?" :empty-lines 2 :clock-in t :created t)
                                          ("a" "Article" entry
                                           (file "~/Dropbox/org/ref.org")
                                           "* %^{Title}  :article:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
Brief description:
%?" :prepend f :empty-lines 2 :created t)
                                          ("i" "Idea" entry
                                           (file "~/Dropbox/org/idea.org")
                                           "* %A :idea:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?" :prepend f :empty-lines 2 :created t)
                                          ("dr" "Daily Review" entry
                                           (file+olp+datetree "~/Dropbox/org/review.org")
                                           "* %^{Review} :review:daily:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%?" :created t )
                                          ("wr" "Week Review" entry
                                           (file+olp+datetree "~/Dropbox/org/review.org")
                                           "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:week:%\\1:

:PROPERTIES:
:Created: %U
:END:
%?" :created t )
                                          ("mr" "Month Review" entry
                                           (file+olp+datetree "~/Dropbox/org/review.org")
                                           "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:month:%\\1:

:PROPERTIES:
:Created: %U
:END:
%?" :created t )
                                          ("W" "Web site" entry
                                           (file "~/Dropbox/org/inbox.org")
                                           "* %A :website:
:PROPERTIES:
:Created: %U
:Linked: %a
:END:
%i
%?" :prepend f :empty-lines 2 :created t)
                                          ))
            diary-file "/Users/xfu/Dropbox/org/cal.diary"
            org-default-notes-file "/Users/xfu/Dropbox/org/inbox.org"
            org-log-done 'time
            org-log-note-clock-out t
            org-log-redeadline 'note
            org-log-reschedule 'note
            org-enforce-todo-dependencies t
            org-habit-graph-column 1
            org-habit-following-days 0
            org-habit-preceding-days 8
            org-habit-show-habits t
            Org-hide-block-startup t
            org-tags-column 0
            org-tag-persistent-alist '(
                                       (:startgrouptag)
                                       ("Mood")
                                       (:grouptags)
                                       ("happy")
                                       ("sad")
                                       ("pity")
                                       ("excited")
                                       ("bored")
                                       ("tired")
                                       ("giveup")
                                       ("eager")
                                       ("hope")
                                       ("warm")
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("People")
                                       (:grouptags)
                                       ("KevinGroup")
                                       ("hscr")
                                       ("Friends")
                                       ("Family")
                                       ("Office")
                                       ("Acquaintances")
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("hscr")
                                       (:grouptags)
                                       ("merce")
                                       ("pak")
                                       ("clara")
                                       ("elly")
                                       ("kathy")
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("KevinGroup")
                                       (:grouptags)
                                       ("kevin")
                                       ("qin")
                                       ("danny")
                                       ("qiong")
                                       ("christina")
                                       ("yong")
                                       ("chengyang")
                                       ("kelly")
                                       ("le")
                                       ("saudan")
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("Field")
                                       (:grouptags)
                                       ("bioinfo")
                                       ("biomed")
                                       ("statistics")
                                       ("cs")
                                       ("math")
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("bioinfo")
                                       (:grouptags)
                                       ("genomics")
                                       ("tools")
                                       ("dataviz")
                                       ("epigenomics")
                                       ("genome_arch")
                                       ("gene_regulation")
                                       ("enhancer")
                                       ("promoter")
                                       ("alignment")
                                       ("assembly")
                                       ("sequencing")
                                       ("mrpa")
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("cs")
                                       (:grouptags)
                                       ("network")
                                       ("deep_learning")
                                       ("machine_learning")
                                       ("algorithm")
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("biomed")
                                       (:grouptags)
                                       ("development")
                                       ("cellbio")
                                       ("crispr")
                                       ("disease")
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("statistics")
                                       (:grouptags)
                                       ("hypo_test")
                                       ("bayesian")
                                       ("linear_model")
                                       ("penalized_regression")
                                       ("significance")
                                       ("fdr")
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("math")
                                       (:grouptags)
                                       ("linear_algebra")
                                       ("calculus")
                                       ("fdr")
                                       ("optimization")
                                       (:endgrouptag)

                                       (:startgrouptag)
                                       ("Research")
                                       (:grouptags)
                                       ("hic")
                                       ("hscr")
                                       ("search")
                                       ("read")
                                       ("think")
                                       ("literature")
                                       ("website")
                                       ("learn")
                                       ("coding")
                                       ("analysis")
                                       ("organize")
                                       ("plot")
                                       ("discuss")
                                       ("talk")
                                       (:endgrouptag)


                                       )
            org-agenda-tags-column 'auto
            org-agenda-restore-windows-after-quit t
            org-agenda-span 'day
            org-agenda-files '("/Users/xfu/Dropbox/org/")
            org-refile-targets '((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))
            org-refile-use-outline-path 'file
            org-outline-path-complete-in-steps nil
            org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA"
            org-global-properties (quote (("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
            org-use-fast-todo-selection t
            org-use-fast-tag-selection nil
            org-treat-insert-todo-heading-as-state-change t
            org-log-into-drawer t
            org-log-state-notes-into-drawer t
            org-clock-clocktable-default-properties (quote (:maxlevel 3 :scope agenda :tags "-COMMENT"))
            org-clocktable-defaults
            (quote
             (:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 t :fileskip0 t :tags "-COMMENT" :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil))
            org-todo-keywords '(
                                (sequence "TODO(t!)"  "|" "DONE(d!/@)")
                                (sequence "WAIT(w@/@)" "|" "OUTD(o@/@)" "KILL(k@/@)")
                                (sequence "HABT(h!)" "|" "DONE(d!/@)" "KILL(k@/@)")
                                )
            org-todo-keyword-faces '(
                                     ("TODO" . org-todo-keyword-todo)
                                     ("HABT" . org-todo-keyword-habt)
                                     ("DONE" . org-todo-keyword-done)
                                     ("WAIT" . org-todo-keyword-wait)
                                     ("KILL" . org-todo-keyword-kill)
                                     ("OUTD" . org-todo-keyword-outd)
                                     )
            )
;;;;; Org-latex
      (defun my-buffer-face-mode-orgwrite ()
        "Sets a fixed width (monospace) font in current buffer"
        (interactive)
        (setq buffer-face-mode-face '(:family "charter" :height 1.2)
              line-spacing 0.4)
        (buffer-face-mode))

      (setq
       org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"))
       org-latex-create-formula-image-program 'dvisvgm
       org-latex-packages-alist (quote (("" "color" t)
                                        ("" "minted" t)
                                        ("" "parskip" t)
                                        ("" "tikz" t)))
       )


      (with-eval-after-load 'org-mac-link
        (setq org-mac-Skim-highlight-selection-p t)
        (defun as-get-skim-page-link ()
          (do-applescript
           (concat
            "tell application \"Skim\"\n"
            "set theDoc to front document\n"
            "set theTitle to (name of theDoc)\n"
            "set thePath to (path of theDoc)\n"
            "set thePage to (get index for current page of theDoc)\n"
            "set theSelection to selection of theDoc\n"
            "set theContent to contents of (get text for theSelection)\n"
            "if theContent is missing value then\n"
            "    set theContent to theTitle & \", p. \" & thePage\n"
            (when org-mac-Skim-highlight-selection-p
              (concat
               "else\n"
               "    tell theDoc\n"
               "        set theNote to make note with data theSelection with properties {type:highlight note}\n"
               "         set text of theNote to (get text for theSelection)\n"
               "    end tell\n"))
            "end if\n"
            "set theLink to \"skim://\" & thePath & \"::\" & thePage & "
            "\"::split::\" & theContent\n"
            "end tell\n"
            "return theLink as string\n")))
        )
      (with-eval-after-load 'org-indent
        (spacemacs|hide-lighter org-indent-mode))
      (let ((dir (configuration-layer/get-layer-local-dir 'org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))
      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      ;; Follow the confirm and abort conventions
      (with-eval-after-load 'org-capture
        (spacemacs/set-leader-keys-for-minor-mode 'org-capture-mode
          dotspacemacs-major-mode-leader-key 'org-capture-finalize
          "a" 'org-capture-kill
          "c" 'org-capture-finalize
          "k" 'org-capture-kill
          "r" 'org-capture-refile))

      (with-eval-after-load 'org-src
        (spacemacs/set-leader-keys-for-minor-mode 'org-src-mode
          dotspacemacs-major-mode-leader-key 'org-edit-src-exit
          "c" 'org-edit-src-exit
          "a" 'org-edit-src-abort
          "k" 'org-edit-src-abort))

      (add-hook 'org-mode-hook 'dotspacemacs//prettify-spacemacs-docs)

      (let ((dir (configuration-layer/get-layer-local-dir 'org)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))
      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))


      ;; Insert key for org-mode and markdown a la C-h k
      ;; from SE endless http://emacs.stackexchange.com/questions/2206/i-want-to-have-the-kbd-tags-for-my-blog-written-in-org-mode/2208#2208
      (defun spacemacs/insert-keybinding-org (key)
        "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
        (interactive "kType key sequence: ")
        (let* ((tag "@@html:<kbd>@@ %s @@html:</kbd>@@"))
          (if (null (equal key "\r"))
              (insert
               (format tag (help-key-description key nil)))
            (insert (format tag ""))
            (forward-char -8))))

      (dolist (prefix '(
                        ("mC" . "clocks")
                        ("md" . "dates")
                        ("me" . "export")
                        ("mh" . "headings")
                        ("mi" . "insert")
                        ("miD" . "download")
                        ("ms" . "trees/subtrees")
                        ("mT" . "toggles")
                        ("mt" . "tables")
                        ("mtd" . "delete")
                        ("mti" . "insert")
                        ("mtt" . "toggle")
                        ("mx" . "text")
                        ))
        (spacemacs/declare-prefix-for-mode 'org-mode (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "'"     'org-edit-special
        "c"     'org-capture
        "Cc"    'org-clock-cancel
        "Ci"    'org-clock-in
        "Co"    'org-clock-out
        "Cr"    'org-resolve-clocks
        "dd"    'org-deadline
        "ds"    'org-schedule
        "dt"    'org-time-stamp
        "dT"    'org-time-stamp-inactive
        "ee"    'org-export-dispatch

        "a"     'org-agenda
        "n" 'org-modify-and-clock-current-heading

        "tt"    'org-show-todo-tree
        "ti"    'org-toggle-inline-images
        "td"    'space-doc-mode
        "te"    'org-toggle-latex-fragment

        ;; More cycling options (timestamps, headlines, items, properties)
        "L"     'org-shiftright
        "H"     'org-shiftleft
        "J"     'org-shiftdown
        "K"     'org-shiftup

        ;; Change between TODO sets
        "C-S-l" 'org-shiftcontrolright
        "C-S-h" 'org-shiftcontrolleft
        "C-S-j" 'org-shiftcontroldown
        "C-S-k" 'org-shiftcontrolup

        ;; Subtree editing
        "sa"    'org-archive-subtree
        "sc" 'org-copy
        "sb"    'org-tree-to-indirect-buffer
        "sh"    'org-promote-subtree
        "sj"    'org-move-subtree-down
        "sk"    'org-move-subtree-up
        "sl"    'org-demote-subtree
        "sn"    'org-narrow-to-subtree
        "sN"    'widen
        "sr"    'org-refile
        "ss"    'org-sparse-tree
        "sS"    'org-sort

        ;; tables
        "Ta"    'org-table-align
        "Tb"    'org-table-blank-field
        "Tc"    'org-table-convert
        "Tdc"   'org-table-delete-column
        "Tdr"   'org-table-kill-row
        "Te"    'org-table-eval-formula
        "TE"    'org-table-export
        "Th"    'org-table-previous-field
        "TH"    'org-table-move-column-left
        "Tic"   'org-table-insert-column
        "Tih"   'org-table-insert-hline
        "TiH"   'org-table-hline-and-move
        "Tir"   'org-table-insert-row
        "TI"    'org-table-import
        "Tj"    'org-table-next-row
        "TJ"    'org-table-move-row-down
        "TK"    'org-table-move-row-up
        "Tl"    'org-table-next-field
        "TL"    'org-table-move-column-right
        "Tn"    'org-table-create
        "TN"    'org-table-create-with-table.el
        "Tr"    'org-table-recalculate
        "Ts"    'org-table-sort-lines
        "Ttf"   'org-table-toggle-formula-debugger
        "Tto"   'org-table-toggle-coordinate-overlays
        "Tw"    'org-table-wrap-region

        ;; Multi-purpose keys
        (or dotspacemacs-major-mode-leader-key ",") 'org-ctrl-c-ctrl-c
        "*"   'org-ctrl-c-star
        "RET" 'org-ctrl-c-ret
        "-"   'org-ctrl-c-minus
        "#"   'org-update-statistics-cookies
        ;; attachments
        "A"   'org-attach
        ;; insertion
        "id"  'org-insert-drawer
        "ie"  'org-set-effort
        "if"  'org-footnote-new
        "ih"  'org-insert-heading
        "iH"  'org-insert-heading-after-current
        "iK"  'spacemacs/insert-keybinding-org
        "il"  'org-insert-link
        "ip"  'org-set-property
        "is"  'org-insert-subheading
        "it"  'org-set-tags
        ;; region manipulation
        "xb" (spacemacs|org-emphasize spacemacs/org-bold ?*)
        "xc" (spacemacs|org-emphasize spacemacs/org-code ?~)
        "xi" (spacemacs|org-emphasize spacemacs/org-italic ?/)
        "xo" 'org-open-at-point
        "xr" (spacemacs|org-emphasize spacemacs/org-clear ? )
        "xs" (spacemacs|org-emphasize spacemacs/org-strike-through ?+)
        "xu" (spacemacs|org-emphasize spacemacs/org-underline ?_)
        "xv" (spacemacs|org-emphasize spacemacs/org-verbatim ?=))

      ;; Add global evil-leader mappings. Used to access org-agenda
      ;; functionalities – and a few others commands – from any other mode.
      (spacemacs/declare-prefix "ao" "org")
      (spacemacs/declare-prefix "aok" "clock")
      (spacemacs/set-leader-keys
        ;; org-agenda
        "ao#"  'org-agenda-list-stuck-projects
        "ao/"  'org-occur-in-agenda-files
        "aoa"  'org-agenda-list
        "aoc"  'org-capture
        "aoe"  'org-store-agenda-views
        "aoki" 'org-clock-in-last
        "aokj" 'org-clock-jump-to-current-clock
        "aoko" 'org-clock-out
        "aokr" 'org-resolve-clocks
        "aol"  'org-store-link
        "aom"  'org-tags-view
        "aoo"  'org-agenda
        "aos"  'org-search-view
        "aot"  'org-todo-list
        ;; SPC C- capture/colors
        "Cc"   'org-capture)

      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (define-key global-map "\C-cc" 'org-capture))
    :config
    (progn
;;; Org-subtree
      (defun org-advance ()
        (interactive)
        (when (buffer-narrowed-p)
          (beginning-of-buffer)
          (widen)
          (org-forward-heading-same-level 1))
        (org-narrow-to-subtree))

      (defun org-retreat ()
        (interactive)
        (when (buffer-narrowed-p)
          (beginning-of-buffer)
          (widen)
          (org-backward-heading-same-level 1))
        (org-narrow-to-subtree))
;;; Org-todo
      (defun my-org-after-todo-state-change-action ()
        (pcase org-state
          ("WAIT" (org-schedule '(4)))
          ("TODO" (org-schedule '(0)))
          ('nil (org-schedule '(4)) (org-deadline '(4)))))
      (add-hook 'org-after-todo-state-change-hook #'my-org-after-todo-state-change-action)

;;; Org-pandoc
      (setq org-pandoc-options '(
                                 (standalone . t)
                                 (self-contained . t)
                                 ))

      (setq org-plantuml-jar-path "~/Source/plantuml.jar")
;;; Org-babel
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (ipython . t)
         (plantuml . t)
         ;; (ein . t)
         (python . t)
         (applescript . t)
         (R . t)
         (shell . t)
         (org . t)
         (latex . t)))


;;; Org-refile-get-targets
      (defun org-refile-get-targets (&optional default-buffer)
        "Produce a table with refile targets."
        (let ((case-fold-search nil)
              ;; otherwise org confuses "TODO" as a kw and "Todo" as a word
              (entries (or org-refile-targets '((nil . (:level . 1)))))
              targets tgs files desc descre)
          (message "Getting targets...")
          (with-current-buffer (or default-buffer (current-buffer))
            (dolist (entry entries)
              (setq files (car entry) desc (cdr entry))
              (cond
               ((null files) (setq files (list (current-buffer))))
               ((eq files 'org-agenda-files)
                (setq files (org-agenda-files 'unrestricted)))
               ((and (symbolp files) (fboundp files))
                (setq files (funcall files)))
               ((and (symbolp files) (boundp files))
                (setq files (symbol-value files))))
              (when (stringp files) (setq files (list files)))
              (cond
               ((eq (car desc) :tag)
                (setq descre (concat "^\\*+[ \t]+.*?:" (regexp-quote (cdr desc)) ":")))
               ((eq (car desc) :todo)
                (setq descre (concat "^\\*+[ \t]+" (regexp-quote (cdr desc)) "[ \t]")))
               ((eq (car desc) :regexp)
                (setq descre (cdr desc)))
               ((eq (car desc) :level)
                (setq descre (concat "^\\*\\{" (number-to-string
                                                (if org-odd-levels-only
                                                    (1- (* 2 (cdr desc)))
                                                  (cdr desc)))
                                     "\\}[ \t]")))
               ((eq (car desc) :maxlevel)
                (setq descre (concat "^\\*\\{1," (number-to-string
                                                  (if org-odd-levels-only
                                                      (1- (* 2 (cdr desc)))
                                                    (cdr desc)))
                                     "\\}[ \t]")))
               (t (error "Bad refiling target description %s" desc)))
              (dolist (f files)
                (with-current-buffer (if (bufferp f) f (org-get-agenda-file-buffer f))
                  (or
                   (setq tgs (org-refile-cache-get (buffer-file-name) descre))
                   (progn
                     (when (bufferp f)
                       (setq f (buffer-file-name (buffer-base-buffer f))))
                     (setq f (and f (expand-file-name f)))
                     (when (eq org-refile-use-outline-path 'file)
                       (push (list (file-name-nondirectory f) f nil nil) tgs))
                     (org-with-wide-buffer
                      (goto-char (point-min))
                      (setq org-outline-path-cache nil)
                      (while (re-search-forward descre nil t)
                        (beginning-of-line)
                        (let ((case-fold-search nil))
                          (looking-at org-complex-heading-regexp))
                        (let ((begin (point))
                              (heading (match-string-no-properties 4)))
                          (unless (or (and
                                       org-refile-target-verify-function
                                       (not
                                        (funcall org-refile-target-verify-function)))
                                      (not heading))
                            (let ((re (format org-complex-heading-regexp-format
                                              (regexp-quote heading)))
                                  (target
                                   (if (not org-refile-use-outline-path) heading
                                     (concat
                                      (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))
                                      " ✦ "
                                      (org-format-outline-path (org-get-outline-path t t) 1000 nil " ➜ ")
                                      ))))

                              (push (list target f re (org-refile-marker (point)))
                                    tgs)))
                          (when (= (point) begin)
                            ;; Verification function has not moved point.
                            (end-of-line)))))))
                  (when org-refile-use-cache
                    (org-refile-cache-put tgs (buffer-file-name) descre))
                  (setq targets (append tgs targets))))))
          (message "Getting targets...done")
          (delete-dups (nreverse targets))))

;;; Org-edit-src-code
      (defun org-edit-src-code (&optional code edit-buffer-name)
        "Edit the source or example block at point.
\\<org-src-mode-map>
The code is copied to a separate buffer and the appropriate mode
is turned on.  When done, exit with `\\[org-edit-src-exit]'.  This \
will remove the
original code in the Org buffer, and replace it with the edited
version.  See `org-src-window-setup' to configure the display of
windows containing the Org buffer and the code buffer.

When optional argument CODE is a string, edit it in a dedicated
buffer instead.

When optional argument EDIT-BUFFER-NAME is non-nil, use it as the
name of the sub-editing buffer."
        (interactive)
        (let* ((element (org-element-at-point))
               (type (org-element-type element)))
          (unless (and (memq type '(example-block src-block))
                       (org-src--on-datum-p element))
            (user-error "Not in a source or example block"))
          (let* ((lang
                  (if (eq type 'src-block) (org-element-property :language element)
                    "example"))
                 (lang-f (and (eq type 'src-block) (org-src--get-lang-mode lang)))
                 (babel-info (and (eq type 'src-block)
                                  (org-babel-get-src-block-info 'light)))
                 deactivate-mark)
            (when (and (eq type 'src-block) (not (functionp lang-f)))
              (error "No such language mode: %s" lang-f))
            (org-src--edit-element
             element
             (or edit-buffer-name
                 (org-src--construct-edit-buffer-name (buffer-name) lang))
             lang-f
             (and (null code)
                  (lambda () (org-escape-code-in-region (point-min) (point-max))))
             (and code (org-unescape-code-in-string code)))
            ;; Finalize buffer.
            (setq-local org-coderef-label-format
                        (or (org-element-property :label-fmt element)
                            org-coderef-label-format))
            (when (eq type 'src-block)
              (setq-local org-src--babel-info babel-info)
              (setq-local params (nth 2 babel-info))
              (setq-local dir (cdr (assq :dir params)))
              (if (bound-and-true-p dir)
                  (cd (file-name-as-directory (expand-file-name dir)))
                )
              (let ((edit-prep-func (intern (concat "org-babel-edit-prep:" lang))))
                (when (fboundp edit-prep-func)
                  (funcall edit-prep-func babel-info))))
            t)))
;;; Org-mode fast-todo-selection
      (defun org-fast-todo-selection ()
        "Fast TODO keyword selection with single keys.
Returns the new TODO keyword, or nil if no state change should occur."
        (let* ((fulltable org-todo-key-alist)
               (done-keywords org-done-keywords) ;; needed for the faces.
               (maxlen (apply 'max (mapcar
                                    (lambda (x)
                                      (if (stringp (car x)) (string-width (car x)) 0))
                                    fulltable)))
               (expert t)
               (fwidth (+ maxlen 3 1 3))
               (ncol (/ (- (window-width) 4) fwidth))
               tg cnt e c tbl
               groups ingroup)
          (save-excursion
            (save-window-excursion
              (if expert
                  (set-buffer (get-buffer-create " *Org todo*"))
                (org-switch-to-buffer-other-window (get-buffer-create " *Org todo*")))
              (erase-buffer)
              (setq-local org-done-keywords done-keywords)
              (setq tbl fulltable cnt 0)
              (while (setq e (pop tbl))
                (cond
                 ((equal e '(:startgroup))
                  (push '() groups) (setq ingroup t)
                  (unless (= cnt 0)
                    (setq cnt 0)
                    (insert "\n"))
                  (insert "{ "))
                 ((equal e '(:endgroup))
                  (setq ingroup nil cnt 0)
                  (insert "}\n"))
                 ((equal e '(:newline))
                  (unless (= cnt 0)
                    (setq cnt 0)
                    (insert "\n")
                    (setq e (car tbl))
                    (while (equal (car tbl) '(:newline))
                      (insert "\n")
                      (setq tbl (cdr tbl)))))
                 (t
                  (setq tg (car e) c (cdr e))
                  (when ingroup (push tg (car groups)))
                  (setq tg (org-add-props tg nil 'face
                                          (org-get-todo-face tg)))
                  (when (and (= cnt 0) (not ingroup)) (insert "  "))
                  (insert "[" c "] " tg (make-string
                                         (- fwidth 4 (length tg)) ?\ ))
                  (when (= (setq cnt (1+ cnt)) ncol)
                    (insert "\n")
                    (when ingroup (insert "  "))
                    (setq cnt 0)))))
              (insert "\n")
              (goto-char (point-min))
              (unless expert (org-fit-window-to-buffer))
              (message "[T]ODO | [D]ONE | [W]AIT | [K]ILL | [O]UTD | [SPC]:Clear")
              (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
              (cond
               ((or (= c ?\C-g)
                    (and (= c ?q) (not (rassoc c fulltable))))
                (setq quit-flag t))
               ((= c ?\ ) nil)
               ((setq e (rassoc c fulltable) tg (car e))
                tg)
               (t (setq quit-flag t)))))))

;;; Org-mode clock
      (defun org-modify-and-clock-current-heading ()
        (interactive)
        (org-narrow-to-subtree)
        (org-clock-in)
        )
;;; Org-latex-fragment
      (defvar kk/org-latex-fragment-last nil
        "Holds last fragment/environment you were on.")

      (defun kk/org-in-latex-fragment-p ()
        "Return the point where the latex fragment begins, if inside
  a latex fragment. Else return false"
        (let* ((el (org-element-context))
               (el-type (car el)))
          (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
               (org-element-property :begin el))))

      (defun kk/org-latex-fragment-toggle ()
        "Toggle a latex fragment image "
        (and (eq 'org-mode major-mode)
             (let ((begin (kk/org-in-latex-fragment-p)))
               (cond
                ;; were on a fragment and now on a new fragment
                ((and
                  ;; fragment we were on
                  kk/org-latex-fragment-last
                  ;; and are on a fragment now
                  begin

                  ;; but not on the last one this is a little tricky. as you edit the
                  ;; fragment, it is not equal to the last one. We use the begin
                  ;; property which is less likely to change for the comparison.
                  (not (and kk/org-latex-fragment-last
                            (= begin
                               kk/org-latex-fragment-last))))
                 ;; go back to last one and put image back, provided there is still a fragment there
                 (save-excursion
                   (goto-char kk/org-latex-fragment-last)
                   (when (kk/org-in-latex-fragment-p) (org-preview-latex-fragment))

                   ;; now remove current image
                   (goto-char begin)
                   (let ((ov (loop for ov in (org--list-latex-overlays)
                                   if
                                   (and
                                    (<= (overlay-start ov) (point))
                                    (>= (overlay-end ov) (point)))
                                   return ov)))
                     (when ov
                       (delete-overlay ov)))
                   ;; and save new fragment
                   (setq kk/org-latex-fragment-last begin)))

                ;; were on a fragment and now are not on a fragment
                ((and
                  ;; not on a fragment now
                  (not begin)
                  ;; but we were on one
                  kk/org-latex-fragment-last)
                 ;; put image back on, provided that there is still a fragment here.
                 (save-excursion
                   (goto-char kk/org-latex-fragment-last)
                   (when (kk/org-in-latex-fragment-p) (org-preview-latex-fragment)))

                 ;; unset last fragment
                 (setq kk/org-latex-fragment-last nil))

                ;; were not on a fragment, and now are
                ((and
                  ;; we were not one one
                  (not kk/org-latex-fragment-last)
                  ;; but now we are
                  begin)
                 ;; remove image
                 (save-excursion
                   (goto-char begin)
                   (let ((ov (loop for ov in (org--list-latex-overlays)
                                   if
                                   (and
                                    (<= (overlay-start ov) (point))
                                    (>= (overlay-end ov) (point)))
                                   return ov)))
                     (when ov
                       (delete-overlay ov))))
                 (setq kk/org-latex-fragment-last begin))))))

      (defun org-latex-clear-fragment ()
        (interactive)
        (delete-directory org-preview-latex-image-directory t))
      (add-hook 'post-command-hook 'kk/org-latex-fragment-toggle t)

;;; Org-tag-with-ivy
      (defun spacemacs//org-ctrl-c-ctrl-c-counsel-org-tag ()
        "Hook for `org-ctrl-c-ctrl-c-hook' to use `counsel-org-tag'."
        (if (save-excursion (beginning-of-line) (looking-at "[ \t]*$"))
            (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
                (user-error "C-c C-c can do nothing useful at this location"))
          (let* ((context (org-element-context))
                 (type (org-element-type context)))
            (case type
              ;; When at a link, act according to the parent instead.
              (link (setq context (org-element-property :parent context))
                    (setq type (org-element-type context)))
              ;; Unsupported object types: refer to the first supported
              ;; element or object containing it.
              ((bold code entity export-snippet inline-babel-call inline-src-block
                     italic latex-fragment line-break macro strike-through subscript
                     superscript underline verbatim)
               (setq context
                     (org-element-lineage
                      context '(radio-target paragraph verse-block table-cell)))))
            ;; For convenience: at the first line of a paragraph on the
            ;; same line as an item, apply function on that item instead.
            (when (eq type 'paragraph)
              (let ((parent (org-element-property :parent context)))
                (when (and (eq (org-element-type parent) 'item)
                           (= (line-beginning-position)
                              (org-element-property :begin parent)))
                  (setq context parent type 'item))))
            (case type
              ((headline inlinetask)
               (save-excursion (goto-char (org-element-property :begin context))
                               (call-interactively 'counsel-org-tag)) t)))))
      (add-hook 'org-ctrl-c-ctrl-c-hook 'spacemacs//org-ctrl-c-ctrl-c-counsel-org-tag)
      (add-hook 'org-capture-before-finalize-hook 'counsel-org-tag)


;;; Spacemacs
      ;; We add this key mapping because an Emacs user can change
      ;; `dotspacemacs-major-mode-emacs-leader-key' to `C-c' and the key binding
      ;; C-c ' is shadowed by `spacemacs/default-pop-shell', effectively making
      ;; the Emacs user unable to exit src block editing.
      (define-key org-src-mode-map
        (kbd (concat dotspacemacs-major-mode-emacs-leader-key " '"))
        'org-edit-src-exit)

      ;; Evilify the calendar tool on C-c .
      (unless (eq 'emacs dotspacemacs-editing-style)
        (define-key org-read-date-minibuffer-local-map (kbd "M-h")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-day 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-l")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-day 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-k")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-week 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-j")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-week 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-H")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-month 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-L")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-month 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-K")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-backward-year 1))))
        (define-key org-read-date-minibuffer-local-map (kbd "M-J")
          (lambda () (interactive)
            (org-eval-in-calendar '(calendar-forward-year 1))))))))

;;; org-agenda
(defun myorg/init-org-agenda ()
  (use-package org-agenda
    :defer t
    :init
    (progn
      (setq org-agenda-restore-windows-after-quit t
            org-agenda-sticky nil
            org-agenda-block-separator ""
            org-agenda-overriding-header ""
            org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3 :fileskip0 t :stepskip0 t :tags "-COMMENT"))
            org-agenda-dim-blocked-tasks (quote invisible)
            org-agenda-log-mode-items (quote (closed clock))
            org-agenda-skip-deadline-if-done t
            org-agenda-skip-deadline-prewarning-if-scheduled t
            org-agenda-skip-scheduled-if-done t
            org-agenda-start-with-log-mode nil
            org-agenda-custom-commands '()
            )

      ;; (defun remove-headline-tags (headline)
      ;;   (if (string-match "\\(.+\\)[ \t]+\\(:.+:\\)"  headline)
      ;;       (string-trim (match-string 1 headline))
      ;;     headline))
      ;; (defun org-agenda-highlight-todo (x)
      ;; (let ((org-done-keywords org-done-keywords-for-agenda)
      ;;       (case-fold-search nil) (level nil) (position nil) re)
      ;;   (if (eq x 'line)
      ;;       (save-excursion
      ;;         (beginning-of-line 1)
      ;;         (setq re (org-get-at-bol 'org-todo-regexp))
      ;;         (setq position (or (text-property-any (point-at-bol) (point-at-eol) 'org-heading t) (point)))
      ;;         (goto-char position)
      ;;         (setq level (get-text-property position 'level))
      ;;         (when (looking-at (concat "[ \t]*\\.*\\(" re "\\) +"))
      ;;           (add-text-properties (match-beginning 0) (match-end 1)
      ;;                                (list 'face (org-get-todo-face 1)))
      ;;           (let* ((s (buffer-substring (match-beginning 1) (match-end 1)))
      ;;                  (data (buffer-substring (match-end 1) (line-end-position)))
      ;;                  (formated_s (format org-agenda-todo-keyword-format s)))
      ;;             (delete-region (match-beginning 1) (point-at-eol))
      ;;             (goto-char (match-beginning 1))
      ;;             (insert (concat "⎥ " formated_s " ⎥ "
      ;;                             (if (string-match "\\(.+\\)\\(\\[#[A-Z]?\\]\\) \\(.+\\)" data)
      ;;                                 (concat (string-trim (match-string 2 data))
      ;;                                         " ⎥" level (remove-headline-tags (match-string 3 data)))
      ;;                               (concat "     ⎥" level (remove-headline-tags (string-trim data)))))))))
      ;;     (let ((pl (text-property-any 0 (length x) 'org-heading t x)))
      ;;       (setq re (get-text-property 0 'org-todo-regexp x))
      ;;       (when (and re pl (equal (string-match (concat "\\(\\.*\\)" re "\\( +\\)") x pl)
      ;;                               pl))
      ;;         (add-text-properties
      ;;          (or (match-end 1) (match-end 0)) (match-end 0)
      ;;          (list 'face (org-get-todo-face (match-string 2 x)))
      ;;          x)
      ;;         (when (match-end 1)
      ;;           (setq x (concat (substring x 0 (match-end 1)) "⎥ "
      ;;                           (format org-agenda-todo-keyword-format (match-string 2 x)) " ⎥ "
      ;;                           (let ((data  (substring x (match-end 3))))
      ;;                             (if (string-match "\\(.*\\)\\(\\[#[A-Z]?\\]\\) \\(.*\\)" data)
      ;;                                 (concat (match-string 2 data) " ⎥"
      ;;                                         (or level (get-text-property 0 'level x))
      ;;                                         (match-string 1 data)
      ;;                                         (remove-headline-tags (match-string 3 data)))
      ;;                               (concat "     ⎥"
      ;;                                       (or level (get-text-property 0 'level x))
      ;;                                       (remove-headline-tags data))))
      ;;                           (org-add-props " " (text-properties-at 0 x))
      ;;                           )))))
      ;;     x)))
      (setq org-agenda-sorting-strategy
            '((agenda time-up priority-down category-keep)
              (todo   priority-down category-keep)
              (tags   priority-down category-keep)
              (search category-keep)))


      (dolist (prefix '(("mC" . "clocks")
                        ("md" . "dates")
                        ("mi" . "insert")
                        ("ms" . "trees/subtrees")))
        (spacemacs/declare-prefix-for-mode 'org-agenda-mode
          (car prefix) (cdr prefix)))
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "a" 'org-agenda
        "," 'counsel-org-tag-agenda
        "Cc" 'org-agenda-clock-cancel
        "Ci" 'org-agenda-clock-in
        "Co" 'org-agenda-clock-out
        "dd" 'org-agenda-deadline
        "ds" 'org-agenda-schedule
        "ie" 'org-agenda-set-effort
        "ip" 'org-agenda-set-property
        "it" 'org-agenda-set-tags
        "sr" 'org-agenda-refile
        "sc" 'org-copy
        )
      (spacemacs|define-transient-state org-agenda
        :title "Org-agenda transient state"
        :on-enter (setq which-key-inhibit t)
        :on-exit (setq which-key-inhibit nil)
        :foreign-keys run
        :doc
        "
Headline^^            Visit entry^^               Filter^^                    Date^^                  Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------     -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule         [_tf_] follow        [_vd_] day         [_cI_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dS_] un-schedule      [_tl_] log           [_vw_] week        [_cO_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fc_] by category          [_dd_] set deadline     [_ta_] archive       [_vt_] fortnight   [_cq_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_dD_] remove deadline  [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_h:_] set tags       ^^                          [_fx_] by regexp            [_dt_] timestamp        [_td_] diaries       [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   [_+_]  do later         ^^                   [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          [_-_]  do earlier       ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                      ^^                   [_vr_] reset       ^^             ^^
[_q_] quit
"
        :bindings
        ;; Entry
        ("h:" org-agenda-set-tags)
        ("hA" org-agenda-archive-default)
        ("hk" org-agenda-kill)
        ("hp" org-agenda-priority)
        ("hr" org-agenda-refile)
        ("ht" org-agenda-todo)

        ;; Visit entry
        ("SPC" org-agenda-show-and-scroll-up)
        ("<tab>" org-agenda-goto :exit t)
        ("TAB" org-agenda-goto :exit t)
        ("RET" org-agenda-switch-to :exit t)
        ("o"   link-hint-open-link :exit t)

        ;; Date
        ("ds" org-agenda-schedule)
        ("dS" (lambda () (interactive)
                (let ((current-prefix-arg '(4)))
                  (call-interactively 'org-agenda-schedule))))
        ("dd" org-agenda-deadline)
        ("dt" org-agenda-date-prompt)
        ("dD" (lambda () (interactive)
                (let ((current-prefix-arg '(4)))
                  (call-interactively 'org-agenda-deadline))))
        ("+" org-agenda-do-date-later)
        ("-" org-agenda-do-date-earlier)

        ;; View
        ("vd" org-agenda-day-view)
        ("vw" org-agenda-week-view)
        ("vt" org-agenda-fortnight-view)
        ("vm" org-agenda-month-view)
        ("vy" org-agenda-year-view)
        ("vn" org-agenda-later)
        ("vp" org-agenda-earlier)
        ("vr" org-agenda-reset-view)

        ;; Toggle mode
        ("tf" org-agenda-follow-mode)
        ("tl" org-agenda-log-mode)
        ("ta" org-agenda-archives-mode)
        ("tr" org-agenda-clockreport-mode)
        ("td" org-agenda-toggle-diary)

        ;; Filter
        ("ft" org-agenda-filter-by-tag)
        ("fr" org-agenda-filter-by-tag-refine)
        ("fc" org-agenda-filter-by-category)
        ("fh" org-agenda-filter-by-top-headline)
        ("fx" org-agenda-filter-by-regexp)
        ("fd" org-agenda-filter-remove-all)

        ;; Clock
        ("cI" org-agenda-clock-in :exit t)
        ("cj" org-agenda-clock-goto :exit t)
        ("cO" org-agenda-clock-out)
        ("cq" org-agenda-clock-cancel)

        ;; Other
        ("q" nil :exit t)
        ("gr" org-agenda-redo)
        ("." org-agenda-goto-today)
        ("gd" org-agenda-goto-date)))
    :config
    (defun myorg-agenda-format-date-aligned (date)
      "Format a DATE string for display in the daily/weekly agenda.
This function makes sure that dates are aligned for easy reading."
      (require 'cal-iso)
      (let* ((dayname (calendar-day-name date))
             (day (cadr date))
             (day-of-week (calendar-day-of-week date))
             (month (car date))
             (monthname (calendar-month-name month))
             (year (nth 2 date))
             (iso-week (org-days-to-iso-week
                        (calendar-absolute-from-gregorian date)))
             (weekyear (cond ((and (= month 1) (>= iso-week 52))
                              (1- year))
                             ((and (= month 12) (<= iso-week 1))
                              (1+ year))
                             (t year)))
             (weekstring (if (= day-of-week 1)
                             (format " W%02d" iso-week)
                           "")))
        (format "%-10s %2d %s %4d%s\n"
                dayname day monthname year weekstring)))

    (setq org-agenda-format-date 'myorg-agenda-format-date-aligned)
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      "q" 'org-agenda-filter-by-tag
      "\\" 'ace-window
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      "h" 'org-habit-toggle-habits
      "l" 'org-agenda-log-mode
      (kbd "M-j") 'org-agenda-next-item
      (kbd "M-k") 'org-agenda-previous-item
      (kbd "M-h") 'org-agenda-earlier
      (kbd "M-l") 'org-agenda-later
      (kbd "gd") 'org-agenda-toggle-time-grid
      (kbd "gr") 'org-agenda-redo
      (kbd "M-RET") 'org-agenda-show-and-scroll-up
      (kbd "M-SPC") 'spacemacs/org-agenda-transient-state/body
      (kbd "s-M-SPC") 'spacemacs/org-agenda-transient-state/body)
    (define-key org-agenda-mode-map (kbd "<escape>") 'org-agenda-quit)
    ;; (defun place-agenda-tags ()
    ;;   "Put the agenda tags by the right border of the agenda window."
    ;;   (setq org-agenda-tags-column (- 0 (window-width)))
    ;;   (org-agenda-align-tags))
    (add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
    ;; (advice-add 'org-agenda-goto :after
    ;;             (lambda (&rest args)
    ;;               (org-narrow-to-subtree)))
    ))

;;; org-brain
(defun myorg/init-org-brain ()
  (use-package org-brain
    :defer t
    :init
    (progn
      (setq org-brain-path "/Users/xfu/Dropbox/org/brain"
            org-id-track-globally t
            org-id-locations-file "~/.emacs.d/.org-id-locations"
            org-brain-visualize-default-choices 'all
            )
      (spacemacs/set-leader-keys "aob" 'org-brain-visualize)
      (evil-set-initial-state 'org-brain-visualize-mode 'emacs)
      )
    :config
    (progn
      (defun org-brain-insert-resource-icon (link)
        "Insert an icon, based on content of org-mode LINK."
        (insert (format "%s "
                        (cond ((string-prefix-p "http" link)
                               (cond ((string-match "wikipedia\\.org" link)
                                      (all-the-icons-faicon "wikipedia-w"))
                                     ((string-match "github\\.com" link)
                                      (all-the-icons-octicon "mark-github"))
                                     ((string-match "vimeo\\.com" link)
                                      (all-the-icons-faicon "vimeo"))
                                     ((string-match "youtube\\.com" link)
                                      (all-the-icons-faicon "youtube"))
                                     (t
                                      (all-the-icons-faicon "globe"))))
                              ((string-prefix-p "brain:" link)
                               (all-the-icons-fileicon "brain"))
                              (t
                               (all-the-icons-icon-for-file link))))))
      (add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)
      (defun aa2u-buffer ()
        (aa2u (point-min) (point-max)))
      (add-hook 'org-brain-after-visualize-hook #'aa2u-buffer)
      (defun org-brain-set-tags (entry)
        "Use `org-set-tags' on headline ENTRY.
If run interactively, get ENTRY from context."
        (interactive (list (org-brain-entry-at-pt)))
        (when (org-brain-filep entry)
          (error "Can only set tags on headline entries"))
        (org-with-point-at (org-brain-entry-marker entry)
          (counsel-org-tag)
          (save-buffer))
        (org-brain--revert-if-visualizing))


      (spacemacs/set-leader-keys-for-major-mode 'org-brain-visualize-mode
        "," 'org-brain-set-tags
        )
      (evilified-state-evilify-map org-brain-visualize-mode-map
        :mode org-brain-visualize-mode
        :bindings
        "a" 'org-brain-visualize-attach
        "b" 'org-brain-visualize-back
        "c" 'org-brain-add-child
        "C" 'org-brain-remove-child
        "p" 'org-brain-add-parent
        "P" 'org-brain-remove-parent
        "f" 'org-brain-add-friendship
        "F" 'org-brain-remove-friendship
        "d" 'org-brain-delete-entry
        "g" 'revert-buffer
        "_" 'org-brain-new-child
        "j" 'forward-button
        "k" 'backward-button
        "l" 'org-brain-add-resource
        "L" 'org-brain-visualize-paste-resource
        "t" 'org-brain-set-title
        "m" 'org-brain-pin
        "o" 'link-hint-open-link
        "q" 'org-brain-visualize-quit
        "r" 'org-brain-visualize-random
        "R" 'org-brain-visualize-wander
        "s" 'org-brain-visualize
        "S" 'org-brain-goto
        (kbd "TAB") 'org-brain-goto-current
        "\\" 'ace-window
        "C-d" 'scroll-down-command
        "C-u" 'scroll-up-command
        ))))

(defun myorg/init-org-expiry ()
  (use-package org-expiry
    :commands (org-expiry-insinuate
               org-expiry-deinsinuate
               org-expiry-insert-created
               org-expiry-insert-expiry
               org-expiry-add-keyword
               org-expiry-archive-subtree
               org-expiry-process-entry
               org-expiry-process-entries)))

(defun myorg/init-org-download ()
  (use-package org-download
    :commands (org-download-enable
               org-download-yank
               org-download-screenshot)
    :init
    (progn
      (add-hook 'org-mode-hook 'org-download-enable)
      (setq org-download-screenshot-method "screencapture -i %s"
            org-download-image-dir "./image/"
            )
      (spacemacs/declare-prefix-for-mode 'org-mode "miD" "download")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "iDy" 'org-download-yank
        "iDs" 'org-download-screenshot))))

(defun myorg/init-org-mime ()
  (use-package org-mime
    :defer t
    :commands (org-mime-htmlize org-mime-org-buffer-htmlize)
    :init
    (progn
      ;; move this key bindings to an `init-message' function
      (spacemacs/set-leader-keys-for-major-mode 'message-mode
        "em" 'org-mime-htmlize)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "em" 'org-mime-org-buffer-htmlize))))

(defun myorg/init-org-pomodoro ()
  (use-package org-pomodoro
    :defer t
    :init
    (progn
      (when (spacemacs/system-is-mac)
        (setq org-pomodoro-audio-player "/usr/bin/afplay"))
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "Cp" 'org-pomodoro)
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "Cp" 'org-pomodoro))))

(defun myorg/init-org-present ()
  (use-package org-present
    :defer t
    :init
    (progn
      (evilified-state-evilify nil org-present-mode-keymap
        "h" 'org-present-prev
        "l" 'org-present-next
        "q" 'org-present-quit)
      (defun spacemacs//org-present-start ()
        "Initiate `org-present' mode"
        (org-present-big)
        (org-display-inline-images)
        (org-present-hide-cursor)
        (org-present-read-only)
        (evil-evilified-state))
      (defun spacemacs//org-present-end ()
        "Terminate `org-present' mode"
        (org-present-small)
        (org-remove-inline-images)
        (org-present-show-cursor)
        (org-present-read-write)
        (evil-normal-state))
      (add-hook 'org-present-mode-hook 'spacemacs//org-present-start)
      (add-hook 'org-present-mode-quit-hook 'spacemacs//org-present-end))))

(defun myorg/init-org-projectile ()
  (use-package org-projectile
    :commands (org-projectile-location-for-project)
    :init
    (progn
      (spacemacs/set-leader-keys
        "aop" 'org-projectile/capture
        "po" 'org-projectile/goto-todos)
      (with-eval-after-load 'org-capture
        (require 'org-projectile)))
    :config
    (if (file-name-absolute-p org-projectile-file)
        (progn
          (setq org-projectile-projects-file org-projectile-file)
          (push (org-projectile-project-todo-entry :empty-lines 1)
                org-capture-templates))
      (org-projectile-per-project)
      (setq org-projectile-per-project-filepath org-projectile-file))))

(defun myorg/init-ox-twbs ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-twbs)))

(defun myorg/init-ox-reveal ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-reveal)))

(defun myorg/init-org-journal ()
  (use-package org-journal
    :defer t
    :commands (org-journal-new-entry org-journal-search-forever)
    :init
    (progn
      (spacemacs/declare-prefix "aoj" "org-journal")
      (spacemacs/set-leader-keys
        "aojj" 'org-journal-new-entry
        "aojs" 'org-journal-search-forever)

      (spacemacs/set-leader-keys-for-major-mode 'calendar-mode
        "r" 'org-journal-read-entry
        "i" 'org-journal-new-date-entry
        "n" 'org-journal-next-entry
        "p" 'org-journal-previous-entry
        "s" 'org-journal-search-forever
        "w" 'org-journal-search-calendar-week
        "m" 'org-journal-search-calendar-month
        "y" 'org-journal-search-calendar-year)

      (spacemacs/set-leader-keys-for-major-mode 'org-journal-mode
        "j" 'org-journal-new-entry
        "n" 'org-journal-open-next-entry
        "p" 'org-journal-open-previous-entry))))

(defun org/init-ox-hugo ()
  (use-package ox-hugo :after ox))
