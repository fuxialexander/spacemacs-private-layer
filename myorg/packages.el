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
        org-pomodoro
        org-present
        (org-projectile :requires projectile)
        (ox-twbs :toggle org-enable-bootstrap-support)
        (ox-reveal :toggle org-enable-reveal-js-support)
        ))

(defun myorg/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf :modes org-mode)
  (spacemacs|add-company-backends :backends company-ispell :modes org-mode)
  )

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
      (setq org-clock-persist-file (concat spacemacs-cache-directory "org-clock-save.el")
            org-id-locations-file (concat spacemacs-cache-directory ".org-id-locations")
            org-publish-timestamp-directory (concat spacemacs-cache-directory ".org-timestamps/")
            org-startup-with-inline-images nil
            org-image-actual-width '(500)
            org-fontify-whole-heading-line t
            org-fontify-done-headline t
            org-fontify-quote-and-verse-blocks t
            org-ellipsis " + "
            org-columns-ellipses " + "
            org-bullets-bullet-list '("" "" "" "" "" "" "" "")
            org-src-fontify-natively t
            org-preview-latex-default-process 'dvisvgm
            org-modules
            (quote
             (org-bibtex org-docview org-habit org-info org-protocol org-mac-iCal org-mac-link org-notmuch))
            org-imenu-depth 8)

;;;;; Org-TODO
      (defun my-org-move-point-to-capture ()
        (cond ((org-at-heading-p) (org-beginning-of-line))
              (t (org-previous-visible-heading 1))))

      (setq org-directory "/Users/xfu/Dropbox/org/"
            org-capture-templates (quote (
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

                  ("d" "Daily Review" entry
                   (file+olp+datetree "~/Dropbox/org/review.org")
                   "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:daily:%\\1:
:PROPERTIES:
:Created: %U
:Linked: [[file:life.org::*Daily%20review][Daily review]]
:END:
%i
%?" :empty-lines 1 :clock-in t :created t)

                  ("w" "Week Review" entry
                   (file+olp+datetree "~/Dropbox/org/review.org")
                   "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:week:%\\1:
:PROPERTIES:
:Created: %U
:Linked: [[file:life.org::*Week%20review][Week review]]
:END:
%i
%?" :empty-lines 1 :clock-in t :created t)

                  ("r" "Month Review" entry
                   (file+olp+datetree "~/Dropbox/org/review.org")
                   "* %^{Review for...|Mood|Research|Learn|Entertainment|Life} :review:month:%\\1:
:PROPERTIES:
:Created: %U
:Linked: [[file:life.org::*Month%20review][Month review]]
:END:
%i
%?" :empty-lines 1 :clock-in t :created t)

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
            org-habit-graph-column 60
            org-hide-block-startup t
            org-tags-column 0
            org-agenda-restore-windows-after-quit t
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
                                     ("TODO" :inherit default :foreground "#d34a65" :weight bold :box (:line-width 1 :color "#d34a65" :style nil))
                                     ("HABT" :inherit default :foreground "#EADC77" :weight bold :box (:line-width 1 :color "#EADC77" :style nil))
                                     ("DONE" :inherit default :foreground "#15c188" :weight bold :box (:line-width 1 :color "#15c188" :style nil))
                                     ("WAIT" :inherit default :foreground "#ff9d00" :weight bold :box (:line-width 1 :color "#ff9d00" :style nil))
                                     ("KILL" :inherit default :foreground "#6f2faf" :weight bold :box (:line-width 1 :color "#6f2faf" :style nil))
                                     ("OUTD" :inherit default :foreground "#6fa1f2" :weight bold :box (:line-width 1 :color "#6fa1f2" :style nil))
                                     )
            )
;;;;; Org-latex
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

        "Tt"    'org-show-todo-tree
        "Ti"    'org-toggle-inline-images
        "TT"    'org-todo
        "TV"    'space-doc-mode
        "Tx"    'org-toggle-latex-fragment

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
        "ta"    'org-table-align
        "tb"    'org-table-blank-field
        "tc"    'org-table-convert
        "tdc"   'org-table-delete-column
        "tdr"   'org-table-kill-row
        "te"    'org-table-eval-formula
        "tE"    'org-table-export
        "th"    'org-table-previous-field
        "tH"    'org-table-move-column-left
        "tic"   'org-table-insert-column
        "tih"   'org-table-insert-hline
        "tiH"   'org-table-hline-and-move
        "tir"   'org-table-insert-row
        "tI"    'org-table-import
        "tj"    'org-table-next-row
        "tJ"    'org-table-move-row-down
        "tK"    'org-table-move-row-up
        "tl"    'org-table-next-field
        "tL"    'org-table-move-column-right
        "tn"    'org-table-create
        "tN"    'org-table-create-with-table.el
        "tr"    'org-table-recalculate
        "ts"    'org-table-sort-lines
        "ttf"   'org-table-toggle-formula-debugger
        "tto"   'org-table-toggle-coordinate-overlays
        "tw"    'org-table-wrap-region

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
        "xv" (spacemacs|org-emphasize spacemacs/org-verbose ?=))

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

;;; Org-pandoc
      (setq org-pandoc-options '(
                                 (standalone . t)
                                 (self-contained . t)
                                 ))


;;; Org-babel
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((emacs-lisp . t)
         (python . t)
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
            (cd (file-name-as-directory (expand-file-name dir)))
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
            org-agenda-block-separator ""
            org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3 :fileskip0 t :stepskip0 t :tags "-COMMENT"))
            org-agenda-dim-blocked-tasks (quote invisible)
            org-agenda-log-mode-items (quote (closed clock))
            org-agenda-restore-windows-after-quit t
            org-agenda-skip-deadline-if-done t
            org-agenda-skip-deadline-prewarning-if-scheduled t
            org-agenda-skip-scheduled-if-deadline-is-shown t
            org-agenda-skip-scheduled-if-done t
            org-agenda-start-with-log-mode nil
            org-agenda-custom-commands
            (quote
             (
              ("n" "Agenda and all TODOs"
               ((agenda ""
                        ((org-agenda-span 3)
                         (org-agenda-overriding-header " Week Agenda
")))
                (alltodo ""
                         ((org-agenda-skip-function
                           (quote
                            (org-agenda-skip-entry-if
                             (quote scheduled))))
                          (org-agenda-overriding-header " TODOs
")
                          (org-agenda-sorting-strategy
                           (quote
                            (todo-state-up deadline-up))))))
               ((org-agenda-tag-filter-preset
                 (quote
                  ("-COMMENT")))))
              ))
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
Headline^^            Visit entry^^               Filter^^                    Date^^               Toggle mode^^        View^^             Clock^^        Other^^
--------^^---------   -----------^^------------   ------^^-----------------   ----^^-------------  -----------^^------  ----^^---------    -----^^------  -----^^-----------
[_ht_] set status     [_SPC_] in other window     [_ft_] by tag               [_ds_] schedule      [_tf_] follow        [_vd_] day         [_cI_] in      [_gr_] reload
[_hk_] kill           [_TAB_] & go to location    [_fr_] refine by tag        [_dd_] set deadline  [_tl_] log           [_vw_] week        [_cO_] out     [_._]  go to today
[_hr_] refile         [_RET_] & del other windows [_fc_] by category          [_dt_] timestamp     [_ta_] archive       [_vt_] fortnight   [_cq_] cancel  [_gd_] go to date
[_hA_] archive        [_o_]   link                [_fh_] by top headline      [_+_]  do later      [_tr_] clock report  [_vm_] month       [_cj_] jump    ^^
[_h:_] set tags       ^^                          [_fx_] by regexp            [_-_]  do earlier    [_td_] diaries       [_vy_] year        ^^             ^^
[_hp_] set priority   ^^                          [_fd_] delete all filters   ^^                   ^^                   [_vn_] next span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vp_] prev span   ^^             ^^
^^                    ^^                          ^^                          ^^                   ^^                   [_vr_] reset       ^^             ^^
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
      ("dd" org-agenda-deadline)
      ("dt" org-agenda-date-prompt)
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
    (evilified-state-evilify-map org-agenda-mode-map
      :mode org-agenda-mode
      :bindings
      "j" 'org-agenda-next-line
      "k" 'org-agenda-previous-line
      (kbd "M-j") 'org-agenda-next-item
      (kbd "M-k") 'org-agenda-previous-item
      (kbd "M-h") 'org-agenda-earlier
      (kbd "M-l") 'org-agenda-later
      (kbd "gd") 'org-agenda-toggle-time-grid
      (kbd "gr") 'org-agenda-redo
      (kbd "M-RET") 'org-agenda-show-and-scroll-up
      (kbd "M-SPC") 'spacemacs/org-agenda-transient-state/body
      (kbd "s-M-SPC") 'spacemacs/org-agenda-transient-state/body)
    (defun place-agenda-tags ()
      "Put the agenda tags by the right border of the agenda window."
      (setq org-agenda-tags-column (- 0 (window-width)))
      (org-agenda-align-tags))
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
      (evil-set-initial-state 'org-brain-visualize-mode 'emacs))))

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
            org-download-image-html-width 500
            org-download-image-latex-width 500
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
