;;; packages.el --- notmuch Layer packages File for Spacemacs

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq notmuch-packages
      '(notmuch
        counsel
        company
        yasnippet
        org-mime
        org
        avy
        wid-edit
        persp-mode
        notmuch-labeler
        ))

(defun notmuch/init-org-mime ()
  (use-package org-mime
    :after notmuch org
    :init (setq org-mime-library 'mml)
    )
  )

(defun notmuch/post-init-company-mode ()
  (spacemacs|add-company-backends
    :backends (company-capf notmuch-company company-yasnippet)
    :modes notmuch-message-mode
    )
  )

(defun notmuch/post-init-yasnippet-mode ())


(defun notmuch/post-init-persp-mode ()
  ;; do not save erc buffers
  (with-eval-after-load 'persp-mode
    (push (lambda (b) (with-current-buffer b (eq major-mode 'notmuch-hello-mode))) persp-filter-save-buffers-functions)
    (push (lambda (b) (with-current-buffer b (eq major-mode 'notmuch-tree-mode))) persp-filter-save-buffers-functions)
    (push (lambda (b) (with-current-buffer b (eq major-mode 'notmuch-search-mode))) persp-filter-save-buffers-functions)
    (push (lambda (b) (with-current-buffer b (eq major-mode 'notmuch-show-mode))) persp-filter-save-buffers-functions)
    (push (lambda (b) (with-current-buffer b (eq major-mode 'notmuch-message-mode))) persp-filter-save-buffers-functions)
    )

  (spacemacs|define-custom-layout "@Notmuch"
    :binding "n"
    :body
    (progn
      (defun spacemacs-layouts/add-notmuch-buffer-to-persp ()
        (persp-add-buffer (current-buffer) (persp-get-by-name "@Notmuch")))
      (add-hook 'notmuch-tree-mode-hook #'spacemacs-layouts/add-notmuch-buffer-to-persp)
      (add-hook 'notmuch-search-mode-hook #'spacemacs-layouts/add-notmuch-buffer-to-persp)
      (add-hook 'notmuch-hello-mode-hook #'spacemacs-layouts/add-notmuch-buffer-to-persp)
      (add-hook 'notmuch-show-mode-hook #'spacemacs-layouts/add-notmuch-buffer-to-persp)
      (add-hook 'notmuch-message-mode-hook #'spacemacs-layouts/add-notmuch-buffer-to-persp)
      (call-interactively 'notmuch)
      (purpose-load-window-layout "notmuch")
      )))

;; For each package, define a function notmuch/init-<package-notmuch>
(defun notmuch/post-init-counsel ())
(defun notmuch/post-init-avy ()
  (use-package avy
    :ensure t
    :config
    (progn
      (defun ace-link--notmuch-hello-collect ()
        "Collect the positions of visible links in *notmuch-hello*."
        (let (candidates pt)
          (save-excursion
            (save-restriction
              (goto-char (point-min))
              (setq pt (point))
              (while (progn (widget-forward 1)
                            (> (point) pt))
                (setq pt (point))
                (when (get-char-property (point) 'button)
                  (push (point) candidates)))))
          (nreverse candidates)))

      (defun ace-link--notmuch-hello-action (pt)
        (when (number-or-marker-p pt)
          (goto-char (1+ pt))
          (widget-button-press (point))))

      (defun ace-link-notmuch-hello ()
        "Open a visible link in *notmuch-hello*."
        (interactive)
        (let ((pt (avy-with ace-link-notmuch-hello
                    (avy--process
                     (ace-link--notmuch-hello-collect)
                     #'avy--overlay-pre))))
          (ace-link--notmuch-hello-action pt)))

      (defun ace-link--notmuch-show-collect ()
        "Collect the positions of visible links in `notmuch-show' buffer."
        (let (candidates pt)
          (save-excursion
            (save-restriction
              (narrow-to-region
               (window-start)
               (window-end))
              (goto-char (point-min))
              (while (re-search-forward "https?://" nil t)
                (setq pt (- (point) (length (match-string 0))))
                (push pt candidates))))
          (nreverse candidates)))

      (defun ace-link--notmuch-show-action  (pt)
        (goto-char pt)
        (browse-url-at-point))

      (defun ace-link-notmuch-show ()
        "Open a visible link in `notmuch-show' buffer."
        (interactive)
        (let ((pt (avy-with ace-link-notmuch-show
                    (avy--process
                     (ace-link--notmuch-show-collect)
                     #'avy--overlay-pre))))
          (ace-link--notmuch-show-action pt)))
      )
    )
  )

(defun notmuch/post-init-wid-edit ())
(defun notmuch/post-init-org ())
(defun notmuch/init-notmuch-labeler ())
(defun notmuch/init-notmuch ()
  "Initialize my package"
  (use-package notmuch
    :commands (notmuch-tree)
    :defer t
    :init (progn
            (remove-hook 'message-mode-hook #'turn-on-auto-fill)
            (remove-hook 'notmuch-message-mode-hook #'turn-on-auto-fill)
            (defun my-buffer-face-mode-notmuch-show ()
              "Sets a fixed width (monospace) font in current buffer"
              (interactive)
              (setq buffer-face-mode-face '(:family "charter" :height 1.2))
              (buffer-face-mode)
              (setq-local line-spacing 0.5)
              )
            (defun my-buffer-face-mode-notmuch ()
              "Sets a fixed width (monospace) font in current buffer"
              (interactive)
              (setq buffer-face-mode-face '(:family "input mono condensed" :height 1.0))
              (buffer-face-mode)
              (setq-local line-spacing 0.2)
              )


            (add-hook 'notmuch-show-mode-hook 'my-buffer-face-mode-notmuch-show)
            (add-hook 'notmuch-tree-mode-hook 'my-buffer-face-mode-notmuch)
            (add-hook 'notmuch-search-mode-hook 'my-buffer-face-mode-notmuch)
            (add-hook 'notmuch-message-mode-hook 'my-buffer-face-mode-notmuch)
            (add-hook 'notmuch-message-mode-hook
                      (lambda ()
                        (set (make-local-variable 'company-backends) '(notmuch-company (company-ispell :with company-yasnippet)))))
            (add-hook 'notmuch-tree-mode-hook (lambda () (setq-local line-spacing nil)))


            (spacemacs/set-leader-keys
              "an" 'notmuch
              "at" 'notmuch-tree
              "a;" 'notmuch-search
              "ai" 'counsel-notmuch
              ))

    :config (progn
              (setq notmuch-tag-formats '(("unread"
                                           (propertize tag 'face 'notmuch-tag-unread))
                                          )
                    notmuch-hello-sections '(
                                             notmuch-hello-insert-saved-searches
                                             notmuch-hello-insert-alltags
                                             )
                    notmuch-saved-searches '(
                                             (:name "kevin"    :query "(from:kevin or (from:fuxialexander and to:kevin)) ")
                                             (:name "hscr"    :query "tag:hscr")
                                             (:name "inbox"    :query "tag:inbox not tag:trash")
                                             (:name "flagged"  :query "tag:flagged")
                                             (:name "sent"     :query "tag:sent")
                                             (:name "drafts"   :query "tag:draft")
                                             )
                    notmuch-archive-tags '("-inbox" "-unread")
                    )

              (defun notmuch-start-notmuch-sentinel (proc event)
                "Process sentinel function used by `notmuch-start-notmuch'."
                (let* ((err-file (process-get proc 'err-file))
                       (err-buffer (or (process-get proc 'err-buffer)
                                       (find-file-noselect err-file)))
                       (err (when (not (zerop (buffer-size err-buffer)))
                              (with-current-buffer err-buffer (buffer-string))))
                       (sub-sentinel (process-get proc 'sub-sentinel))
                       (real-command (process-get proc 'real-command)))
                  (condition-case err
                      (progn
                        ;; Invoke the sub-sentinel, if any
                        (when sub-sentinel
                          (funcall sub-sentinel proc event))
                        ;; Check the exit status.  This will signal an error if the
                        ;; exit status is non-zero.  Don't do this if the process
                        ;; buffer is dead since that means Emacs killed the process
                        ;; and there's no point in telling the user that (but we
                        ;; still check for and report stderr output below).
                        (when (buffer-live-p (process-buffer proc))
                          (notmuch-check-async-exit-status proc event real-command err))
                        ;; If that didn't signal an error, then any error output was
                        ;; really warning output.  Show warnings, if any.
                        (let ((warnings
                               (when err
                                 (with-current-buffer err-buffer
                                   (goto-char (point-min))
                                   (end-of-line)
                                   ;; Show first line; stuff remaining lines in the
                                   ;; errors buffer.
                                   (let ((l1 (buffer-substring (point-min) (point))))
                                     (skip-chars-forward "\n")
                                     (cons l1 (unless (eobp)
                                                (buffer-substring (point) (point-max)))))))))
                          (when warnings
                            (notmuch-logged-error (car warnings) (cdr warnings)))))
                    (error
                     ;; Emacs behaves strangely if an error escapes from a sentinel,
                     ;; so turn errors into messages.
                     (message "%s" (error-message-string err))))
                  (when err-buffer
                    (set-process-query-on-exit-flag (get-buffer-process err-buffer) nil)
                    (kill-buffer err-buffer))
                  (when err-file (ignore-errors (delete-file err-file)))))
              (eval-after-load "notmuch-hello" `(define-key notmuch-hello-mode-map "o" 'ace-link-notmuch-hello))
              (eval-after-load "notmuch-show" `(define-key notmuch-show-mode-map "o" 'ace-link-notmuch-show))

              (defun notmuch-update ()
                (interactive)
                (start-process-shell-command "manually update email" nil "cd .mail/gmail && gmi sync && notmuch new && afew -a -t")
                (notmuch-hello-update)
                )

              (defvar counsel-notmuch-history nil
                "History for `counsel-notmuch'.")

              (defun counsel-notmuch-cmd (input)
                "Return mail"
                (counsel-require-program "/usr/local/bin/notmuch")
                (format "notmuch search %s" input)
                )

              (defun counsel-notmuch-function (input)
                "helper function"
                (setq counsel-notmuch-base-command "/usr/local/bin/notmuch search")
                (if (< (length input) 3)
                    (counsel-more-chars 3)
                  (counsel--async-command
                   (counsel-notmuch-cmd input)) '("" "working...")))

              (defun counsel-notmuch-action-tree (thread)
                "open search result in tree view"
                (setq thread-id (car (split-string thread "\\ +")))
                (notmuch-tree thread-id initial-input nil)
                )

              ;; (require 'xwidget)

              ;; (defun xw-gnus-article-html (&optional handle)
              ;;   (let ((article-buffer (current-buffer)))
              ;;     (unless handle
              ;;       (setq handle (mm-dissect-buffer t)))
              ;;     (save-restriction
              ;;       (narrow-to-region (point) (point))
              ;;       (save-excursion
              ;;         (mm-with-part handle
              ;;           (let* ((coding-system-for-read 'utf-8)
              ;;                  (coding-system-for-write 'utf-8)
              ;;                  (default-process-coding-system
              ;;                    (cons coding-system-for-read coding-system-for-write))
              ;;                  (charset (mail-content-type-get (mm-handle-type handle)
              ;;                                                  'charset)))
              ;;             (when (and charset
              ;;                        (setq charset (mm-charset-to-coding-system
              ;;                                       charset nil t))
              ;;                        (not (eq charset 'ascii)))
              ;;               (insert (prog1
			        ;;                           (decode-coding-string (buffer-string) charset)
			        ;;                         (erase-buffer)
			        ;;                         (mm-enable-multibyte))))
	            ;;             (setq tf-handle (make-temp-file "/tmp/test.html"))
              ;;             (write-region (point-min) (point-max) tf-handle t)
              ;;             (xwidget-webkit-new-session (concat "file://" tf-handle))
              ;;             )))
              ;;       )))

              ;; (setq mm-text-html-renderer 'xw-gnus-article-html)

              (defun notmuch-show-reuse-buffer (thread-id &optional elide-toggle parent-buffer query-context buffer-name)
                "Run \"notmuch show\" with the given thread ID and display results.

ELIDE-TOGGLE, if non-nil, inverts the default elide behavior.

The optional PARENT-BUFFER is the notmuch-search buffer from
which this notmuch-show command was executed, (so that the
next thread from that buffer can be show when done with this
one).

The optional QUERY-CONTEXT is a notmuch search term. Only
messages from the thread matching this search term are shown if
non-nil.

The optional BUFFER-NAME provides the name of the buffer in
which the message thread is shown. If it is nil (which occurs
when the command is called interactively) the argument to the
function is used.

Returns the buffer containing the messages, or NIL if no messages
matched."
                (interactive "sNotmuch show: \nP")
                (let (
                      ;; (buffer-name "*counsel-notmuch-show*")
                      ;; We override mm-inline-override-types to stop application/*
                      ;; parts from being displayed unless the user has customized
                      ;; it themselves.
                      (mm-inline-override-types
                       (if (equal mm-inline-override-types
                                  (eval (car (get 'mm-inline-override-types 'standard-value))))
                           (cons "application/*" mm-inline-override-types)
                         mm-inline-override-types)))
                  (switch-to-buffer (get-buffer-create buffer-name))
                  ;; No need to track undo information for this buffer.
                  (let ((inhibit-read-only t))
                    (erase-buffer))
                  (setq buffer-undo-list t)

                  (notmuch-show-mode)

                  ;; Set various buffer local variables to their appropriate initial
                  ;; state. Do this after enabling `notmuch-show-mode' so that they
                  ;; aren't wiped out.
                  (setq notmuch-show-thread-id thread-id
                        notmuch-show-parent-buffer parent-buffer
                        notmuch-show-query-context query-context

                        notmuch-show-process-crypto notmuch-crypto-process-mime
                        ;; If `elide-toggle', invert the default value.
                        notmuch-show-elide-non-matching-messages
                        (if elide-toggle
                            (not notmuch-show-only-matching-messages)
                          notmuch-show-only-matching-messages))

                  (add-hook 'post-command-hook #'notmuch-show-command-hook nil t)
                  (jit-lock-register #'notmuch-show-buttonise-links)

                  (notmuch-tag-clear-cache)

                  (let ((inhibit-read-only t))
                    (if (notmuch-show--build-buffer)
                        ;; Messages were inserted into the buffer.
                        (current-buffer)

                      ;; No messages were inserted - presumably none matched the
                      ;; query.
                      (kill-buffer (current-buffer))
                      (ding)
                      (message "No messages matched the query!")
                      nil))))

              (defun counsel-notmuch-action-show (thread)
                "open search result in show view"
                (let ((title (concat "*counsel-notmuch-show*" (substring thread 24)))
                      (thread-id (car (split-string thread "\\ +"))))
                  (notmuch-show-reuse-buffer thread-id nil nil nil title)))




              (defun counsel-notmuch (&optional initial-input)
                "search for your email in notmuch"
                (interactive)
                (ivy-set-prompt 'counsel-notmuch counsel-prompt-function)
                (ivy-read "Notmuch Search"
                          #'counsel-notmuch-function
                          :initial-input initial-input
                          :dynamic-collection t
                          ;; :keymap counsel-notmuch-map
                          :history 'counsel-notmuch-history
                          :action '(1
                                    ("o" counsel-notmuch-action-show "Show")
                                    ("t" counsel-notmuch-action-tree "Tree View")
                                    )
                          :unwind (lambda ()
                                    (counsel-delete-process)
                                    (swiper--cleanup))
                          :caller 'counsel-notmuch))

              (defun counsel-notmuch-transformer (str)
                "search notmuch asynchronously through ivy"
                (when (string-match "thread:" str)
                  (let* ((mid (substring str 24))
                         (date (propertize (substring str 24 37) 'face 'notmuch-search-date))
                         (mat (propertize
                               (substring mid (string-match "[[]" mid) (+ (string-match "[]]" mid) 1))
                               'face
                               'notmuch-search-count))
                         (people
                          (propertize
                           (truncate-string-to-width (s-trim (nth 1 (split-string mid "[];]"))) 20)
                           'face
                           'notmuch-search-matching-authors))
                         (subject
                          (propertize
                           (truncate-string-to-width (s-trim (nth 1 (split-string mid "[;]"))) (- (window-width) 32))
                           'face
                           'notmuch-search-subject))
                         (str (format "%s\t%10s\t%20s\t%s" date mat people subject)))
                    str
                    )))

              (ivy-set-display-transformer 'counsel-notmuch 'counsel-notmuch-transformer)


              (defun open-message-with-mail-app-notmuch-tree ()
                (interactive)
                (let* ((msg-path (car (plist-get (notmuch-tree-get-message-properties) :filename)))
                       (temp (make-temp-file "notmuch-message-" nil ".eml")))
                  (shell-command-to-string (format "cp '%s' '%s'; open '%s' -a Mail; l/bin/rm '%s'" msg-path temp temp temp))))
              (defun open-message-with-mail-app-notmuch-show ()
                (interactive)
                (let* ((msg-path (car (plist-get (notmuch-show-get-message-properties) :filename)))
                       (temp (make-temp-file "notmuch-message-" nil ".eml")))
                  (shell-command-to-string (format "cp '%s' '%s'; open '%s' -a Mail; l/bin/rm '%s'" msg-path temp temp temp))))

              (defun notmuch-search-delete () (interactive) (notmuch-search-add-tag (list "+trash" "-inbox" "-unread")) (notmuch-search-next-thread))
              (defun notmuch-tree-delete () (interactive) (notmuch-tree-add-tag (list "+trash" "-inbox" "-unread")) (notmuch-tree-next-message))

              (defun notmuch-search-spam () (interactive) (notmuch-search-add-tag (list "+spam" "-inbox" "-unread")) (notmuch-search-next-thread))
              (defun notmuch-tree-spam () (interactive) (notmuch-tree-add-tag (list "+spam" "-inbox" "-unread")) (notmuch-tree-next-message))

              (defun notmuch-hello-insert-searches (title query-list &rest options)
                "Insert a section with TITLE showing a list of buttons made from QUERY-LIST.

QUERY-LIST should ideally be a plist but for backwards
compatibility other forms are also accepted (see
`notmuch-saved-searches' for details).  The plist should
contain keys :name and :query; if :count-query is also present
then it specifies an alternate query to be used to generate the
count for the associated search.

Supports the following entries in OPTIONS as a plist:
:initially-hidden - if non-nil, section will be hidden on startup
:show-empty-searches - show buttons with no matching messages
:hide-if-empty - hide if no buttons would be shown
   (only makes sense without :show-empty-searches)
:filter - This can be a function that takes the search query as its argument and
   returns a filter to be used in conjuction with the query for that search or nil
   to hide the element. This can also be a string that is used as a combined with
   each query using \"and\".
:filter-count - Separate filter to generate the count displayed each search. Accepts
   the same values as :filter. If :filter and :filter-count are specified, this
   will be used instead of :filter, not in conjunction with it."
                (widget-insert (propertize title 'face 'org-agenda-structure))
                (if (and notmuch-hello-first-run (plist-get options :initially-hidden))
                    (add-to-list 'notmuch-hello-hidden-sections title))
                (let ((is-hidden (member title notmuch-hello-hidden-sections))
                      (widget-push-button-prefix "")
                      (widget-push-button-suffix "")
                      (start (point)))
                  (if is-hidden
                      (widget-create 'push-button
                                     :notify `(lambda (widget &rest ignore)
                                                (setq notmuch-hello-hidden-sections
                                                      (delete ,title notmuch-hello-hidden-sections))
                                                (notmuch-hello-update))
                                     (propertize " +" 'face 'org-agenda-structure))
                    (widget-create 'push-button
                                   :notify `(lambda (widget &rest ignore)
                                              (add-to-list 'notmuch-hello-hidden-sections
                                                           ,title)
                                              (notmuch-hello-update))
                                   " -"))
                  (widget-insert "\n")
                  (when (not is-hidden)
                    (let ((searches (apply 'notmuch-hello-query-counts query-list options)))
                      (when (or (not (plist-get options :hide-if-empty))
                                searches)
                        (widget-insert "\n")
                        (notmuch-hello-insert-buttons searches)
                        (indent-rigidly start (point) notmuch-hello-indent))))))

              (defun notmuch-hello-insert-saved-searches ()
                "Insert the saved-searches section."
                (let ((searches (notmuch-hello-query-counts
                                 (if notmuch-saved-search-sort-function
                                     (funcall notmuch-saved-search-sort-function
                                              notmuch-saved-searches)
		                               notmuch-saved-searches)
		                             :show-empty-searches notmuch-show-empty-saved-searches)))
                  (when searches
                    (widget-insert (propertize "Notmuch" 'face 'org-agenda-date-today))
                    (widget-insert "\n\n")
                    (widget-insert (propertize "Saved searches" 'face 'org-agenda-structure))
                    (widget-insert "\n\n")
                    (let ((start (point)))
	                    (notmuch-hello-insert-buttons searches)
	                    (indent-rigidly start (point) notmuch-hello-indent)))))

              (defun notmuch-hello-insert-buttons (searches)
                "Insert buttons for SEARCHES.

SEARCHES must be a list of plists each of which should contain at
least the properties :name NAME :query QUERY and :count COUNT,
where QUERY is the query to start when the button for the
corresponding entry is activated, and COUNT should be the number
of messages matching the query.  Such a plist can be computed
with `notmuch-hello-query-counts'."
                (let* ((widest (notmuch-hello-longest-label searches))
                       (tags-and-width (notmuch-hello-tags-per-line widest))
                       (tags-per-line (car tags-and-width))
                       (column-width (cdr tags-and-width))
                       (column-indent 0)
                       (count 0)
                       (reordered-list (notmuch-hello-reflect searches tags-per-line))
                       ;; Hack the display of the buttons used.
                       (widget-push-button-prefix "")
                       (widget-push-button-suffix ""))
                  ;; dme: It feels as though there should be a better way to
                  ;; implement this loop than using an incrementing counter.
                  (mapc (lambda (elem)
                          ;; (not elem) indicates an empty slot in the matrix.
                          (when elem
                            (if (> column-indent 0)
                                (widget-insert (make-string column-indent ? )))
                            (let* ((name (plist-get elem :name))
                                   (query (plist-get elem :query))
                                   (oldest-first (case (plist-get elem :sort-order)
                                                   (newest-first nil)
                                                   (oldest-first t)
                                                   (otherwise notmuch-search-oldest-first)))
                                   (search-type (eq (plist-get elem :search-type) 'tree))
                                   (msg-count (plist-get elem :count)))
                              (widget-insert (format "%8s "
                                                     (notmuch-hello-nice-number msg-count)))
		                          (widget-create 'push-button
			                                       :notify #'notmuch-hello-widget-search
			                                       :notmuch-search-terms query
			                                       :notmuch-search-oldest-first oldest-first
			                                       :notmuch-search-type search-type
			                                       name)
		                          (setq column-indent
		                                (1+ (max 0 (- column-width (length name)))))))
	                        (setq count (1+ count))
	                        (when (eq (% count tags-per-line) 0)
	                          (setq column-indent 0)
	                          (widget-insert "\n")))
	                      reordered-list)

                  ;; If the last line was not full (and hence did not include a
                  ;; carriage return), insert one now.
                  (unless (eq (% count tags-per-line) 0)
                    (widget-insert "\n"))))

              (setq notmuch-fcc-dirs nil
                    notmuch-show-logo nil
                    message-kill-buffer-on-exit t
                    message-send-mail-function 'message-send-mail-with-sendmail
                    notmuch-search-oldest-first nil
                    send-mail-function 'sendmail-send-it
                    sendmail-program "/usr/local/bin/msmtp"
                    )

              (spacemacs/set-leader-keys-for-major-mode 'notmuch-message-mode
                dotspacemacs-major-mode-leader-key 'notmuch-mua-send-and-exit
                "k" 'notmuch-mua-kill-buffer
                "s" 'message-dont-send         ; saves as draft
                "f" 'mml-attach-file)

              (evilified-state-evilify-map notmuch-search-mode-map
                :mode notmuch-search-mode
                :bindings
                (kbd "j") 'notmuch-search-next-thread
                (kbd "k") 'notmuch-search-previous-thread
                (kbd "t") 'notmuch-tree-from-search-thread
                (kbd "T") 'notmuch-tree-from-search-current-query
                (kbd "d") 'notmuch-search-delete
                (kbd "q") 'notmuch
                (kbd "\\") 'ace-window
                (kbd "x") 'notmuch-search-spam
                )
              (evilified-state-evilify-map notmuch-tree-mode-map
                :mode notmuch-tree-mode
                :bindings
                (kbd "j") 'notmuch-tree-next-message
                (kbd "k") 'notmuch-tree-prev-message
                (kbd "S") 'notmuch-search-from-tree-current-query
                (kbd "t") 'notmuch-tree
                (kbd "q") 'notmuch-tree-quit
                (kbd "\\") 'ace-window
                (kbd "r") 'notmuch-search-reply-to-thread-sender
                (kbd "i") 'open-message-with-mail-app-notmuch-tree
                (kbd "d") 'notmuch-tree-delete
                (kbd "x") 'notmuch-tree-spam
                )

              (evilified-state-evilify-map notmuch-hello-mode-map
                :mode notmuch-hello-mode
                :bindings
                (kbd "t") 'notmuch-tree
                (kbd "k") 'widget-backward
                (kbd "j") 'widget-forward
                (kbd "\\") 'ace-window
                (kbd "q") 'notmuch-hello-update
                (kbd "e") 'notmuch-update
                )

              (evilified-state-evilify-map notmuch-show-mode-map
                :mode notmuch-show-mode
                :bindings
                (kbd "i") 'open-message-with-mail-app-notmuch-show
                (kbd "I") 'notmuch-show-view-all-mime-parts
                (kbd "q") 'quit-window
                (kbd "\\") 'ace-window
                (kbd "e") 'evil-forward-word-end
                (kbd "w") 'evil-forward-word-begin
                (kbd "b") 'evil-backward-word-begin
                (kbd "f") 'evil-snipe-f
                (kbd "F") 'evil-snipe-F
                (kbd "b") 'evil-backward-word-begin
                (kbd "t") 'notmuch-tree-from-show-current-query
                (kbd "n") 'notmuch-show-next-thread-show
                (kbd "p") 'notmuch-show-previous-thread-show
                )


              )))

;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
