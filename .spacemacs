;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;;; Layers
(defun dotspacemacs/layers()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'nil
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation nil
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-private-snippets-directory nil)
     better-defaults
     emacs-lisp
     evil-snipe
     imenu-list
     git
     version-control
     colors
     pandoc
     markdown
     (treemacs :variables treemacs-use-follow-mode t)
     ;; (shell :variables
     ;;        shell-default-shell 'eshell
     ;;        shell-default-height 40
     ;;        )
     ivy
     (latex :variables latex-build-command "LatexMk")
     (ess :variables ess-enable-smart-equals t)
     python
     osx
     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-tooltips nil)
     ;; ipython-notebook
     display
     personal
     notmuch
     myshellscripts
     mybibtex
     (myorg :variables org-enable-bootstrap-support t)
     (myelfeed :variables rmh-elfeed-org-files (list "~/.emacs.d/private/feed.org"))
     (languagetool :variables langtool-language-tool-jar "/usr/local/Cellar/languagetool/3.9/libexec/languagetool-commandline.jar")
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.

;;;; Additional Packages

   dotspacemacs-additional-packages '(
                                      ;; circadian
                                      helpful
                                      ivy-dired-history
                                      flycheck-package
                                      cdlatex
                                      tiny
                                      olivetti
                                      shx
                                      shrink-path
                                      fringe-helper
                                      kurecolor
                                      dired-narrow
                                      prodigy
                                      ob-async
                                      org-edit-latex
                                      language-detection
                                      shr-tag-pre-highlight
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
;;;; Excluded Packages
   dotspacemacs-excluded-packages '(
                                    flycheck-pos-tip
                                    pos-tip
                                    ess-R-object-popup
                                    evil-escape
                                    vi-tilde-fringe
                                    spaceline
                                    exec-path-from-shell
                                    neotree
                                    magit-gitflow
                                    magit-gh-pulls
                                    org-repo-todo
                                    auto-complete
                                    org-projectile
                                    linum
                                    linum-relative
                                    skewer-mode
                                    livid-mode
                                    gh-md
                                    disaster
                                    fancy-battery
                                    helm-flyspell
                                    flyspell-correct-helm
                                    lorem-ipsum
                                    symon
                                    highlight-indentation
                                    smooth-scrolling
                                    clean-aindent-mode
                                    helm-c-yasnippet
                                    ace-jump-helm-line
                                    helm-themes
                                    helm-swoop
                                    helm-spacemacs-help
                                    ido-vertical-mode
                                    flx-ido
                                    helm-purpose
                                    helm-make
                                    )
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

;;; Init
(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the lastest
   ;; version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil
   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists nil
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive nil
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         (modern-solarizedlight :location (recipe :fetcher github :repo "fuxialexander/modern-light-theme"))
                         )
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("operator mono lig"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.2
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose t
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   dotspacemacs-frame-title-format "%I@%S$%a$"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

;;; User-init
(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; (setq gc-cons-threshold 1000000)
  (add-to-list 'load-path "~/.emacs.d/private/elisp/")
  (defun t/project-root ()
    "Get project root without throwing"
    (let (projectile-require-project-root strict-p)
      (projectile-project-root)))

  (defun t/init-modeline () (+doom-modeline|init))
  (defun export-diary-from-cal ()
    (interactive)
    (start-process-shell-command "export diary" nil "/Users/xfu/.emacs.d/private/local/calendardiary 30 > /Users/xfu/Dropbox/org/cal.diary")
    )

  (require 'doom-modeline)
  (add-hook 'after-init-hook #'t/init-modeline)

  (add-hook 'help-mode-hook #'doom-hide-modeline-mode)
  (add-hook 'message-mode-hook #'doom-hide-modeline-mode)
  (add-hook 'after-init-hook 'export-diary-from-cal)
  (setq evil-respect-visual-line-mode t)
  )

;;; User-config
(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."


;;;; Helpful
  (use-package helpful
    :ensure t
    :bind ("H-/" . helpful-at-point)
    :config
    (evilified-state-evilify-map helpful-mode-map
      :mode helpful-mode
      :bindings
      (kbd "q") 'quit-window
      (kbd "l") 'helpful-forward-button
      (kbd "h") 'helpful-backward-button
      (kbd "q") 'quit-window
      (kbd "RET") 'helpful-visit-reference
      (kbd "\\") 'ace-window
      )
    )

;;;; XWidget
;;   (defun xwidget-webkit-browse-url (url &optional new-session)
;;     "Ask xwidget-webkit to browse URL.
;; NEW-SESSION specifies whether to create a new xwidget-webkit session.
;; Interactively, URL defaults to the string looking like a url around point."
;;     (interactive (progn
;;                    (require 'browse-url)
;;                    (browse-url-interactive-arg "xwidget-webkit URL: "
;;                                                ;;(xwidget-webkit-current-url)
;;                                                )))
;;     (or (featurep 'xwidget-internal)
;;         (user-error "Your Emacs was not compiled with xwidgets support"))
;;     (when (stringp url)
;;       (if new-session
;;           (xwidget-webkit-new-session url)
;;         (xwidget-webkit-goto-url url))))


;;;; Edit
  ;; (use-package tiny
  ;;   :ensure t
  ;;   :config (tiny-setup-default)
  ;;   )


  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . 'dark)) ; or 'dark, to switch to white title text


  (defun camelize (s)
    "Convert under_score string S to CamelCase string."
    (store-substring
     (mapconcat 'identity
                (mapcar (lambda (word)
                          (capitalize (downcase word)))
                        (split-string s "[-_ ]+")) "")
     0 (if (> (length s) 0)
           (downcase (substring s 0 1)))))

  (defun align-repeat (start end regexp)
    "Repeat alignment with respect to
     the given regular expression."
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end
                  (concat "\\(\\s-*\\)" regexp) 1 1 t))


  (yas-global-mode)

;;;; UI


;; ;;;;; Circadian
;;   (use-package circadian
;;     :load-path "~/.emacs.d/config/circadian/"
;;     :ensure t
;;     :config
;;     (setq circadian-themes '(("8:00" . modern-light)
;;                              ("19:30" . modern-dawn)
;;                              ("00:00" . modern-dark)))
;;     (circadian-setup))


;;;;; Term line-spacing

  ;; (use-package shx
  ;;   :commands shx
  ;;   )

  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (setq line-spacing nil)))

  (setf (cdr (assq 'continuation fringe-indicator-alist))
        '(nil nil) ;; no continuation indicators
        ;; '(nil right-curly-arrow) ;; right indicator only
        ;; '(left-curly-arrow nil) ;; left indicator only
        ;; '(left-curly-arrow right-curly-arrow) ;; default
        )
  (setf (cdr (assq 'truncation fringe-indicator-alist))
        '(nil nil) ;; no continuation indicators
        ;; '(nil right-curly-arrow) ;; right indicator only
        ;; '(left-curly-arrow nil) ;; left indicator only
        ;; '(left-curly-arrow right-curly-arrow) ;; default
        )

;;;;; Doom
  (use-package shrink-path
    :commands (shrink-path-prompt shrink-path-file-mixed))

  (use-package company-files
    :defer t
    :init
    (spacemacs|add-company-backends
      :backends company-files
      :modes shell-script-mode))


  (setq-default
   ;; browse-url-browser-function 'xwidget-webkit-browse-url
   ;; browse-url-new-window-flag t
   ispell-program-name "/usr/local/bin/aspell"
   python-shell-interpreter "/usr/local/bin/python3"
   python-shell-interpreter-args "-i"
   python-shell-interpreter-interactive-arg "-i"
   counsel-describe-function-function 'helpful-function
   counsel-describe-variable-function 'helpful-variable
   line-spacing 0.2
   exec-path-from-shell-check-startup-files nil
   display-line-numbers nil
   bidi-paragraph-direction 'left-to-right
   company-frontends '(company-preview-frontend company-echo-metadata-frontend)
   company-statistics-mode t
   company-require-match nil
   company-minimum-prefix-length 2
   custom-buffer-done-kill t
   auto-revert-remote-files nil
   ;; mac-mouse-wheel-smooth-scroll t

   blink-matching-paren nil    ; don't blink--too distracting
   cursor-in-non-selected-windows nil  ; hide cursors in other windows
   frame-inhibit-implied-resize t
   highlight-nonselected-windows nil
   vc-follow-symlinks t
   image-animate-loop t
   indicate-buffer-boundaries nil
   indicate-empty-lines nil
   image-animate-loop t
   indicate-buffer-boundaries nil
   indicate-empty-lines nil
   max-mini-window-height 0.3
   hybrid-mode-enable-hjkl-bindings t
   mode-line-default-help-echo nil ; disable mode-line mouseovers
   mouse-yank-at-point t           ; middle-click paste at point, not at click
   resize-mini-windows 'grow-only  ; Minibuffer resizing
   show-help-function nil          ; hide :help-echo text
   uniquify-buffer-name-style 'forward
   use-dialog-box nil              ; always avoid GUI
   visible-cursor nil
   x-stretch-cursor nil
   ;; defer jit font locking slightly to [try to] improve Emacs performance
   jit-lock-defer-time nil
   jit-lock-stealth-nice 0.1
   jit-lock-stealth-time 0.2
   jit-lock-stealth-verbose nil
   ;; no beeping or blinking please
   ring-bell-function #'ignore
   visible-bell nil
   truncate-lines t
   split-height-threshold 100
   split-width-threshold 100

   ;; window-divider-mode t
   ;; global-hl-line-mode t
   ;; global-company-mode t
   ;; global-auto-revert-mode t
   ;; global-auto-revert-non-file-buffers t
   frame-resize-pixelwise t
   window-resize-pixelwise t
   window-divider-default-places t
   window-divider-default-bottom-width 0
   window-divider-default-right-width 1)

  (window-divider-mode                 )
  (global-hl-line-mode                 )
  (global-company-mode                 )
  (global-auto-revert-mode             )


  ;; standardize default fringe width
  (defvar doom-fringe-size '4
    "Default fringe width.")
  (if (fboundp 'fringe-mode) (fringe-mode doom-fringe-size))
  (add-hook 'after-init-hook #'window-divider-mode)
  (global-hl-line-mode -1)
  (use-package fringe-helper
    :commands (fringe-helper-define fringe-helper-convert)
    :init
    (unless (fboundp 'define-fringe-bitmap)
      (defun define-fringe-bitmap (&rest _))))
  ;; NOTE Adjust these bitmaps if you change `doom-fringe-size'
  (with-eval-after-load 'flycheck
    ;; because git-gutter is in the left fringe
    (setq flycheck-indication-mode 'right-fringe)
    ;; A non-descript, left-pointing arrow
    (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
      "...X...."
      "..XX...."
      ".XXX...."
      "XXXX...."
      ".XXX...."
      "..XX...."
      "...X...."))

  ;; subtle diff indicators in the fringe
  (with-eval-after-load 'git-gutter-fringe+
    ;; places the git gutter outside the margins.
    (setq-default fringes-outside-margins t)
    (setq git-gutter-fr+-side 'left-fringe)
    ;; thin fringe bitmaps
    (fringe-helper-define 'git-gutter-fr+-added '(center repeated)
      "XXX.....")
    (fringe-helper-define 'git-gutter-fr+-modified '(center repeated)
      "XXX.....")
    (fringe-helper-define 'git-gutter-fr+-deleted 'bottom
      "X......."
      "XX......"
      "XXX....."
      "XXXX...."))

  (evil-snipe-override-mode 1)

;;;; Keybindings
  (spacemacs/set-leader-keys "TAB" 'ivy-switch-buffer)
  (global-set-key (kbd "C-H-M-!") 'spacemacs/custom-perspective-@Research )
  (global-set-key (kbd "C-H-M-@") 'spacemacs/custom-perspective-@Notmuch )
  (global-set-key (kbd "C-H-M-#") 'spacemacs/custom-perspective-@Elfeed )
  (global-set-key (kbd "C-H-M-$") 'spacemacs/custom-perspective-@Lisp )
  (define-key key-translation-map (kbd "<escape>") nil)
  (define-key key-translation-map (kbd "ESC") nil)
  (define-key image-mode-map (kbd "<escape>") 'quit-window)
  ;; (define-key python-mode-map (kbd "C-j") 'counsel-oi)
  (define-key emacs-lisp-mode-map (kbd "C-j") 'counsel-oi)
  (define-key evil-motion-state-map (kbd "\\") 'ace-window)
  (define-key evil-evilified-state-map (kbd "\\") 'ace-window)
  (global-set-key (kbd "H-f") 'counsel-company )

  (defun org-agenda-show-daily (&optional arg)
    (interactive "P")
    (org-agenda arg "a"))

  (global-set-key (kbd "H-g") 'org-agenda-show-daily )
  (global-set-key (kbd "H-r") 'counsel-org-capture )

  (define-key winum-keymap (kbd "M-1") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-1-and-exit)
  (define-key winum-keymap (kbd "M-2") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-2-and-exit)
  (define-key winum-keymap (kbd "M-3") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-3-and-exit)
  (define-key winum-keymap (kbd "M-4") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-4-and-exit)
  (define-key winum-keymap (kbd "M-5") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-5-and-exit)
  (define-key winum-keymap (kbd "M-6") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-6-and-exit)
  (define-key winum-keymap (kbd "M-7") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-7-and-exit)
  (define-key winum-keymap (kbd "M-8") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-8-and-exit)
  (define-key winum-keymap (kbd "M-9") 'spacemacs/workspaces-transient-state/eyebrowse-switch-to-window-config-9-and-exit)

  (global-set-key (kbd "H-1") 'winum-select-window-1)
  (global-set-key (kbd "H-2") 'winum-select-window-2)
  (global-set-key (kbd "H-3") 'winum-select-window-3)
  (global-set-key (kbd "H-4") 'winum-select-window-4)
  (global-set-key (kbd "H-5") 'winum-select-window-5)
  (global-set-key (kbd "H-6") 'winum-select-window-6)
  (global-set-key (kbd "H-7") 'winum-select-window-7)
  (global-set-key (kbd "H-8") 'winum-select-window-8)
  (global-set-key (kbd "H-9") 'winum-select-window-9)


  (global-set-key (kbd "H-,") 'customize )
  (global-set-key (kbd "H-o") 'org-store-link )
  (global-set-key (kbd "H-d") 'split-window-right-and-focus )
  (global-set-key (kbd "H-D") 'split-window-below-and-focus )
  (define-key global-map (kbd "<C-tab>") nil)
  (global-set-key (kbd-mac-command "h") 'ns-do-hide-emacs)
  (global-set-key (kbd "<f11>") nil)
  ;; (global-set-key (kbd "C-H-f") 'spacemacs/toggle-fullscreen)
  (eval-after-load "flyspell" '(define-key flyspell-mode-map (kbd "C-.") nil))
  (eval-after-load "flyspell" '(define-key flyspell-mode-map (kbd "C-;") nil))
  (eval-after-load "flyspell" '(define-key flyspell-mode-map (kbd "M-;") 'flyspell-correct-previous-word-generic))
  (eval-after-load "magit" '(define-key magit-mode-map (kbd "M-1") nil))
  (eval-after-load "magit" '(define-key magit-mode-map (kbd "M-2") nil))
  (eval-after-load "magit" '(define-key magit-mode-map (kbd "M-3") nil))
  (eval-after-load "magit" '(define-key magit-mode-map (kbd "M-4") nil))
  (eval-after-load "magit" '(define-key magit-mode-map (kbd "M-w") nil))
  (eval-after-load "shr" '(define-key shr-map (kbd "o") nil))

  (defun highlight-grammar ()
    (interactive)
    (highlight-regexp "\\\w+s[\\\., ;]" 'hi-yellow)
    )



  (eval-after-load "org" '(define-key org-mode-map (kbd "M-;") nil))
  (eval-after-load "org" '(define-key org-mode-map (kbd "H-p") 'org-toggle-latex-fragment))
  (eval-after-load "org" '(define-key org-mode-map (kbd "M-;") 'flyspell-correct-previous-word-generic))
  (eval-after-load "org" '(define-key org-mode-map (kbd "H-l") 'org-ref-ivy-insert-cite-link))
  (eval-after-load "org" '(define-key org-mode-map (kbd "C-j") 'counsel-org-goto))
  (eval-after-load "org" '(define-key org-mode-map (kbd "C-H-j") 'counsel-org-goto-all))
  (eval-after-load "org" '(define-key org-mode-map (kbd "H-i") 'org-insert-link))
  (eval-after-load "auctex" '(define-key latex-mode-map (kbd "H-l") 'org-ref-ivy-insert-cite-link))
  ;; (eval-after-load "eshell" '(eshell-git-prompt-use-theme 'powerline))
  (define-key evil-hybrid-state-map (kbd "C-;") 'hippie-expand)

;;;; Treemacs
  (with-eval-after-load 'treemacs
    (define-key treemacs-mode-map (kbd "<escape>") 'treemtreemacs-toggle)
    (add-hook 'treemacs-mode-hook 'doom-hide-modeline-mode)
    )

;;;; Magit
  (with-eval-after-load 'magit
    (add-hook 'magit-mode-hook 'doom-hide-modeline-mode)
    (add-hook 'magit-popup-mode-hook 'doom-hide-modeline-mode)
    )
;;;; Layout
  (with-eval-after-load 'persp-mode
    (push (lambda (b) (with-current-buffer b (eq major-mode 'dired-mode))) persp-filter-save-buffers-functions)
    (push (lambda (b) (with-current-buffer b (eq major-mode 'elfeed-search-mode))) persp-filter-save-buffers-functions)
    (push (lambda (b) (with-current-buffer b (eq major-mode 'elfeed-show-mode))) persp-filter-save-buffers-functions)

    (setq persp-add-buffer-on-after-change-major-mode nil)

    (add-hook 'persp-common-buffer-filter-functions
              ;; there is also `persp-add-buffer-on-after-change-major-mode-filter-functions'
              #'(lambda (b) (or
                             (string-prefix-p " " (buffer-name b))
                             (string-match-p "feed.org" (buffer-name b))
                             (string-match-p ".custom-settings" (buffer-name b))
                             (and
                              (string-prefix-p "*" (buffer-name b))
                              (not (string-match-p "*Org Src" (buffer-name b)))
                              ;; (not (string-match-p "*Org Agenda" (buffer-name b)))
                              )
                             )))
    )

  (spacemacs|define-custom-layout "@Lisp"
    :binding "l"
    :body
    (progn
      (defun spacemacs-layouts/add-lisp-buffer-to-persp ()
        (persp-add-buffer (current-buffer) (persp-get-by-name "@Lisp")))
      (add-hook 'emacs-lisp-mode-hook #'spacemacs-layouts/add-lisp-buffer-to-persp)
      (add-hook 'helpful-mode-hook #'spacemacs-layouts/add-lisp-buffer-to-persp)
      (call-interactively 'spacemacs/find-dotfile)
      (purpose-load-window-layout "elisp")
      ))

  (spacemacs|define-custom-layout "@Research"
    :binding "r"
    :body
    (progn
      (defun spacemacs-layouts/add-research-buffer-to-persp ()
        (persp-add-buffer (current-buffer) (persp-get-by-name "@Research")))
      ;; (add-hook 'org-mode-hook #'spacemacs-layouts/add-research-buffer-to-persp)
      (add-hook 'org-src-mode-hook #'spacemacs-layouts/add-research-buffer-to-persp)
      ;; (add-hook 'org-agenda-mode-hook #'spacemacs-layouts/add-research-buffer-to-persp)
      (add-hook 'inferior-ess-mode-hook #'spacemacs-layouts/add-research-buffer-to-persp)
      ;; (add-hook 'ess-mode-hook #'spacemacs-layouts/add-research-buffer-to-persp)
      (add-hook 'dired-mode-hook #'spacemacs-layouts/add-research-buffer-to-persp)
      (org-agenda nil "a")
      (purpose-load-window-layout "org-agenda")
      ))

  (spacemacs|define-custom-layout "@Elfeed"
    :binding "e"
    :body
    (progn
      (defun spacemacs-layouts/add-elfeed-buffer-to-persp ()
        (persp-add-buffer (current-buffer) (persp-get-by-name "@Elfeed")))
      (add-hook 'elfeed-search-mode-hook #'spacemacs-layouts/add-elfeed-buffer-to-persp)
      (add-hook 'elfeed-show-mode-hook #'spacemacs-layouts/add-elfeed-buffer-to-persp)
      (call-interactively 'elfeed)
      (purpose-load-window-layout "elfeed")
      ))




;;;; Prodigy
  (use-package prodigy
    :ensure t
    :init
    ;; After initialization, start these services
    :config
    ;; Tag
    ;; Define a new tag with ARGS.
    (prodigy-define-tag
      :name 'email
      :ready-message "Checking Email using IMAP IDLE. Ctrl-C to shutdown.")
    ;; Service
    ;; Define a new service with ARGS.
    (prodigy-define-service
      :name "imapnotify-gmail"
      :command "/usr/local/bin/imapnotify"
      :args (list "-c" (expand-file-name "./imapnotify.js" (getenv "HOME")))
      :tags '(email)
      :kill-signal 'sigkill)
    )

  (prodigy-start-service (prodigy-find-service "imapnotify-gmail"))

;;;; ESS
  (with-eval-after-load 'ess
    (defun ess-indent-region-with-formatR-tidy-source (beg end)
      "Format region of code R using formatR::tidy_source()."
      (interactive "r")
      (let ((string
             (replace-regexp-in-string
              "\"" "\\\\\\&"
              (replace-regexp-in-string ;; how to avoid this double matching?
               "\\\\\"" "\\\\\\&"
               (buffer-substring-no-properties beg end))))
            (buf (get-buffer-create "*ess-command-output*")))
        (ess-force-buffer-current "Process to load into:")
        (ess-command
         (format
          "local({
          formatR::tidy_source(text=\"\n%s\",
                               arrow=TRUE, width.cutoff=60) })\n"
          string) buf)
        (with-current-buffer buf
          (goto-char (point-max))
          ;; (skip-chars-backward "\n")
          (let ((end (point)))
            (goto-char (point-min))
            (goto-char (1+ (point-at-eol)))
            (setq string (buffer-substring-no-properties (point) end))
            ))
        (delete-region beg end)
        (insert string)
        (delete-backward-char 2)
        ))
    )

;;;; Applescript
;;;;; Helper function
  (defun applescript-quote-string (argument)
    "Quote a string for passing as a string to AppleScript."
    (if (or (not argument) (string-equal argument ""))
        "\"\""
      ;; Quote using double quotes, but escape any existing quotes or
      ;; backslashes in the argument with backslashes.
      (let ((result "")
            (start 0)
            end)
        (save-match-data
          (if (or (null (string-match "[^\"\\]" argument))
                  (< (match-end 0) (length argument)))
              (while (string-match "[\"\\]" argument start)
                (setq end (match-beginning 0)
                      result (concat result (substring argument start end)
                                     "\\" (substring argument end (1+ end)))
                      start (1+ end))))
          (concat "\"" result (substring argument start) "\"")))))

;;;;; Papers
  (defun export-papers-bib ()
    (interactive)
    (do-applescript
     (format
      "tell application id \"com.mekentosj.papers3\"
         set outFile to \"/Users/xfu/Dropbox/org/ref.bib\"
         export (every publication item of application id \"com.mekentosj.papers3\" as list) to outFile
       end tell"
      ))
    (start-process-shell-command "modify bib" nil "sleep 4;~/.emacs.d/private/local/cleanbib.sh")
    )

  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
          "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"))



;;;;; iTerm
(defun mac-iTerm-shell-command (text)
  "Write TEXT into iTerm like user types it with keyboard."
  (interactive
   (list
    (read-shell-command "Run Shell command in iTerm: "
                        (when (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))))
  (do-applescript
   (concat
    "tell application \"iTerm\"\n"
    "    activate\n"
    "    create window with default profile\n"
    "    tell current session of current window\n"
    "        write text \"" text "\"\n"
    "    end tell\n"
    "end tell")))

(defun mac-iTerm-shell-command-current (text)
  "Write TEXT into iTerm like user types it with keyboard."
  (interactive
   (list
    (read-shell-command "Run Shell command in iTerm: "
                        (when (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))))))
  (do-applescript
   (concat
    "tell application \"iTerm\"\n"
    "    activate\n"
    "    tell current session of current window\n"
    "        write text \"" text "\"\n"
    "    end tell\n"
    "end tell")))


(defun mac-iTerm-cd (dir)
  "Switch to iTerm and change directory there to DIR."
  (interactive (list
                ;; Because shell doesn't expand 'dir'
                (expand-file-name
                 (if current-prefix-arg
                     (read-directory-name "cd to: ")
                   default-directory))))
  (if (file-remote-p dir)
      (let* (
             (host (tramp-file-name-host (tramp-dissect-file-name dir)))
             (dir (tramp-file-name-localname (tramp-dissect-file-name dir)))
             (sshcmd (format "ssh %s" host))
             (cdcmd (format "cd %s" dir))
             )
        (mac-iTerm-shell-command sshcmd)
        (mac-iTerm-shell-command-current cdcmd)
        )
    (let ((cmd (format "cd %s" dir)))
      (mac-iTerm-shell-command cmd))
      )
  )

;;;;; Tyme2
  ;;   ;; (defun start-tyme ()
  ;;   ;;   (setq taskname (nth 4 (org-heading-components)))
  ;;   ;;   (do-applescript
  ;;   ;;    (format
  ;;   ;;     "set tsk to %s
  ;;   ;;     set proj to %s
  ;;   ;;     tell application \"Tyme2\"
  ;;   ;;     set judgep to false
  ;;   ;;     repeat with someproj in every project
  ;;   ;;       if ((name of someproj) is proj) then set judgep to true
  ;;   ;;     end repeat
  ;;   ;;     if (judgep is false) then make new project with properties {name:proj}
  ;;   ;;     end tell
  ;;   ;;     tell application \"Tyme2\"
  ;;   ;;     set judget to false
  ;;   ;;     repeat with sometsk in every task of project proj
  ;;   ;;       if ((name of sometsk) is tsk) then set judget to true
  ;;   ;;     end repeat
  ;;   ;;     if (judget is false) then make new task of (project proj) with properties {name:tsk, taskType:\"timed\", completed:false}
  ;;   ;;     end tell
  ;;   ;;     tell application \"Tyme2\"
  ;;   ;;     set t to id of the first item of (every task of project proj whose name = tsk)
  ;;   ;;     StartTrackerForTaskID t
  ;;   ;;     end tell"
  ;;   ;;     (applescript-quote-string taskname)
  ;;   ;;     (applescript-quote-string (file-name-base))
  ;;   ;;     )))
  ;;   ;; (defun stop-tyme ()
  ;;   ;;   (setq taskname (nth 4 (org-heading-components)))
  ;;   ;;   (do-applescript
  ;;   ;;    (format
  ;;   ;;     "set tsk to %s
  ;;   ;;        set proj to %s
  ;;   ;;        tell application \"Tyme2\"
  ;;   ;;        set judgep to false
  ;;   ;;        repeat with someproj in every project
  ;;   ;;          if ((name of someproj) is proj) then set judgep to true
  ;;   ;;        end repeat
  ;;   ;;        if (judgep is false) then make new project with properties {name:proj}
  ;;   ;;        end tell
  ;;   ;;        tell application \"Tyme2\"
  ;;   ;;        set judget to false
  ;;   ;;        repeat with sometsk in every task of project proj
  ;;   ;;          if ((name of sometsk) is tsk) then set judget to true
  ;;   ;;        end repeat
  ;;   ;;        if (judget is false) then make new task of (project proj) with properties {name:tsk, taskType:\"timed\", completed:false}
  ;;   ;;        end tell
  ;;   ;;        tell application \"Tyme2\"
  ;;   ;;        set t to id of the first item of (every task of project proj whose name = tsk)
  ;;   ;;        StopTrackerForTaskID t
  ;;   ;;        end tell"
  ;;   ;;     (applescript-quote-string taskname)
  ;;   ;;     (applescript-quote-string (file-name-base))
  ;;   ;;     )))
  ;;   ;; (add-hook 'org-clock-in-hook 'start-tyme)
  ;;   ;; (add-hook 'org-clock-out-hook 'stop-tyme)

;;;; Tramp
  (setq tramp-default-method "ssh")
  (setq tramp-ssh-controlmaster-options "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")
  (setq projectile-mode-line "Projectile")
  (setq tramp-remote-process-environment (quote ("TMOUT=0" "LC_CTYPE=''" "TERM=dumb" "INSIDE_EMACS='25.2.1,tramp:2.2.13.25.2'" "CDPATH=" "HISTORY=" "MAIL=" "MAILCHECK=" "MAILPATH=" "PAGER=cat" "autocorrect=" "correct=" "http_proxy=http://proxy.cse.cuhk.edu.hk:8000" "https_proxy=http://proxy.cse.cuhk.edu.hk:8000" "ftp_proxy=http://proxy.cse.cuhk.edu.hk:8000")))
  (with-eval-after-load 'tramp-sh
    (add-to-list 'tramp-remote-path "/research/kevinyip10/xfu/miniconda3/bin")
    (add-to-list 'tramp-remote-path "/uac/gds/xfu/bin")
    )

;;;; Ivy
  (with-eval-after-load 'ivy
    (defvar company-candidates)
    (defvar company-point)
    (defvar company-common)
    (declare-function company-complete "ext:company")
    (declare-function company-mode "ext:company")
    (declare-function company-complete-common "ext:company")

    (defun counsel-company-show-doc-buffer (selected)
      "Temporarily show the documentation buffer for the selection."
      (interactive)
      (let (other-window-scroll-buffer)
        (company--electric-do
          (let* ((doc-buffer (or (company-call-backend 'doc-buffer selected)
                                 (user-error "No documentation available")))
                 start)
            (when (consp doc-buffer)
              (setq start (cdr doc-buffer)
                    doc-buffer (car doc-buffer)))
            (setq other-window-scroll-buffer (get-buffer doc-buffer))
            (let ((win (display-buffer doc-buffer t)))
              (set-window-start win (if start start (point-min))))))))

    (defun counsel-company-action (str)
      "Insert STR, erasing the previous one.
The previous string is between `ivy-completion-beg' and `ivy-completion-end'."
      (when (consp str)
        (setq str (cdr str)))
      (when (stringp str)
        (let ((fake-cursors (and (fboundp 'mc/all-fake-cursors)
                                 (mc/all-fake-cursors)))
              (pt (point))
              (beg ivy-completion-beg)
              (end ivy-completion-end))
          (when ivy-completion-beg
            (delete-region
             ivy-completion-beg
             ivy-completion-end))
          (setq ivy-completion-beg
                (move-marker (make-marker) (point)))
          (insert (substring-no-properties str))
          (setq ivy-completion-end
                (move-marker (make-marker) (point)))
          (save-excursion
            (dolist (cursor fake-cursors)
              (goto-char (overlay-start cursor))
              (delete-region (+ (point) (- beg pt))
                             (+ (point) (- end pt)))
              (insert (substring-no-properties str))
              ;; manually move the fake cursor
              (move-overlay cursor (point) (1+ (point)))
              (move-marker (overlay-get cursor 'point) (point))
              (move-marker (overlay-get cursor 'mark) (point))))))
      (yas-expand)
      )

    (defun counsel-company ()
      "Complete using `company-candidates'."
      (interactive)
      (company-mode 1)
      (unless company-candidates
        (company-complete))
      (when company-point
        (when (looking-back company-common (line-beginning-position))
          (setq ivy-completion-beg (match-beginning 0))
          (setq ivy-completion-end (match-end 0)))
        (ivy-read "company cand: " company-candidates
                  :predicate (lambda (candidate)
                               (string-prefix-p prefix (car candidate)))
                  :caller 'counsel-company
                  :action 'counsel-company-action)))

    (add-to-list 'ivy-display-functions-alist '(counsel-company . ivy-display-function-overlay))
    (ivy-set-actions
     'counsel-company
     '(
       ("d" counsel-company-show-doc-buffer "Doc")
       ))


    (setq
     counsel-org-goto-face-style 'org
     counsel-org-goto-separator " ➜ "
     counsel-org-goto-display-style 'headline

     ivy-use-virtual-buffers t
     ivy-re-builders-alist '((t . ivy--regex-plus)))

    (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-call)

    (defun counsel-org-goto--get-headlines ()
  "Get all headlines from the current org buffer."
  (save-excursion
    (let (entries
          start-pos
          stack
          (stack-level 0))
      (goto-char (point-min))
      (setq start-pos (or (and (org-at-heading-p)
                               (point))
                          (outline-next-heading)))
      (while start-pos
        (let ((name (org-get-heading
                     (not counsel-org-goto-display-tags)
                     (not counsel-org-goto-display-todo)))
              level)
          (search-forward " ")
          (setq level
                (- (length (buffer-substring-no-properties start-pos (point)))
                   1))
          (cond ((eq counsel-org-goto-display-style 'path)
                 ;; Update stack. The empty entry guards against incorrect
                 ;; headline hierarchies e.g. a level 3 headline immediately
                 ;; following a level 1 entry.
                 (while (<= level stack-level)
                   (pop stack)
                   (cl-decf stack-level))
                 (while (> level stack-level)
                   (push "" stack)
                   (cl-incf stack-level))
                 (setf (car stack) (counsel-org-goto--add-face name level))
                 (setq name (mapconcat
                             #'identity
                             (reverse stack)
                             counsel-org-goto-separator)))
                (t
                 (when (eq counsel-org-goto-display-style 'headline)
                   (setq name (concat (make-string (* 2 level) ? ) (nth (- level 1) org-bullets-bullet-list) "  " name)))
                 (setq name (counsel-org-goto--add-face name level))))
          (push (cons name (point-marker)) entries))
        (setq start-pos (outline-next-heading)))
      (nreverse entries))))

    )
;;;; Dired
  (with-eval-after-load 'dired
    (require 'ivy-dired-history)
    (define-key dired-mode-map "f" 'dired-narrow)
    (define-key dired-mode-map "," 'dired))

;;;; EWW
  (with-eval-after-load 'shr
    (require 'shr-tag-pre-highlight)
    (add-to-list 'shr-external-rendering-functions
                 '(pre . shr-tag-pre-highlight)))

  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions)))


  (require 'ox-alex)
  ;; (require 'lsp-mode)
  ;; (require 'lsp-python)
  ;; (add-hook 'python-mode-hook #'lsp-python-enable)
  ;; (require 'company-lsp)
  ;; (push 'company-lsp company-backends)



  (add-hook 'anaconda-mode-hook #'python-mode-outline-hook)

  (add-to-list 'purpose-user-mode-purposes '(emacs-lisp-mode . el))
  (add-to-list 'purpose-user-mode-purposes '(helpful-mode . el-help))

  (add-to-list 'purpose-user-mode-purposes '(elfeed-search-mode . elfeed-search))
  (add-to-list 'purpose-user-mode-purposes '(elfeed-show-mode . elfeed-show))

  (add-to-list 'purpose-user-mode-purposes '(notmuch-hello-mode . nhello))
  (add-to-list 'purpose-user-mode-purposes '(notmuch-search-mode . nlist))
  (add-to-list 'purpose-user-mode-purposes '(notmuch-tree-mode . nlist))
  (add-to-list 'purpose-user-mode-purposes '(notmuch-show-mode . nshow))
  (add-to-list 'purpose-user-mode-purposes '(notmuch-message-mode . nmessage))

  (add-to-list 'purpose-user-regexp-purposes '("*Org Src*" . orgsrc))
  (add-to-list 'purpose-user-mode-purposes '(org-mode . org))
  (add-to-list 'purpose-user-mode-purposes '(org-agenda-mode . agenda))

  (add-to-list 'purpose-user-mode-purposes '(anaconda-mode-view-mode . doc))
  (add-to-list 'purpose-user-regexp-purposes '("*ob-ipython-inspect*" . doc))
  (add-to-list 'purpose-user-regexp-purposes '("*ob-ipython-out*" . out))
  (add-to-list 'purpose-user-regexp-purposes '("*ob-ipython-debug*" . out))
  (add-to-list 'purpose-user-regexp-purposes '("*ob-ipython-traceback*" . out))

  (purpose-compile-user-configuration)
;;;; Outline-level
      (defun python-mode-outline-hook ()
        (outline-minor-mode 1)
        (setq outline-level 'python-outline-level)
        (setq outline-regexp
              (rx (or
                   ;; Commented outline heading
                   (group
                    (* space)	 ; 0 or more spaces
                    (one-or-more (syntax comment-start))
                    (one-or-more space)
                    ;; Heading level
                    (group (repeat 1 8 "\*"))	 ; Outline stars
                    (one-or-more space))

                   ;; Python keyword heading
                   (group
                    ;; Heading level

                    ;; TODO: Try setting this to python-indent-offset
                    ;; instead of space.  Might capture the indention levels
                    ;; better.
                    (group (* space))	; 0 or more spaces
                    bow
                    ;; Keywords
                    (or "class" "def" "else" "elif" "except" "for" "if" "try" "while")
                    eow))))
        (define-key python-mode-map (kbd "C-j") 'counsel-oi)
        )

      (defun python-outline-level ()
        ;; Based on this code found at
        ;; http://blog.zenspider.com/blog/2013/07/my-emacs-setup-ruby-and-outline.html:
        ;; (or (and (match-string 1)
        ;;	     (or (cdr (assoc (match-string 1) outline-heading-alist))
        ;;		 (- (match-end 1) (match-beginning 1))))
        ;;	(and (match-string 0)
        ;;	     (cdr (assoc (match-string 0) outline-heading-alist)))

        ;; This doesn't work properly. It sort-of works, but it's not
        ;; correct. Running this function consecutively on the same line
        ;; sometimes returns different results. And it doesn't seem to
        ;; correctly recognize top-level Python functions or classes as
        ;; top-level headings, so subheadings beneath them don't collapse
        ;; properly.

        (or
         ;; Commented outline heading
         (and (string-match (rx
                             (* space)
                             (one-or-more (syntax comment-start))
                             (one-or-more space)
                             (group (one-or-more "\*"))
                             (one-or-more space))
                            (match-string 0))
              (- (match-end 0) (match-beginning 0)))

         ;; Python keyword heading, set by number of indentions
         ;; Add 8 (the highest standard outline level) to every Python keyword heading
         (+ 8 (- (match-end 0) (match-beginning 0)))))

      (defun ap/sh-outline-level ()
        (or
         ;; Commented outline heading
         (and (string-match (rx
                             (* space)
                             (one-or-more (syntax comment-start))
                             (one-or-more space)
                             (group (one-or-more "\*"))
                             (one-or-more space))
                            (match-string 0))
              (- (match-end 1) (match-beginning 1) 1))

         ;; Keyword/function heading
         ;; Add 8 (the highest standard outline level) to every keyword
         ;; heading
         (+ 8 (- (match-end 3) (match-beginning 3)))))

      (defun ap/sh-mode-outline-hook ()
        (outline-minor-mode 1)
        (setq outline-level 'ap/sh-outline-level)
        (setq outline-regexp (rx (group (or
                                         ;; Outline headings
                                         (and (* space)
                                              (one-or-more (syntax comment-start))
                                              (* space)
                                              (group (one-or-more "\*"))
                                              (* space))

                                         ;; Keywords and functions
                                         (and (group (* space))
                                              (or
                                               ;; e.g. "function foo"
                                               (and (or "function" "if" "elif" "else" "for" "while")
                                                    (one-or-more space))

                                               ;; e.g. "foo()"
                                               (and (one-or-more (or alnum "_-"))
                                                    (* space)
                                                    (syntax open-parenthesis)
                                                    (syntax close-parenthesis)))))))))
      (add-hook 'sh-mode-hook 'ap/sh-mode-outline-hook)

      (defun ap/el-outline-level ()
        (or
         ;; Commented outline heading
         (and (string-match (rx
                             (* space)
                             (group (one-or-more (syntax comment-start)))
                             (one-or-more space))
                            (match-string 0))
              (- (match-end 0) (match-beginning 0) 1))

         ;; Lisp def heading
         ;; Add 8 (the highest standard outline level) to every keyword
         ;; heading
         (+ 8 (- (match-end 0) (match-beginning 0)))))
      (defun ap/el-mode-outline-hook ()
        (outline-minor-mode 1)
        (setq outline-level 'ap/el-outline-level)
        (setq outline-regexp "\\(;;[;]\\{1,8\\} \\|\\((defun\\)\\)"))
      (add-hook 'emacs-lisp-mode-hook 'ap/el-mode-outline-hook)

      (defun ap/general-outline-level ()
        (or
         ;; Commented outline heading
         (and (string-match (rx
                             (* space)
                             (one-or-more (syntax comment-start))
                             (one-or-more space)
                             (group (one-or-more "\*"))
                             (one-or-more space))
                            (match-string 0))
              (- (match-end 1) (match-beginning 1) 1))))

      (defun ap/general-outline-mode-enable ()
        (interactive)
        (outline-minor-mode 1)
        (setq outline-level 'ap/general-outline-level)
        (setq outline-regexp (rx (group (* space)
                                        (one-or-more (syntax comment-start))
                                        (* space)
                                        (group (one-or-more "\*"))
                                        (* space)))))

      (mac-auto-operator-composition-mode)

  )

;;; Customize
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (prettify-utils yapfify ws-butler winum which-key wgrep volatile-highlights uuidgen use-package unfill treemacs-projectile treemacs-evil toc-org tiny string-inflection smex smeargle shx shrink-path shr-tag-pre-highlight reveal-in-osx-finder restart-emacs request rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pytest pyenv-mode py-isort prodigy popwin pip-requirements persp-mode pcre2el pbcopy password-generator paradox pandoc-mode ox-twbs ox-pandoc outshine osx-trash osx-dictionary orgit org-web-tools org-super-agenda org-present org-pomodoro org-mime org-edit-latex org-download org-bullets org-brain org-bookmark-heading open-junk-file olivetti ob-ipython ob-async notmuch-labeler mwim move-text modern-solarizedlight-theme mmm-mode markdown-toc macrostep live-py-mode link-hint launchctl langtool kurecolor ivy-purpose ivy-hydra ivy-dired-history ivy-bibtex insert-shebang info+ indent-guide hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers hide-comnt helpful help-fns+ google-translate golden-ratio gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ fuzzy flyspell-correct-ivy flycheck-package flycheck-bashate flx fill-column-indicator eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-ediff evil-args evil-anzu eval-sexp-fu ess-smart-equals ess-R-data-view elisp-slime-nav elfeed-org editorconfig dumb-jump dired-narrow diminish diff-hl cython-mode counsel-projectile company-statistics company-auctex company-anaconda column-enforce-mode color-identifiers-mode cdlatex browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile auctex-latexmk all-the-icons aggressive-indent adaptive-wrap ace-link ac-ispell)))
 '(tramp-syntax (quote default) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
