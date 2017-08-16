(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (csv-mode auctex yapfify xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights virtualenvwrapper uuidgen use-package unfill toc-org tagedit symon string-inflection spaceline solarized-theme smex smeargle slim-mode shell-pop scss-mode sass-mode reveal-in-osx-finder restart-emacs request rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode prettify-utils popwin pip-requirements persp-mode pcre2el pbcopy password-generator paradox pandoc-mode ox-pandoc outshine osx-trash osx-dictionary orgit org-ref org-projectile org-present org-pomodoro org-download org-bullets org-brain open-junk-file ob-async neotree mwim multi-term move-text mmm-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl langtool json-mode js2-refactor js-doc ivy-rich ivy-purpose ivy-hydra insert-shebang info+ indent-guide impatient-mode ibuffer-projectile hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make graphviz-dot-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-ivy flycheck-pos-tip flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu eval-sexp-fu ess-smart-equals ess-R-object-popup ess-R-data-view eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav editorconfig dumb-jump doom-themes cython-mode counsel-projectile company-web company-tern company-statistics company-shell company-quickhelp company-lua company-anaconda column-enforce-mode coffee-mode clean-aindent-mode auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile all-the-icons-ivy aggressive-indent adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background-face ((t (:foreground "#586e75"))))
 '(company-tooltip ((t (:inherit tooltip :background "black"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(flyspell-duplicate ((t (:underline "DarkOrange"))))
 '(flyspell-incorrect ((t (:underline "maroon2"))))
 '(font-lock-comment-face ((t (:foreground "#586e75" :italic t))))
 '(font-lock-doc-face ((t (:foreground "#2aa198" :italic t))))
 '(highlight ((t (:inherit default :background "#353a49" :foreground "#adb5c4"))))
 '(ivy-current-match ((t (:inherit nil :background "#21242b" :weight black))))
 '(org-agenda-calendar-sexp ((t (:inherit org-agenda-diary))))
 '(org-agenda-current-time ((t (:inherit org-time-grid :foreground "gray100"))))
 '(org-agenda-date ((t (:inherit org-agenda-structure :foreground "SkyBlue1" :overline nil :underline nil))))
 '(org-agenda-diary ((t (:inherit org-agenda-done :foreground "gray86"))))
 '(org-agenda-done ((t (:inherit org-done))))
 '(org-agenda-structure ((t (:inherit org-level-3 :foreground "LightSkyBlue" :overline t :underline t :slant italic :family "input mono compressed"))))
 '(org-block ((t (:background "#262b33" :weight normal :family "operator mono"))))
 '(org-block-begin-line ((t (:height 1.05 :foreground "#576e75" :box t :weight bold))))
 '(org-column ((t (:strike-through nil :underline nil :slant normal :weight normal))))
 '(org-date ((t (:foreground "#a9a1e1" :weight normal :height 0.85 :family "input mono compressed"))))
 '(org-date-selected ((t (:inherit org-date :foreground "Pink" :inverse-video t))))
 '(org-default ((t (:inherit variable-pitch :height 1.0))))
 '(org-kbd ((t (:background "#323944" :foreground "white"))))
 '(org-level-1 ((t (:background "#23272e" :foreground "#51afef" :weight normal :height 1.2 :family "operator ssm"))))
 '(org-level-3 ((t (:foreground "DarkSlateGray3" :weight normal :family "operator ssm"))))
 '(org-level-4 ((t (:inherit org-level-3 :foreground "PeachPuff3"))))
 '(org-level-5 ((t (:inherit org-level-3 :foreground "light sea green"))))
 '(org-level-6 ((t (:inherit org-level-3 :foreground "RosyBrown3"))))
 '(org-priority ((t (:inherit nil :foreground "#ff6c6b" :height 0.85 :family "input mono compressed"))))
 '(org-ref-cite-face ((t (:foreground "DeepSkyBlue2" :underline t))))
 '(org-scheduled ((t (:inherit org-level-3 :foreground "MediumPurple2"))))
 '(org-scheduled-previously ((t (:inherit org-level-3 :foreground "DeepSkyBlue2"))))
 '(org-scheduled-today ((t (:inherit org-level-3 :foreground "MediumPurple1"))))
 '(org-table ((t (:background "#262b33" :foreground "#a9a1e1" :family "iosevka"))))
 '(org-tag ((t (:slant italic :weight bold))))
 '(org-time-grid ((t (:inherit org-agenda-done))))
 '(org-upcoming-deadline ((t (:inherit org-level-3 :foreground "HotPink1"))))
 '(org-warning ((t (:inherit org-level-3 :foreground "VioletRed1" :slant italic))))
 '(outline-1 ((t (:height 1.25 :foreground "#C3A29E" :weight ultra-bold :italic t :underline t))))
 '(outline-2 ((t (:height 1.15 :foreground "#8D6B94" :weight extra-bold :italic t :underline t))))
 '(outline-3 ((t (:height 1.15 :foreground "#8C5F66" :weight bold :italic t :underline t))))
 '(sp-show-pair-match-face ((t (:inherit sp-show-pair-match-face :background "#586e75"))))
 '(variable-pitch ((t (:weight semi-light :family "operator ssm")))))

