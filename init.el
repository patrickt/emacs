;;; init.el -- Patrick Thomson's emacs config

;;; Commentary:
;; This file is in the public domain.

;;; Code:

;; Temporarily disable GC limits.

(defvar old-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000)

;; Package-initialization preamble.

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

;; Fullscreen by default, as early as possible.

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Start with split windows

(split-window-horizontally)
(other-window 1)

;; Open my TODO on Mac

(when (eq system-type 'darwin)
  (find-file "~/txt/semantic.org"))

(other-window 1)

;; Use Operator Mono, my favorite monospaced font, handling its absence gracefully.

(ignore-errors
  (set-frame-font "Fira Code Retina-14"))

;; Any Customize-based settings should live in custom.el, not here.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Important preamble settings: use lexical scope and prefer newer files.

(setq
 lexical-binding t
 load-prefer-newer t)

;; Disable otiose GUI settings: they just waste space.

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Not sure if this is necessary, but it has no performance hit, so…

(eval-when-compile
  (require 'use-package)
  (require 'bind-key))

(use-package diminish
  :ensure t)

;; Apropospriate is easy on the eyes.

(use-package material-theme
  :ensure t
  :init
  (load-theme 'material))

;; I use this for prose writing to ensure I hit a minimum goal for the day.

(use-package wc-goal-mode
  :defer t
  :ensure t)

;; Ace-window is a nice way to switch between frames quickly.

(use-package ace-window
  :defer t
  :ensure t
  :bind (("C-," . ace-window)))

;; The default modeline is ugly. Until spacemakes makes their modeline into a
;; package that the rest of use can use, powerline will suffice.

(use-package powerline
  :ensure t
  :init (powerline-default-theme))

;; Ensure that items in the PATH are made available to Emacs. This should
;; probably just come with the main distribution.

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; Recentf comes with Emacs but it should always be enabled.

(use-package recentf
  :init (recentf-mode t)
  :config (add-to-list 'recentf-exclude "\\.emacs.d"))

;; Some people object to helm, but I love it like a family member.
;; TODO: bind C-s to helm-occur.

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-c ;"   . helm-M-x)
         ("C-c r"   . helm-recentf)
         ("C-c y"   . helm-show-kill-ring)
         ("C-c b"   . helm-mini)
         ("C-s"     . helm-occur)
 	 ("C-x C-f" . helm-find-files)
         ("C-c i"   . helm-imenu)
         ("C-x b"   . helm-mini))
  :config
  (helm-mode t)
  (helm-autoresize-mode t)
  (helm-adaptive-mode t)
  (setq-default helm-M-x-fuzzy-match t))

;; Keychain stuff. Note to self: if you keep having to enter your
;; keychain password on OS X, make sure that you have the following in .ssh/config:
;; Host *
;;    UseKeychain yes


(use-package keychain-environment
  :ensure t
  :init (keychain-refresh-environment))

;; Company has its issues but is overall the most modern autocomplete
;; facility for Emacs.

(use-package company
  :ensure t
  :defer t
  :init (global-company-mode 1)
  :bind (("M-/" . company-dabbrev))
  :diminish company-mode
  :config
  ;; It completes a little too aggressively out of the box. Slow down, champ.
  (setq company-minimum-prefix-length 4
        company-idle-delay 1
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next))

;; restclient-mode is an essential tool for interacting with HTTP APIs.
;; Since I rarely, if ever, save a restclient buffer, the :mode matcher
;; enables when I create an ephemeral buffer named 'restclient'.
(use-package restclient
  :ensure t
  :mode ("restclient" . restclient-mode))

;; SQL-mode is very good. To avoid having to type in usernames and database
;; names every time, customize sql-user and sql-database.
(use-package sql
  :ensure t
  :config
  (setq
   sql-server "localhost"))

;; I honestly don't know if this does anything or not, but hey, it can't hurt.

(use-package company-restclient
  :ensure t
  :defer restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

;; Prodigy is an amazing process manager. I used to have a lot more here for
;; stuff at my last job, but I'm sure I'll fill this out more in the future.

(use-package prodigy
  :ensure t
  :defer t
  :bind (("C-c q" . prodigy)))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-c n" . er/expand-region)))

;; helm can look inside makefiles if you have helm-make enabled.
;; turning this off for now, though.

(use-package helm-make
  :disabled
  :bind (("C-c m" . helm-make-projectile))
  :config (setq helm-make-sort-targets t))

(bind-key "C-c m" 'compile)

;; I should use god-mode more often, but I tend to forget it exists.

(use-package god-mode
  :ensure t
  :bind (("C-c <SPC>" . god-mode-all))
  :config
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor))

(defun my-update-cursor ()
  "Use a block cursor if god-mode is on."
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                        'bar)))

;; Ripgrep is fast as heck.

(use-package helm-ag
  :ensure t
  :defer helm
  :bind (("C-c G" . helm-do-ag-project-root)
	 ("C-c h" . helm-do-ag-project-root))
  :config
  (setq helm-ag-base-command "rg --no-heading"
	helm-ag-insert-at-point 'symbol
	helm-ag-fuzzy-match t))

;; Keeping helm-git-grep around for when I need case-insensitive search.

(use-package helm-git-grep
  :ensure t
  :bind (("C-c H" . helm-git-grep)))

;; I don't always write Go… but when I do, I complain mightily.

(use-package go-mode
  :ensure t
  :defer helm-ag
  :config
  (setq helm-ag-base-command "rg --no-heading")
  (add-hook 'before-save-hook #'gofmt-before-save))

;; Projectile comes with Emacs, and is pretty essential. All these functions
;; hook into helm for their UI.

(use-package projectile
  :ensure t
  :bind (("C-c f" . projectile-find-file)
         ("C-x f" . projectile-find-file) ; overwrites set-fill-column
         ("C-c c" . projectile-compile-project))
  :init (projectile-global-mode)
  :config (setq projectile-completion-system 'helm
                projectile-enable-caching t)
  :diminish projectile-mode)

;; Magit is one of the best pieces of OSS I have ever used. It is truly esssential.

(use-package magit
  :defer t
  :ensure t
  :bind (("C-c g" . magit-status))
  :init
  (global-auto-revert-mode t)
  (diminish auto-revert-mode)
  (add-hook 'magit-mode-hook 'display-line-numbers-mode)
  (add-hook 'magit-mode-hook '(lambda () (define-key git-commit-mode-map "`" 'electric-pair)))
  ;; There is an extant bug where magit-refresh prompts to save files that haven't
  ;; been modified. We work around this with some defadvice over maybe-unset-buffer-modified. SO:
  ;; https://emacs.stackexchange.com/questions/24011/make-emacs-diff-files-before-asking-to-save

  (defun current-buffer-matches-file-p ()
    "Return t if the current buffer is identical to its associated file."
    (autoload 'diff-no-select "diff")
    (when buffer-file-name
      (diff-no-select buffer-file-name (current-buffer) nil 'noasync)
      (with-current-buffer "*Diff*"
        (and (search-forward-regexp "^Diff finished \(no differences\)\." (point-max) 'noerror) t))))

  (defun maybe-unset-buffer-modified ()
    "Clear modified bit on all unmodified buffers."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and buffer-file-name (buffer-modified-p))
          (when (current-buffer-matches-file-p)
            (set-buffer-modified-p nil))))))

  (advice-add 'magit-refresh :before #'maybe-unset-buffer-modified)
  :config
  (setq-default magit-last-seen-setup-instructions "1.4.0"))

;; This is a feature that someone stole from Sublime Text - show git information in
;; the gutter. It's pretty nice.

(use-package git-gutter
  :disabled
  :ensure t
  :init (global-git-gutter-mode)
  :config (setq git-gutter:update-interval 1)
  :diminish git-gutter-mode)

;; Since I grew up on Textmate, I'm more-or-less reliant on snippets. It uses helm
;; when there is an ambiguiity as to which snippet is appropriate, which is nice.

(use-package yasnippet
  :ensure t
  :defer 3
  :diminish yas-minor-mode
  :config
  (yas-global-mode +1)
  (setq yas-prompt-functions '(yas-completing-prompt)))

;; I usually don't edit very large files, but saveplace is nice on the occasions I do.

(use-package saveplace
  :config (setq-default save-place t))

;; Haskell and Elisp are made a lot easier when delimiters are nicely color-coded.

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Common Haskell snippets. These take a while to load, so no need to block on startup.

(use-package haskell-snippets
  :defer yasnippet
  :ensure t)

;; The beauty of undo-tree is that it means that, once you've typed something into
;; a buffer, you'll always be able to get it back. That is crucial.

(use-package undo-tree
  :defer t
  :ensure t
  :bind (("C-c _" . undo-tree-visualize))
  :init (global-undo-tree-mode +1)
  :diminish undo-tree-mode)

(defun my-c-mode-hook ()
  "C initialization."
  (setq indent-tabs-mode ni.))

;; C stuff.
(use-package cc-mode)

;; I'm trying to wean myself off of zsh/Terminal.app, with limited success.

(use-package eshell
  :bind (("C-c s" . eshell)
         ("C-r" . helm-eshell-history)))

;; I do all of my writing in either org-mode or markdown-mode.

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$" . markdown-mode)
  :config
  (remove-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'markdown-mode-hook '(lambda () (linum-mode -1)))
  (unbind-key "M-<left>" markdown-mode-map)
  (unbind-key "M-<right>" markdown-mode-map))

(use-package pandoc-mode
  :ensure t
  :defer markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  (add-hook 'markdown-mode-hook 'toggle-word-wrap))

;; YAML is underappreciated.

(use-package yaml-mode
  :defer t
  :ensure t)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (add-hook 'lisp-mode-hook 'slime-setup)
  (add-to-list 'slime-contribs 'slime-repl))

(use-package slime-company
  :defer slime)

;; Ace-jump is much nicer than goto-line (which you can get to with M-g g), so
;; I overwrite the C-l binding.

(use-package ace-jump-mode
  :ensure t
  :bind (("C-l"   . ace-jump-line-mode)
         ("C-c j" . ace-jump-mode)))

;; Sometimes being able to jump quickly through a bunch of helm completions
;; is nice, though I usually forget that this exists.

(use-package ace-jump-helm-line
  :ensure t
  :defer helm
  :bind (("C-'" . ace-jump-helm-line)))

;; Quickly duplicate whatever's under the cursor. I'm shocked this requires a
;; third-party package; it should be standard.

(use-package duplicate-thing
  :ensure t
  :bind (("C-c u" . duplicate-thing)))

(use-package purescript-mode
  :defer t
  :ensure t)

;; I also forget that this exists a lot, but it's nice when asking "why is emacs slow".

(use-package memory-usage
  :ensure t)

;; I can never remember the hierarchies of certain bindings, like C-x v for version control
;; stuff. Guide-key helps there. (TODO: figure out other places it'll help.)

(use-package guide-key
  :ensure t
  :init (guide-key-mode +1)
  :config (setq guide-key/guide-key-sequence '("C-x v" "C-c a"))
  :diminish guide-key-mode)

;; Since the in-emacs Dash browser doesn't owrk on OS X, we have to settle for dash-at-point.

(use-package dash-at-point
  :ensure t
  :disabled
  :bind ("C-c d" . dash-at-point))

(use-package dumb-jump
  :ensure t
  :bind (("C-c d" . dumb-jump-go)
	 ("C-c D" . dumb-jump-go-prompt))
  :config (setq dumb-jump-selector 'helm))

;; I rarely have to edit Lua anymore.

(use-package lua-mode
  :disabled)

;; OCaml.

(ignore-errors
  (load (expand-file-name "~/.opam/system/share/emacs/site-lisp/tuareg-site-file")))

(defun em-dash ()
  "Insert an em-dash."
  (interactive)
  (insert "—"))

(defun ellipsis ()
  "Insert an ellipsis."
  (interactive)
  (insert "…"))

;; I do a lot of writing in org-mode, though I have yet to truly take advantage
;; of its enormous power. It steps on a few of my keybindings, so we take care of
;; those with unbind-key.

(use-package org
  :bind (("M--" . em-dash)
         ("M-;" . ellipsis)
         ("C-c w" . wc-goal-mode))
  :config
  (defun my-org-mode-hook ()
    (wc-goal-mode)
    (visual-line-mode)
    (electric-pair-mode nil))
  (unbind-key "C-c ;" org-mode-map)
  (unbind-key "C-,"   org-mode-map)
  (add-hook 'org-mode-hook 'my-org-mode-hook)
  (setq org-src-fontify-natively t
	org-agenda-files (list "~/Dropbox/todo.org")))

;; Flycheck is very useful, though in order for it to work properly in a lot of
;; Haskell files we need to enable a bunch of default extensions.

(use-package flycheck
  :disabled
  :ensure t
  :defer helm-mode
  :init (global-flycheck-mode)
  :bind ("C-c n" . flycheck-next-error)
  :config
  (setq flycheck-ghc-language-extensions (append
                                          flycheck-ghc-language-extensions
                                          '("TemplateHaskell"
                                          "OverloadedStrings"
                                          "QuasiQuotes"
                                          "FlexibleContexts"
                                          "GeneralizedNewtypeDeriving"
                                          "DeriveGeneric"
                                          "MultiParamTypeClasses"
                                          "FunctionalDependencies"
                                          "FlexibleInstances"
                                          "RecordWildCards"
                                          "ScopedTypeVariables"
                                          "TypeFamilies"
                                          "DeriveDataTypeable"))))

(defun haskell-right-arrow ()
  "Insert a right arrow."
  (interactive)
  (insert (if (eolp) " -> " "->")))

(defun haskell-left-arrow ()
  "Insert a left arrow."
  (interactive)
  (insert (if (eolp) " <- " "<-")))

(use-package haskell-mode
  :ensure t
  :bind (("C-c a c" . haskell-cabal-visit-file)
	 ("C-c a b" . haskell-mode-stylish-buffer)
         ("C-c a i" . haskell-navigate-imports)
         ("C-c a ," . haskell-left-arrow)
         ("C-c a ." . haskell-right-arrow))
  :init
  (defun my-haskell-mode-hook ()
    "Make sure the compile command is right."
    (setq-local compile-command "stack build --fast")
    (setq-local helm-ag-base-command "rg --no-heading -t haskell")
    (mac-auto-operator-composition-mode))
  (defun my-lithaskell-mode-hook ()
    "Turn off auto-indent for Literate Haskell snippets."
    (setq-local yas-indent-line nil))
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (add-hook 'literate-haskell-mode-hook 'my-lithaskell-mode-hook)
  :config
  (defalias 'haskell-completing-read-function 'helm--completing-read-default)
  (defalias 'haskell-complete-module-read 'helm--completing-read-default))

(use-package intero
  :ensure t
  :defer haskell-mode
  :bind (("C-c a r" . intero-repl)
         ("C-c a j" . intero-goto-definition)
         ("C-c a n" . intero-info)
         ("C-c a t" . intero-type-at)
         ("C-c a u" . intero-uses-at)
         ("C-c a s" . intero-apply-suggestions))
  :config
  (add-hook 'haskell-mode-hook 'global-intero-mode))

(use-package idris-mode
  :ensure t
  :bind (("C-c C-v" . idris-case-split)))

(use-package typescript-mode
  :ensure t)

(defun my-elisp-mode-hook ()
  "My elisp customizations."
  (electric-pair-mode)
  (eldoc-mode t)
  (diminish 'eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(defadvice isearch-search (after isearch-no-fail activate)
  "Automatically wrap around in search results."
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file user-init-file))

(bind-key "C-c e" 'open-init-file)
(bind-key "C-c o" '(lambda () (interactive) (find-file "~/txt/semantic.org")))

(defun kill-all-buffers ()
  "Close all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(bind-key "C-c k" 'kill-all-buffers)

(defun split-right-and-enter ()
  "Split the window to the right and enter it."
  (interactive)
  (split-window-right)
  (other-window 1))

(bind-key "C-c 3" 'split-right-and-enter)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun display-startup-echo-area-message ()
  "Overrides the normally tedious error message."
  (message "Welcome back."))

(bind-key "C-c '"  'switch-to-previous-buffer)

(defun eol-then-newline ()
  "Go to end of line then return."
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

(bind-key "s-<return>"	'eol-then-newline)
(bind-key "C-c l"	'goto-line)
(bind-key "C-c 5"	'query-replace-regexp)
(bind-key "C-c \\"	'align-regexp)
(bind-key "C-c /"	'comment-or-uncomment-region)
(bind-key "C-c x"	'ESC-prefix)
(bind-key "s-+"		'text-scale-increase)
(bind-key "s-_"		'text-scale-decrease)
(bind-key "s-/"		'hippie-expand)
(bind-key "s-s"         'save-buffer)
(bind-key "s-c"		'kill-ring-save)
(bind-key "s-v"		'yank)
(bind-key "s-z"		'undo)
(bind-key "s-a"		'mark-whole-buffer)
(bind-key "s-<"         'beginning-of-buffer)
(bind-key "<home>"      'beginning-of-buffer)
(bind-key "<end>"       'end-of-buffer)
(bind-key "s->"         'end-of-buffer)

(unbind-key (kbd "<prior>"))
(unbind-key (kbd "<next>"))

(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)   ; Always highlight the current line.
(show-paren-mode t)       ; And point out matching parentheses.
(delete-selection-mode t) ; Behave like any other sensible text editor would.
(column-number-mode t)    ; Show column information in the modeline.
(display-time-mode t)     ; Show the current time, though I never use this.
(auto-save-mode -1)       ; Don't litter everywhere with backups.
(prettify-symbols-mode)   ; Use pretty Unicode symbols where possible.
(global-display-line-numbers-mode)


(setq
 blink-matching-paren t            ; Flash the opening paren when a closer is inserted.
 compilation-always-kill t         ; Never prompt to kill a compilation session.
 compilation-scroll-output t       ; Always scroll to the bottom.
 create-lockfiles nil              ; Emacs sure loves to put lockfiles everywhere.
 default-directory "~/src"         ; My code lives here.
 inhibit-startup-screen t          ; No need to see GNU agitprop.
 mac-mouse-wheel-smooth-scroll nil ; Smooth-scrolling is way too slow.
 kill-whole-line t                 ; Delete the whole line if C-k is hit at the beginning of a line.
 make-backup-files nil             ; No backups, thanks.
 mac-command-modifier 'super       ; I'm not sure this is the right toggle, but whatever.
 require-final-newline t           ; Auto-insert trailing newlines.
 ring-bell-function 'ignore        ; Do not ding. Ever.
 linum-delay t                     ; No need to slow down emacs when recalculating line numbers.
 use-dialog-box nil                ; Dialogues always go in the modeline.
 indent-tabs-mode nil              ; Fuck tabs.
 frame-title-format "emacs – %b"   ; Put something useful in the status bar.
 initial-scratch-message nil       ; SHUT UP SHUT UP SHUT UP
 mac-option-modifier 'meta
 ;; Scroll amounts are way too high on mac. This does something to ameliorate the situation.
 mouse-wheel-scroll-amount '(3 ((shift) . 5))
 ;; Save system copy/paste commands to the kill ring.
 save-interprogram-paste-before-kill t)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; Bar cursors everywhere.

(setq-default cursor-type 'bar)
(setq-default indent-tabs-mode nil)

;; Always trim trailing whitespace.

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Restore original GC threshold.
(setq gc-cons-threshold old-cons-threshold)
(makunbound 'old-cons-threshold)

(provide 'init)

;; goodbye, thanks for reading

;;; init.el ends here
