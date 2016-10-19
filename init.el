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

;; Use Operator Mono, my favorite monospaced font.
;; TODO: handle its absence gratefully.

(set-frame-font "Operator Mono-11")

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
  (require 'bind-key)
  (require 'diminish))

;; Apropospriate is easy on the eyes.

(use-package apropospriate-theme
  :ensure t
  :init
  (load-theme 'apropospriate-dark))

;; I use this for prose writing to ensure I hit a minimum goal for the day.

(use-package wc-goal-mode
  :ensure t)

;; Ace-window is a nice way to switch between frames quickly.

(use-package ace-window
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
  :init (exec-path-from-shell-initialize))

;; Recentf comes with Emacs but it should always be enabled.

(use-package recentf
  :init (recentf-mode t)
  :config (add-to-list 'recentf-exclude "\\.emacs.d"))

;; Anzu is small but very nice: during searches, it shows how many matches
;; are found in the current buffer.

(use-package anzu
  :defer
  :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode +1))

;; Some people object to helm, but I love it like a family member.
;; TODO: bind C-s to helm-occur.

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-c ;" . helm-M-x)
         ("C-c r" . helm-recentf)
         ("C-c y" . helm-show-kill-ring)
         ("C-c b" . helm-mini)
         ("C-c S" . helm-occur)
         ("C-c i" . helm-imenu)
         ("C-x b" . helm-mini))
  :config
  (helm-mode t)
  (helm-autoresize-mode t)
  (helm-adaptive-mode t)
  (setq-default helm-M-x-fuzzy-match t))

;; Company has its issues but is overall the most modern autocomplete
;; facility for Emacs.

(use-package company
  :ensure t
  :init (global-company-mode 1)
  :bind (("M-/" . company-complete))
  :diminish company-mode
  :config
  ;; It completes a little too aggressively out of the box. Slow down, champ.
  (setq company-minimum-prefix-length 4
        company-idle-delay 0.05)
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

;; Intelligently sort company predictions based on buffer statistics. This can
;; make load-time a little slow, so be sure to :defer it.

(use-package company-statistics
  :defer t
  :ensure t
  :init (company-statistics-mode +1))

;; Prodigy is an amazing process manager. I used to have a lot more here for
;; stuff at my last job, but I'm sure I'll fill this out more in the future.

(use-package prodigy
  :ensure t
  :bind (("C-c q" . prodigy)))

;; helm can look inside makefiles if you have helm-make enabled.

(use-package helm-make
  :ensure t
  :bind (("C-c m" . helm-make-projectile))
  :config (setq helm-make-sort-targets t))

;; I should use god-mode more often, but I tend to forget it exists.

(use-package god-mode
  :ensure t
  :bind ("C-c <SPC>" . god-mode-all)
  :config
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor))

(defun my-update-cursor ()
  "Use a block cursor if god-mode is on."
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                        'bar)))

;; Quick access to git-grep is a must. Helm's support for it is nice.
;; Though helm-git-grep-at-point is annoyingly buggy.

(use-package helm-git-grep
  :ensure t
  :bind (("C-c G" . helm-git-grep)
         ("C-c h" . helm-git-grep-at-point)))

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

;; I'm sure somebody out there doesn't want line numbers in their gutters.
;; But that person is not me.

(use-package linum
  :init (global-linum-mode t)
  :config (setq linum-format "%d"))

;; Magit is one of the best pieces of OSS I have ever used. It is truly esssential.

(use-package magit
  :defer t
  :ensure t
  :bind (("C-c g" . magit-status))
  :init (global-auto-revert-mode t)
  :config (setq-default magit-last-seen-setup-instructions "1.4.0"))

;; This is a feature that someone stole from Sublime Text - show git information in
;; the gutter. It's pretty nice.

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode)
  :config (setq git-gutter:update-interval 1)
  :diminish git-gutter-mode)

;; Since I grew up on Textmate, I'm more-or-less reliant on snippets. It uses helm
;; when there is an ambiguiity as to which snippet is appropriate, which is nice.

(use-package yasnippet
  :ensure t
  :defer 1
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
  :defer 3
  :ensure t)

;; I'm still not happy with my Haskell indentation setup, but hi2 is nice and reasonably
;; customizable, so I'll take it for now.

(use-package hi2
  :defer haskell-mode
  :ensure t
  :diminish hi2-mode
  :config
  (setq hi2-show-indentations t
        hi2-layout-offset 4
        hi2-starter-offset 4
        hi2-left-offset 4
        hi2-show-indentations t
        hi2-show-indentations-after-eol t))

;; The beauty of undo-tree is that it means that, once you've typed something into
;; a buffer, you'll always be able to get it back. That is crucial.

(use-package undo-tree
  :ensure t
  :bind (("C-c _" . undo-tree-visualize))
  :init (global-undo-tree-mode +1)
  :diminish undo-tree-mode)

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
  (unbind-key "M-<left>" markdown-mode-map)
  (unbind-key "M-<right>" markdown-mode-map))

;; YAML is underappreciated.

(use-package yaml-mode
  :ensure t)

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

;; I also forget that this exists a lot, but it's nice when asking "why is emacs slow".

(use-package memory-usage
  :ensure t)

;; I can never remember the hierarchies of certain bindings, like C-x v for version control
;; stuff. Guide-key helps there. (TODO: figure out other places it'll help.)

(use-package guide-key
  :ensure t
  :init (guide-key-mode +1)
  :config (setq guide-key/guide-key-sequence '("C-x v"))
  :diminish guide-key-mode)

;; Since the in-emacs Dash browser doesn't owrk on OS X, we have to settle for dash-at-point.

(use-package dash-at-point
  :ensure t
  :bind ("C-c d" . dash-at-point))

;; I rarely have to edit Lua anymore.

(use-package lua-mode)

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
    (visual-line-mode)
    (setq org-src-fontify-natively t))
  (unbind-key "C-c ;" org-mode-map)
  (unbind-key "C-,"   org-mode-map)
  (add-hook 'org-mode-hook 'my-org-mode-hook))

;; Flycheck is very useful, though in order for it to work properly in a lot of
;; Haskell files we need to enable a bunch of default extensions.

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind ("C-c n" . flycheck-next-error)
  :config
  (setq flycheck-ghc-language-extensions (append
                                          flycheck-ghc-language-extensions
                                          '("TemplateHaskell"
                                          "OverloadedStrings"
                                          "QuasiQuotes"
                                          "FlexibleContexts"
                                          "NoImplicitPrelude"
                                          "GeneralizedNewtypeDeriving"
                                          "DeriveGeneric"
                                          "MultiParamTypeClasses"
                                          "FunctionalDependencies"
                                          "FlexibleInstances"
                                          "RecordWildCards"
                                          "ScopedTypeVariables"
                                          "TypeFamilies"
                                          "DeriveDataTypeable"))))

(defun my-haskell-mode-hook ()
  "My haskell-mode configuration."
  (interactive-haskell-mode)
  (haskell-decl-scan-mode)
  (turn-on-hi2)
  ;; (turn-on-haskell-indent)
  ;; (flycheck-select-checker 'haskell-stack-ghc)

  (haskell-doc-mode)
  (mapc 'diminish '(interactive-haskell-mode
                    haskell-doc-mode)))

(defun my-cabal-mode-hook ()
  "My cabal configuration."
  (electric-indent-local-mode -1))

(defun haskell-bring-and-compile ()
  "Bring the compilation window to front and compile."
  (interactive)
  (haskell-interactive-bring)
  (haskell-process-cabal-build))

;; My Haskell configuration is SUPER-crufty and needs to be torn down
;; and reassembled from first principles. But it will do for now.

(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (add-hook 'haskell-cabal-mode-hook 'my-cabal-mode-hook)
  :bind (("C-c a i" . haskell-process-do-info)
         ("C-c a c" . haskell-cabal-visit-file)
         ("C-c a d" . haskell-mode-jump-to-def)
         ("C-c a f" . haskell-interactive-bring)
         ("C-c a b" . haskell-mode-stylish-buffer)
         ("C-c c"   . haskell-bring-and-compile)
         ("C-c C-c" . haskell-bring-and-compile)
         ("C-c a i" . haskell-add-import)
         ("C-c a F" . haskell-session-kill)
         ("C-c a s" . haskell-hayoo))
         ;;("SPC" . haskell-mode-contextual-space))
  :mode ("\\.hs$" . haskell-mode)
  :config
  (setq
   haskell-notify-p t
   haskell-font-lock-symbols t
   haskell-process-load-or-reload-prompt t
   haskell-interactive-mode-scroll-to-bottom t
   haskell-process-type 'stack-ghci
   haskell-stylish-on-save t
   haskell-process-log t
   haskell-indent-spaces 4)
  (defalias 'haskell-completing-read-function 'helm--completing-read-default)
  (defalias 'haskell-complete-module-read 'helm--completing-read-default))

;;; End use-package invocations

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

(bind-key "C-c '"  'switch-to-previous-buffer)

(defun eol-then-newline ()
  "Go to end of line then return."
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

(bind-key "s-<return>" 'eol-then-newline)

(bind-key "C-c \\" 'align-regexp)
(bind-key "C-c /"  'comment-or-uncomment-region)
(bind-key "C-c x"  'ESC-prefix)
(bind-key "s-+"    'text-scale-increase)
(bind-key "s-_"    'text-scale-decrease)
(bind-key "s-/"    'hippie-expand)
(bind-key "s-c"    'kill-ring-save)
(bind-key "s-v"    'yank)
(bind-key "s-z"    'undo)
(bind-key "s-a"    'mark-whole-buffer)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(column-number-mode t)
(display-time-mode t)
(auto-save-mode -1)
(prettify-symbols-mode)

(setq
 blink-matching-paren t
 compilation-always-kill t
 compilation-scroll-output t
 create-lockfiles nil
 default-directory "~/src"
 inhibit-startup-screen t
 initial-scratch-message nil
 mac-mouse-wheel-smooth-scroll nil
 kill-whole-line t
 make-backup-files nil
 mac-command-modifier 'super
 require-final-newline t
 ring-bell-function 'ignore
 linum-delay t
 use-dialog-box nil
 indent-tabs-mode nil
 mouse-wheel-scroll-amount '(2 ((shift) . 5)))

(split-window-horizontally)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default
 cursor-type 'bar
 indent-tabs-mode nil)

(setq gc-cons-threshold old-cons-threshold)
(makunbound 'old-cons-threshold)

(provide 'init)

;;; init.el ends here
