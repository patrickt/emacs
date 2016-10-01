;;; init.el -- Patrick Thomson's emacs config

;;; Commentary:
;; This file is in the public domain.

;;; Code:

(defvar old-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000)

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(package-initialize)

(set-default-font "Operator Mono-11")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq
 lexical-binding t
 load-prefer-newer t)

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))

(use-package apropospriate-theme
  :ensure t
  :init
  (load-theme 'apropospriate-dark))

(use-package darkroom)

(use-package wc-goal-mode
  :ensure t)

(use-package ace-window
  :ensure t
  :bind (("C-," . ace-window)))

(use-package powerline
  :ensure t
  :init (powerline-default-theme))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package recentf
  :init (recentf-mode t)
  :config (add-to-list 'recentf-exclude "\\.emacs.d"))

(use-package nix-mode
  :mode ("\\.nix$" . nix-mode))

(use-package anzu
  :defer
  :ensure t
  :diminish anzu-mode
  :init (global-anzu-mode +1))

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

(use-package company
  :ensure t
  :init (global-company-mode 1)
  :bind (("M-/" . company-complete))
  :diminish company-mode
  :config
  (setq company-minimum-prefix-length 4
        company-idle-delay 0.05)
  (define-key company-active-map (kbd "C-n") 'company-select-next))

(use-package restclient
  :ensure t
  :mode ("\\.restclient" . restclient-mode))

(use-package sql
  :ensure t
  :config
  (setq
   sql-user "dashboard"
   sql-database "dashboard"
   sql-server "localhost"))

(use-package company-restclient
  :ensure t
  :defer restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-statistics
  :ensure t
  :init (company-statistics-mode +1))

(use-package prodigy
  :ensure t
  :bind (("C-c q" . prodigy)))

(use-package helm-make
  :ensure t
  :bind (("C-c m" . helm-make-projectile))
  :config (setq helm-make-sort-targets t))

(use-package god-mode
  :ensure t
  :bind ("C-c <SPC>" . god-mode-all)
  :config
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor))

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(use-package helm-git-grep
  :ensure t
  :bind (("C-c G" . helm-git-grep)
         ("C-c h" . helm-git-grep-at-point)))

(use-package projectile
  :ensure t
  :bind (("C-c f" . projectile-find-file)
         ("C-x f" . projectile-find-file) ; overwrites set-fill-column
         ("C-c c" . projectile-compile-project))
  :init (projectile-global-mode)
  :config (setq projectile-completion-system 'helm
                projectile-enable-caching t)
  :diminish projectile-mode)

(use-package linum
  :init (global-linum-mode t)
  :config (setq linum-format "%d"))

(use-package magit
  :defer t
  :ensure t
  :bind (("C-c g" . magit-status))
  :init (global-auto-revert-mode t)
  :config (setq-default magit-last-seen-setup-instructions "1.4.0"))

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode)
  :config (setq git-gutter:update-interval 1)
  :diminish git-gutter-mode)

(use-package yasnippet
  :ensure t
  :defer 1
  :diminish yas-minor-mode
  :config
  (yas-global-mode +1)
  (setq yas-prompt-functions '(yas-completing-prompt)))

(use-package saveplace
  :config (setq-default save-place t))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package haskell-snippets
  :defer 3
  :ensure t)

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

(use-package undo-tree
  :ensure t
  :bind (("C-c _" . undo-tree-visualize))
  :init (global-undo-tree-mode +1)
  :diminish undo-tree-mode)

(use-package eshell
  :bind (("C-c s" . eshell)
         ("C-r" . helm-eshell-history)))

(use-package markdown-mode
  :ensure t
  :config (remove-hook 'before-save-hook 'delete-trailing-whitespace)
  :mode ("\\.md$" . markdown-mode))

(use-package scss-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-l"   . ace-jump-line-mode)
         ("C-c j" . ace-jump-mode)))

(use-package ace-jump-helm-line
  :ensure t
  :defer helm
  :bind (("C-'" . ace-jump-helm-line)))

(use-package duplicate-thing
  :ensure t
  :bind (("C-c u" . duplicate-thing)))

(use-package memory-usage
  :ensure t)

(use-package guide-key
  :ensure t
  :init (guide-key-mode +1)
  :config (setq guide-key/guide-key-sequence '("C-x v"))
  :diminish guide-key-mode)

(use-package dash-at-point
  :ensure t
  :bind ("C-c d" . dash-at-point))

(use-package lua-mode
  :ensure t)

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

;; Highlight-tail mode creates problems with Helm
;; (use-package highlight-tail
;;   :ensure t
;;   :init (highlight-tail-mode)
;;   :diminish highlight-tail-mode
;;   :config
;;   (setq highlight-tail-timer 0.003)
;;   (setq highlight-tail-steps 30)
;;   (highlight-tail-reload))

(defun my-cabal-mode-hook ()
  "My cabal configuration."
  (electric-indent-local-mode -1))

(defun haskell-bring-and-compile ()
  "Bring the compilation window to front and compile."
  (interactive)
  (haskell-interactive-bring)
  (haskell-process-cabal-build))

(use-package elm-mode
  :ensure t)

(defun em-dash ()
  "Insert an em-dash."
  (interactive)
  (insert "—"))

(defun ellipsis ()
  "Insert an ellipsis."
  (interactive)
  (insert "…"))

(use-package org
  :bind (("M--" . em-dash)
         ("M-;" . ellipsis))
  :init
  (defun my-org-mode-hook ()
    (visual-line-mode)
    (wc-goal-mode)
    (setq org-src-fontify-natively t)
    (local-unset-key (kbd "C-c ;")))
  (add-hook 'org-mode-hook 'my-org-mode-hook))

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
   haskell-doc-show-reserved nil
   haskell-indent-spaces 4
   haskell-indent-offset 4
   haskell-doc-show-global-types t)
  (defalias 'haskell-completing-read-function 'helm--completing-read-default)
  (defalias 'haskell-complete-module-read 'helm--completing-read-default))

(use-package ghc
  :pin melpa-stable
  :commands ghc-init ghc-debug
  :bind (("C-c a t" . ghc-show-type))
  :ensure t)

(use-package xml-mode
  :config (setq-default nxml-child-indent 4)
  :mode ("\\.tpl$" . xml-mode))


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

(defun eol-then-newline ()
  "Go to end of line then return."
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

(bind-key "s-<return>" 'eol-then-newline)

(bind-key "C-c '"  'switch-to-previous-buffer)
(bind-key "C-c \\" 'align-regexp)
(bind-key "C-c /"  'comment-or-uncomment-region)
(bind-key "C-c x"  'ESC-prefix)
(bind-key "s-+"    'text-scale-increase)
(bind-key "s-_"    'text-scale-decrease)
(bind-key "s-?"    'hippie-expand)
(bind-key "s-c"    'kill-ring-save)
(bind-key "s-v"    'yank)
(bind-key "s-z"    'undo)

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
 use-dialog-box nil)

(require 'mac-key-mode)

(split-window-horizontally)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default
 cursor-type 'bar
 indent-tabs-mode nil)

(setq gc-cons-threshold old-cons-threshold)
(makunbound 'old-cons-threshold)

(provide 'init)

;;; init.el ends here
