;; init.el -- Patrick Thomson's emacs config. -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is in the public domain.
;;
;; If you want to give this file a try, you can just drop it into
;; ~/.emacs.d: it downloads everything it needs. It is also relatively
;; fast to start up (once it's downloaded all the required packages),
;; as it endeavours to load packages lazily when possible.
;;
;; A general note on keybindings: the custom keybindings applicable to
;; all major modes appear with the C-c prefix, as is standard.
;; Per-language commands appear with the C-c a prefix. The most
;; important keybinding, C-c ;, provides counsel-M-x, which lets you
;; fuzzy-find through the space of available functions. Exploring
;; counsel-M-x is the best way to become familiar with the space of
;; invokable functions, which is a sine qua non for being comfortable
;; with Emacs.

;;; Code:

;; To start, we adjust the garbage collection parameter up from a
;; measly 8 MB. We don't set it too high, though, lest GCs never happen.

(setq gc-cons-threshold (expt 2 28)
      garbage-collection-messages t) ;; indicator of thrashing

;; Bump up the recursion limit.
(setq max-lisp-eval-depth 2000)

;; Package-initialization preamble, adding melpa and melpa-stable.

(require 'package)

(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

(append-to-list package-archives
                '(("melpa" . "http://melpa.org/packages/")
                  ("melpa-stable" . "http://stable.melpa.org/packages/")
                  ("org-elpa" . "https://orgmode.org/elpa/")
                  ("ublt" . "https://elpa.ubolonton.org/packages/")))

(package-initialize)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Allow navigation between use-package stanzas with imenu.
;; This has to be set before loading use-package.
(defvar use-package-enable-imenu-support t)

(require 'use-package)

(setq
 use-package-always-ensure t
 use-package-verbose t
 custom-safe-themes t)

;; Fullscreen by default, as early as possible.

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; UTF-8 everywhere, please.

(prefer-coding-system 'utf-8)

;; Iosevka is my font of choice, but don't freak out if it's present.

(ignore-errors (set-frame-font "Iosevka-13"))

;; Any Customize-based settings should live in custom.el, not here.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Always prefer newer files.

(setq load-prefer-newer t)

;; Disable otiose GUI settings: they just waste space.
;; fringe-mode is especially ruinous performance-wise.

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (fringe-mode -1))

;; Haven't figured out how to diminish eldoc-mode outside of
;; requiring this explicitly and doing it manually.

(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode))

;; Ensure GNU ELPA has the GPG keys it needs

(use-package gnu-elpa-keyring-update)

;; The Doom Emacs themes look really good.

(use-package doom-themes
  :config
  (let ((chosen-theme 'doom-challenger-deep))
    (load-theme chosen-theme)
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)

    ;; Docstrings should be a bit lighter, since they're important.
    (custom-theme-set-faces
     chosen-theme
     '(font-lock-doc-face ((t (:foreground "#D8D2C1")))))))

(use-package aggressive-indent
  :config (global-aggressive-indent-mode)
  :diminish)

;; Icons are nice. We can't let VS Code have all the nice things.
(use-package all-the-icons)

(use-package centaur-tabs
  :after all-the-icons
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-buffer-groups-function 'centaur-tabs-projectile-buffer-groups)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 36)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")

  :bind (("s-{" . centaur-tabs-backward)
         ("s-}" . centaur-tabs-forward)))


;; Recentf comes with Emacs but it should always be enabled.
(use-package recentf
  :init (recentf-mode t)
  :config
  (add-to-list 'recentf-exclude "\\.emacs.d")
  (add-to-list 'recentf-exclude ".+tmp......\\.org"))

;; Ivy makes most minibuffer prompts sortable and filterable. I used
;; to use helm, but it was too slow. Unfortunately org-ref depends on
;; it, but I never load it, so we good.

(use-package ivy
  :ensure t
  :diminish
  :custom
  (ivy-height 30)
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1)
  (unbind-key "S-SPC" ivy-minibuffer-map)
  (defun swiper-at-point ()
    "Start searching for the thing at point."
    (interactive)
    (swiper (thing-at-point 'word)))

  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-c s"   . swiper-at-point)
         ("C-s"     . swiper)))

;; ivy-rich makes Ivy look a little bit more like Helm.

(use-package ivy-rich
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (ivy-rich-mode))

;; Provides visual interface to hydra layouts. I don't really
;; use hydras anywhere yet but some packages do.

(use-package ivy-hydra)

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package volatile-highlights
  :diminish
  :config (volatile-highlights-mode))

;; fish is a good shell. You should try it.

(use-package fish-mode)

;; Counsel applies Ivy-like behavior to other builtin features of
;; emacs, e.g. search.

(use-package counsel
  :ensure t
  :after ivy
  :init
  (counsel-mode 1)

  :bind (("C-c ;" . counsel-M-x)
         ("C-c U" . counsel-unicode-char)
         ("C-c i" . counsel-imenu)
         ("C-x f" . counsel-find-file)
         ("C-c y" . counsel-yank-pop)
	 ("C-c r" . counsel-recentf)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :diminish)

;; Deadgrep is amazing.

(use-package deadgrep
  :bind (("C-c h" . deadgrep)))

;; projectile comes with Emacs these days, but we want to enable
;; caching, since I work on big projects.

(use-package projectile
  :diminish
  :bind (("C-c F" . projectile-switch-project))
  :custom
  (projectile-enable-caching t)
  (projectile-completion-system 'ivy))

;; Counsel and projectile should work together.

(use-package counsel-projectile
  :bind (("C-c f" . counsel-projectile))
  :init (counsel-projectile-mode))

;; Sort commands by recency in ivy windows.

(use-package smex)

;; Keychain stuff. Note to self: if you keep having to enter your
;; keychain password on OS X, make sure that you have the following in .ssh/config:
;; Host *
;;    UseKeychain yes

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; Emacs makes it weirdly hard to edit a file as root. This helps.
(use-package sudo-edit)

;; Elm stuff.

(use-package elm-mode
  :defer)

;; Company is the best Emacs completion system.

(use-package company
  :bind (("C-." . company-complete))
  :diminish company-mode
  :hook ((prog-mode . company-mode))
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-idle-delay 0.1 "Faster!")
  :config
  ;; use numbers 0-9 to select company completion candidates
  (let ((map company-active-map))
    (mapc (lambda (x) (define-key map (format "%d" x)
                        `(lambda () (interactive) (company-complete-number ,x))))
          (number-sequence 0 9))))

;; Textmate-style tap-to-expand-into-the-current-delimiter.

(use-package expand-region
  :bind (("C-c n" . er/expand-region)))

;; Magit is one of the best pieces of OSS I have ever used. It is truly esssential.

(use-package magit
  :bind (("C-c g" . magit-status))

  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :custom
  (magit-remote-set-if-missing t)
  (magit-diff-refine-hunk t)
  (magit-branch-prefer-remote-upstream)
  :config
  (magit-auto-revert-mode t)

  ;; Magit, and Emacs in general, has a nasty habit of prompting to save buffers
  ;; that are identical to those on disk. This is an attempt at remedying that,
  ;; one that I should probably attach to other functions like save-buffers-kill-emacs.
  (advice-add 'magit-refresh :before #'maybe-unset-buffer-modified)
  (advice-add 'magit-commit  :before #'maybe-unset-buffer-modified)
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-to-list 'magit-no-confirm 'stage-all-changes))

;; Unclear whether this does anything at the moment.

(use-package libgit
  :after magit)

(use-package magit-libgit
  :after libgit)

;; Since I grew up on Textmate, I'm more-or-less reliant on snippets.

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-completing-prompt))
  :diminish yas-minor-mode)

;; Haskell and Elisp are made a lot easier when delimiters are nicely color-coded.

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; multiple-cursors is better than cua-selection-mode.
;; TODO: learn ace-mc

(use-package multiple-cursors
  :bind (("C-c M" . mc/edit-lines)))

;; Common Haskell snippets. These take a while to load, so no need to block on startup.

(use-package haskell-snippets
  :defer yasnippet)

;; The beauty of undo-tree is that it means that, once you've typed something into a buffer,
;; you'll always be able to get it back. At least in theory. undo-tree has long-standing data
;; loss bugs that are unlikely to be fixed. But no other package provodes a comparable experience.

(use-package undo-tree
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map)
  :diminish)

;; C stuff.

(use-package cc-mode)

(use-package purescript-mode
  :hook (purescript-mode . turn-on-purescript-indentation))

;; I do all of my writing in either org-mode or markdown-mode.

(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown -t html")))

;; Avy is better than ace-jump.
(use-package avy
  :defer ivy
  :bind (("C-c l"   . avy-goto-line)
         ("C-c L c" . avy-goto-char-timer)
         ("C-c L w" . avy-goto-word-1)
         :map minibuffer-local-completion-map
         ("C-'"     . ivy-avy)))

(use-package yaml-mode
  :bind (("C-c a c" . haskell-cabal-visit-file)))

;; Quickly duplicate whatever's under the cursor. I'm shocked this requires a
;; third-party package; it should be standard.

(use-package duplicate-thing
  :config
  (defun duplicate-thing-without-moving (N)
    (interactive "P")
    (save-mark-and-excursion (duplicate-thing N)))

  :bind (("C-c u" . duplicate-thing-without-moving)))

;; I can never remember the hierarchies of certain bindings, like C-x v for version control
;; stuff. Guide-key helps there. (TODO: figure out other places it'll help.)

(use-package guide-key
  :diminish guide-key-mode
  :config
  (guide-key-mode t)
  :custom
  (guide-key/recursive-key-sequence-flag t)
  (guide-key/guide-key-sequence '("C-x v" ;; version control
                                  "C-c a" ;; my mode-specific bindings
                                  "C-c o" ;; org-mode
                                  )))

;; Since the in-emacs Dash browser doesn't work on OS X, we have to settle for dash-at-point.

(use-package dash-at-point
  :bind ("C-c d" . dash-at-point))

;; Need to remember to use this more, though I have my issues with the UI.

(use-package dumb-jump
  :bind (("C-c j" . dumb-jump-go)
         ("C-c J" . dumb-jump-go-prompt))
  :config (setq dumb-jump-selector 'ivy))

(use-package elec-pair
  :config
  (add-to-list 'electric-pair-pairs '(?` . ?`))  ; electric-quote backticks
  (add-to-list 'electric-pair-pairs '(?“ . ?”))) ; and curlies

(use-package olivetti
  :custom
  (olivetti-minimum-body-width 60))

;; Some useful text-manipulation functions used later on in org-mode stuff.

(defun em-dash ()
  "Insert an em-dash."
  (interactive)
  (insert "—"))

(defun ellipsis ()
  "Insert an ellipsis."
  (interactive)
  (insert "…"))

(defun lambduh ()
  "Insert a lowercase lambda."
  (interactive)
  (insert "λ"))

(defun open-semantic-notes ()
  "Open my notes file."
  (interactive)
  (find-file "~/txt/semantic.org"))

(use-package org
  ;; Always get this from the GNU archive.
  :pin gnu

  :diminish org-indent-mode

  :custom
  (company-dabbrev-ignore-case . nil)

  ;; Global functions that drop me into org-mode-related modes
  ;; are prefixed under C-c o.
  :bind (("C-c o c"  . org-capture)
         ("C-c o n"  . open-semantic-notes)
         ("C-c o s"  . org-store-link)
         ("C-c o a"  . org-agenda)
         :map org-mode-map
         ("M-s-<return>" . org-insert-todo-heading)
         ("M--"      . em-dash)
         ("M-;"      . ellipsis)
         ("C-c c"    . org-mode-insert-code)
         ("C-c a s"  . org-emphasize)
         ("C-c a r"  . org-ref)
         ("C-c a e"  . outline-show-all)
         ("C-c a l"  . lambduh)
         ("C-c a t"  . unindent-by-four))

  :hook ((org-mode . visual-line-mode)
         (org-mode . flyspell-mode))


  :config

  ;; TODO: build a real indentation solution

  (defun unindent-by-four ()
    (interactive)
    (indent-rigidly (region-beginning) (region-end) -4))

  ;; Org-mode conflicts with a lot of stuff I take for granted.
  (unbind-key "C-c ;" org-mode-map)
  (unbind-key "C-,"   org-mode-map)
  (unbind-key "M-<left>" org-mode-map)
  (unbind-key "M-<right>" org-mode-map)

  (let ((todo-path (expand-file-name "~/txt/todo.org")))
    (when (file-exists-p todo-path)
      (setq org-agenda-files (list todo-path)
            org-default-notes-file todo-path)))

  (setq org-footnote-section ""
        org-startup-with-inline-images t
        org-pretty-entities t
        org-ellipsis "…"
        org-startup-folded nil
        org-footnote-section nil
        org-hide-leading-stars nil
        )

  ;; This allegedly gets better behavior for delineating org-mode
  ;; emphasis. But (Monique voice) I would like to see it.
  (setcar (nthcdr 4 org-emphasis-regexp-components) 4)

  (defun org-mode-insert-code ()
    (interactive)
    (org-emphasize ?~)))

;; Autocomplete for org tags.
(use-package org-ac :after org)

(use-package flyspell-correct-ivy
  :after ivy)

;; vterm is good.
(use-package vterm
  )

;; I forgot editors can do that
(use-package iedit)

(use-package vterm-toggle
  :custom
  (vterm-toggle-fullscreen-p nil)
  :bind (("C-c t" . vterm-toggle)))

;; Sometimes useful for putting the right piece of punctuation in there.
(use-package typo)

;; Reference management disabled for org-ref until I figure out what its deal is.
(use-package org-ref
  :disabled
  :defer
  :config
  (ignore-errors (load-private-settings)))

(use-package go-mode
  :init
  (defun my-go-mode-hook ()
    (add-hook 'before-save-hook #'gofmt-before-save))

  :hook (go-mode . my-go-mode-hook))

;; Flycheck mode is usually useful, but I need to sit down
;; and figure out what the story is with Haskell checking.
(use-package flycheck
  :hook (org-mode . flycheck-mode)
  :bind (("C-c `" . flycheck-next-error)
         ("C-c a e" . flycheck-list-errors))
  :config
  (setq-default flycheck-ghc-args
                '( "-XDataKinds"
                   "-XDeriveFoldable"
                   "-XDeriveFunctor"
                   "-XDeriveGeneric"
                   "-XDeriveTraversable"
                   "-XFlexibleContexts"
                   "-XFlexibleInstances"
                   "-XMonadFailDesugaring"
                   "-XMultiParamTypeClasses"
                   "-XOverloadedStrings"
                   "-XRecordWildCards"
                   "-XStandaloneDeriving"
                   "-XTypeApplications"
                   ))


  (global-flycheck-mode)
  (add-to-list 'flycheck-checkers 'proselint)
  (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc))

;; For Hammerspoon.
(use-package lua-mode)

;; Haskell is my programming language of choice.
(use-package haskell-mode
  :config

  (unbind-key "C-c C-s" haskell-mode-map)

  (defun toggle-haskell-stylish-on-save ()
    (interactive)
    (setq haskell-stylish-on-save (not haskell-stylish-on-save)))

  (setq haskell-stylish-on-save t
        haskell-font-lock-symbols t
        haskell-font-lock-symbols-alist
        '(("\\" . "λ")
          ("==" . "≡")
          ("<>" . "♢")
          ("/=" . "≢")
          ("*"  . "★")
          ("<+>" . "⍚")
          ("undefined" . "⊥")
          ("forall" . "∀")
          ("." "∘" haskell-font-lock-dot-is-not-composition) ; or '◦'
          ))

  ;; Unfortunately haskell-mode doesn't quite track the latest and
  ;; greatest in Haskell extensions, so we have to give the font-lock
  ;; system a couple of hints.

  (append-to-list haskell-ghc-supported-extensions
                  '("DerivingVia" "BlockArguments" "DerivingStrategies" "QuantifiedConstraints"))

  (append-to-list haskell-font-lock-keywords '("capi" "via" "stock" "anyclass"))

  (append-to-list haskell-language-extensions
                  '("-XDataKinds"
                    "-XDeriveFoldable"
                    "-XDeriveFunctor"
                    "-XDeriveGeneric"
                    "-XDeriveTraversable"
                    "-XFlexibleContexts"
                    "-XFlexibleInstances"
                    "-XMonadFailDesugaring"
                    "-XMultiParamTypeClasses"
                    "-XOverloadedStrings"
                    "-XRecordWildCards"
                    "-XStandaloneDeriving"
                    "-XStrictData"
                    "-XTypeApplications"))

  :mode ("\\.hs$" . haskell-mode)

  :bind (:map haskell-mode-map
              ("C-c a c" . haskell-cabal-visit-file)
              ("C-c a b" . haskell-mode-stylish-buffer)
              ("C-c a i" . haskell-navigate-imports)
              ("C-c a a" . haskell-mode-toggle-scc-at-point)))

(use-package attrap
  :bind (("C-c q" . attrap-attrap))
  :custom
  (append-to-list 'attrap-haskell-extensions '("DerivingStrategies" "LambdaCase")))

(use-package dante
  :after haskell-mode
  :hook (haskell-mode . dante-mode)
  :custom
  (flymake-no-changes-timeout nil)
  (flycheck-start-syntax-check-on-newline nil)
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (dante-tap-type-time 2)
  :bind (:map dante-mode-map
              ("C-c /" . comment-dwim)
              :map haskell-mode-map
              ("C-c a t" . dante-type-at)
              ("C-c a n" . dante-info)
              ("C-c C-c" . attrap-attrap)
              ("C-c c"   . attrap-attrap)
              ))

;; Someday I'm going to start using Idris again.
(use-package idris-mode
  :disabled
  :bind (("C-c C-v" . idris-case-split)))

(use-package typescript-mode :defer)

(use-package protobuf-mode)

(use-package visual-regexp
  :bind (("C-c 5" . vr/replace)))

(use-package dockerfile-mode)

(use-package web-mode
  :mode ("\\.html$" . web-mode))

(use-package dtrace-script-mode)

(use-package rust-mode
  :defer
  :custom
  (rust-format-on-save t)
  (company-idle-delay 1.0)
  :config
  (defun rust-cargo-visit-file ()
    "Visit the cargo.toml file at the project root."
    (interactive)
    (find-file (concat (projectile-project-root) "./Cargo.toml")))
  :bind (:map rust-mode-map
              ("C-c a c" . rust-cargo-visit-file)))

(use-package racer
  :hook (rust-mode . racer-mode)
  :bind (:map racer-mode-map
              ("C-c a i" . racer-desc)))

(use-package flycheck-rust
  :after rust-mode
  :hook (rust-mode . flycheck-rust-setup))

(use-package flycheck-inline
  :hook (rust-mode . flycheck-inline-mode))

(use-package cargo
  :custom (compile-command  "cargo build")
  :bind (:map rust-mode-map
              ("C-c a b" . cargo-process-build)
              ("C-c a d" . cargo-process-doc-open)))

(use-package smerge-mode
  :custom
  (smerge-command-prefix (kbd "C-c m"))
  )

(use-package github-notifier
  :hook (prog-mode . github-notifier-mode))

(defun my-elisp-mode-hook ()
  "My elisp customizations."
  (electric-pair-mode 1)
  (add-hook 'before-save-hook 'check-parens nil t)
  (auto-composition-mode nil))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file user-init-file))

(defun kill-all-buffers ()
  "Close all buffers."
  (interactive)
  (maybe-unset-buffer-modified)
  (save-some-buffers)
  (let ((kill-buffer-query-functions '()))
    (mapc 'kill-buffer-with-prejudice (buffer-list))))


(defun split-right-and-enter ()
  "Split the window to the right and enter it."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun random-choice (items)
  "Choose a random element from ITEMS, which must be nonempty."
  (nth (random (length items)) items))

(defun display-startup-echo-area-message ()
  "Overrides the normally tedious error message."
  (message (random-choice '("Welcome back."
                            "Center others today."
                            "Be kind."))))

(defun eol-then-newline ()
  "Go to end of line then return."
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

(autoload 'diff-no-select "diff")

(defun current-buffer-matches-file-p ()
  "Return t if the current buffer is identical to its associated file."
  (when (and buffer-file-name (buffer-modified-p))
    (diff-no-select buffer-file-name (current-buffer) nil 'noasync)
    (with-current-buffer "*Diff*"
      (and (search-forward-regexp "^Diff finished \(no differences\)\." (point-max) 'noerror) t))))

(defun maybe-unset-buffer-modified (&optional _)
  "Clear modified bit on all unmodified buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name (buffer-modified-p) (current-buffer-matches-file-p))
        (set-buffer-modified-p nil)))))

;; Don't prompt to save unmodified buffers on exit.
(advice-add 'save-buffers-kill-emacs :before #'maybe-unset-buffer-modified)

(defun kill-buffer-with-prejudice (&optional _)
  "Kill a buffer, eliding the save dialogue if there are no diffs."
  (interactive)
  (when (current-buffer-matches-file-p) (set-buffer-modified-p nil))
  (kill-buffer))

(defun insert-current-date ()
  "Insert the date into the current buffer."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(bind-key "C-x k"      'kill-buffer-with-prejudice)
(bind-key "C-c e"      'open-init-file)
(bind-key "C-c k"      'kill-all-buffers)
(bind-key "s-<return>" 'eol-then-newline)
(bind-key "M-/"        'hippie-expand)
(bind-key "C-c '"      'switch-to-previous-buffer)
(bind-key "C-c \\"     'align-regexp)
(bind-key "C-c m"      'compile)
(bind-key "C-c 3"      'split-right-and-enter)
(bind-key "C-c x"      'ESC-prefix)
(bind-key "C-c ,"      'other-window)
(bind-key "C-,"        'other-window)
(bind-key "M-,"        'other-window)
(bind-key "s-,"        'other-window)
(bind-key "M-i"        'delete-indentation)
(bind-key "C-c /"      'comment-dwim)
(bind-key "C-c p"      'copy-file-name-to-clipboard)

;; When tracking down slowness, opening ivy to start these functions
;; throws off the traces.
(bind-key "C-c a p" 'profiler-start)
(bind-key "C-c a P" 'profiler-report)

;; macOS-style bindings, too (no cua-mode, it's nasty)
(bind-key "s-+"	   'text-scale-increase)
(bind-key "s-_"	   'text-scale-decrease)
(bind-key "s-s"    'save-buffer)
(bind-key "s-c"	   'kill-ring-save)
(bind-key "s-v"	   'yank)
(bind-key "s-z"	   'undo)
(bind-key "s-a"	   'mark-whole-buffer)
(bind-key "s-<"    'beginning-of-buffer)
(bind-key "s-x"    'kill-region)
(bind-key "s-w"    'kill-buffer)
(bind-key "<home>" 'beginning-of-buffer)
(bind-key "<end>"  'end-of-buffer)
(bind-key "s->"    'end-of-buffer)
(bind-key "M-_"    'em-dash)
(bind-key "M-;"    'ellipsis)
(bind-key "C-="    'next-error)
(bind-key "C-o"    'other-window)

(unbind-key "C-z")     ;; I never want to suspend the frame
(unbind-key "C-<tab>") ;; prevent switching to tab mode randomly
(unbind-key "C-h n")   ;; I have never wanted to see emacs news ever
(unbind-key "C-h C-n") ;; why on earth is it bound to two keybindings??
(unbind-key "C-x C-d") ;; list-directory is utterly useless given the existence of dired
(unbind-key "M-o")     ;; facemenu mode is useless
(unbind-key "C-x C-r") ;; as is find-file-read-only


;; I'm not made of time, I can't type "yes" for every choice
(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)              ; Always highlight the current line.
(show-paren-mode t)                  ; And point out matching parentheses.
(delete-selection-mode t)            ; Behave like any other sensible text editor would.
(global-display-line-numbers-mode)   ; Emacs has this builtin now, it's fast
(save-place-mode)                    ; Remember where I was

;; Make sure that ligatures from fonts that offer them are enabled.
;; This isn't present on GNU Emacs and requires a tremendous amount
;; of munging of 'prettify-symbols-alist'.
(ignore-errors (mac-auto-operator-composition-mode))

(setq
 compilation-always-kill t              ; Never prompt to kill a compilation session.
 compilation-scroll-output 'first-error ; Always scroll to the bottom.
 make-backup-files nil                  ; No backups, thanks.
 auto-save-default nil                  ; Or autosaves. What's the difference between autosaves and backups?
 create-lockfiles nil                   ; Emacs sure loves to put lockfiles everywhere.
 default-directory "~/src/"             ; My code lives here.
 inhibit-startup-screen t               ; No need to see GNU agitprop.
 kill-whole-line t                      ; Lets C-k delete the whole line
 mac-command-modifier 'super            ; I'm not sure this is the right toggle, but whatever.
 require-final-newline t                ; Auto-insert trailing newlines.
 ring-bell-function 'ignore             ; Do not ding. Ever.
 use-dialog-box nil                     ; Dialogues always go in the modeline.
 frame-title-format "emacs – %b"        ; Put something useful in the status bar.
 initial-scratch-message nil            ; SHUT UP SHUT UP SHUT UP
 mac-option-modifier 'meta              ; why isn't this the default
 save-interprogram-paste-before-kill t  ; preserve paste to system ring
 enable-recursive-minibuffers t         ; don't fucking freak out if I use the minibuffer twice
 sentence-end-double-space nil          ; are you fucking kidding me with this shit
 confirm-kill-processes nil             ; don't whine at me when I'm quitting.
 mac-mouse-wheel-smooth-scroll nil      ; no smooth scrolling
 mac-drawing-use-gcd t                  ; and you can do it on other frames
 mark-even-if-inactive nil              ; prevent really unintuitive undo behavior
 auto-window-vscroll nil                ; may be connected to speed
 ispell-program-name "hunspell"
 )

;; dired whines at you on macOS unless you do this.
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(setq-default
  cursor-type 'bar
  indent-tabs-mode nil
  cursor-in-non-selected-windows nil)

(set-fill-column 95)

;; Always trim trailing whitespace.

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; I moved this into custom.el because it can screw up other people's git commits.

(unless (stringp user-full-name)
  (message "user-full-name is not set. Add it to custom.el for the best experience."))

;; goodbye, thanks for reading

(provide 'init)
;;; init.el ends here
