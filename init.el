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
;; important keybinding, C-;, provides counsel-M-x, which lets you
;; fuzzy-find through the space of available functions. Exploring
;; counsel-M-x is the best way to become familiar with the space of
;; extensible functions, which is a sine qua non for being comfortable
;; with Emacs.

;;; Code:

;; To start, we adjust the garbage collection param

(setq gc-cons-threshold 32000000     ;; 32 MB
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
                  ("org-elpa" . "https://orgmode.org/elpa/")))

(package-initialize)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Allow navigation between use-package stanzas with iMenu.
(setq-default use-package-enable-imenu-support t)

(require 'use-package)

(setq
 use-package-always-ensure t
 use-package-verbose t)

;; Fullscreen by default, as early as possible.

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; UTF-8 everywhere, please.

(prefer-coding-system 'utf-8)

(ignore-errors
  (set-frame-font "Iosevka-14"))

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
  :ensure t)

(diminish 'eldoc-mode)

;; The Doom Emacs themes look really good. I use opera.

(use-package doom-themes
  :config
  (load-theme 'doom-tomorrow-night)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)

  ;; Docstrings should be a bit lighter, since they're important.
  (custom-theme-set-faces
  'doom-tomorrow-night
  '(font-lock-doc-face ((t (:foreground "#D8D2C1"))))))

;; Ensure that items in the PATH are made available to Emacs. This should
;; probably just come with the main distribution.

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
  :init
  (ivy-mode 1)
  (unbind-key "S-SPC" ivy-minibuffer-map)
  (setq ivy-height 30
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t)
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'word)))
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-c s"   . swiper-at-point)
         ("C-s"     . swiper))
  :diminish)

;; ivy-rich makes Ivy look a little bit more like Helm.

(use-package ivy-rich
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :init
  (ivy-rich-mode))

(use-package ivy-hydra)

;; Slurp environment variables from the shell.

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package fish-mode)

(use-package gnu-elpa-keyring-update)

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
  :bind (("C-c f" . projectile-find-file))
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy)
  :diminish)

;; Counsel and projectile should work together.

(use-package counsel-projectile
  :bind (("C-c f" . counsel-projectile))
  :init
  (counsel-projectile-mode))

;; Sort commands by recency in ivy windows.

(use-package smex)

;; Keychain stuff. Note to self: if you keep having to enter your
;; keychain password on OS X, make sure that you have the following in .ssh/config:
;; Host *
;;    UseKeychain yes

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; Elm stuff.

(use-package elm-mode
  :disabled)

;; Company is the best Emacs completion system.

(use-package company
  :bind (("C-." . company-complete))
  :diminish company-mode
  :custom
  (company-dabbrev-downcase nil "Don't downcase returned candidates.")
  (company-show-numbers t "Numbers are helpful.")
  (company-tooltip-limit 20 "The more the merrier.")
  (company-abort-manual-when-too-short t "Be less enthusiastic about completion.")
  :config
  (global-company-mode)

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
  :disabled
  :after magit)

;; Since I grew up on Textmate, I'm more-or-less reliant on snippets.

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-completing-prompt))
  :diminish yas-minor-mode)

;; I usually don't edit very large files, but saveplace is nice on the occasions I do.

(use-package saveplace
  :disabled
  :config (setq-default save-place t))

;; Haskell and Elisp are made a lot easier when delimiters are nicely color-coded.

(use-package rainbow-delimiters
  :disabled
  :hook (prog-mode . rainbow-delimiters-mode))

;; multiple-cursors is better than cua-selection-mode.
;; TODO: learn ace-mc

(use-package multiple-cursors
  :bind (("C-c M" . mc/edit-lines)))

;; Common Haskell snippets. These take a while to load, so no need to block on startup.

(use-package haskell-snippets
  :defer yasnippet)

;; The beauty of undo-tree is that it means that, once you've typed something into
;; a buffer, you'll always be able to get it back. That is crucial.

(use-package undo-tree
  :bind (("C-c _" . undo-tree-visualize))
  :config
  (global-undo-tree-mode +1)
  (unbind-key "M-_" undo-tree-map)
  :diminish)

;; Trying undo-propose, which seems to offer a better experience, as
;; undo tree is prone to losing data.

(use-package undo-propose
  :disabled
  :bind (("C-c _" . undo-propose)
         :map undo-propose-mode-map
         ("<up>" . undo-only)))

;; (use-package ansi-color
;;   :config
;;   (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;; C stuff.

(use-package cc-mode
  :disabled)

;; I do all of my writing in either org-mode or markdown-mode.

(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown -t html")))

;; Avy is better than ace-jump.
(use-package avy
  :defer ivy
  :bind (("C-c l l" . avy-goto-line)
         ("C-c l c" . avy-goto-char-timer)
         ("C-c l w" . avy-goto-word-1)
         ("C-'"     . ivy-avy)))

(use-package yaml-mode
  :bind (("C-c a c" . haskell-cabal-visit-file)))

;; Quickly duplicate whatever's under the cursor. I'm shocked this requires a
;; third-party package; it should be standard.

(use-package duplicate-thing
  :bind (("C-c u" . duplicate-thing)))

;; I can never remember the hierarchies of certain bindings, like C-x v for version control
;; stuff. Guide-key helps there. (TODO: figure out other places it'll help.)

(use-package guide-key
  :diminish guide-key-mode
  :config
  (guide-key-mode t)
  (setq guide-key/guide-key-sequence '("C-x v" ;; version control
                                       "C-c a" ;; my mode-specific bindings
                                       "C-c l" ;; line-jumping
                                       "C-c o"
                                       )))

;; Since the in-emacs Dash browser doesn't work on OS X, we have to settle for dash-at-point.

(use-package dash-at-point
  :bind ("C-c d" . dash-at-point))

;; Need to remember to use this more, though I have my issues with the UI.

(use-package dumb-jump
  :disabled
  :bind (("C-c j" . dumb-jump-go-prompt))
  :config (setq dumb-jump-selector 'ivy))

(use-package elec-pair
  :config
  (add-to-list 'electric-pair-pairs '(?` . ?`))  ; electric-quote backticks
  (add-to-list 'electric-pair-pairs '(?“ . ?”))) ; and curlies

;; OCaml is loaded not through melpa, but through OPAM itself.

(ignore-errors
  (autoload (expand-file-name "~/.opam/system/share/emacs/site-lisp/tuareg-site-file")))

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

  :hook (org-mode . visual-line-mode)

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

;; Flycheck mode is usually useful.
(use-package flycheck
  :hook (org-mode . flycheck-mode)
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

  (defun my-haskell-mode-hook ()
    "Make sure the compile command is right."
    (setq-local compile-command "stack build --fast"))

  (unbind-key "C-c C-s" haskell-mode-map)

  ;; I don't go overboard with the symbols but they can be nice.
  (setq haskell-font-lock-symbols 't
        haskell-font-lock-symbols-alist
        '(("\\" . "λ")
          ("<=" . "≤")
          (">=" . "≥")
          ("==" . "≡")
          ("<>" . "♢")
          ("/=" . "≢")
          ("*"  . "★")
          ("<=<" . "<=<")
;;          ("::" . "∷")
          ("<+>" . "⍚")
          ("undefined" . "⊥")
          ("forall" . "∀")
          ("." "∘" haskell-font-lock-dot-is-not-composition) ; or '◦'
          ))

  ;; Unfortunately haskell-mode doesn't quite track the latest and
  ;; greatest in Haskell extensions, so we have to give the font-lock
  ;; system a couple of hints.

  (append-to-list haskell-ghc-supported-extensions
                  '("DerivingVia" "BlockArguments" "DerivingStrategies"))

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
  :hook (haskell-mode . my-haskell-mode-hook)

  :bind (:map haskell-mode-map
         ("C-c a c" . haskell-cabal-visit-file)
	 ("C-c a b" . haskell-mode-stylish-buffer)
         ("C-c a i" . haskell-navigate-imports)
         ("C-c a a" . haskell-mode-toggle-scc-at-point)
         ("C-c a w" . stack-watch)))

;; Intero… well, it sort-of works. It generally chokes on large projects,
;; but for sandboxes and small projects it's the best thing out there
;; (though I need to try dante-mode, or so joshvera tells me).
(use-package intero
  :disabled
  :after haskell-mode
  :bind (:map haskell-mode-map
         ("C-c a r" . intero-repl)
         ("C-c a j" . intero-goto-definition)
         ("C-c a n" . intero-info)
         ("C-c a t" . intero-type-at)
         ("C-c a u" . intero-uses-at)
         ("C-c a s" . intero-apply-suggestions)))

(use-package attrap
  :bind (("C-c q" . attrap-attrap)))

(use-package dante
  :after haskell-mode
  :bind (:map haskell-mode-map
              ("C-c a t" . dante-type-at)
              ("C-c a n" . dante-info)
              ("C-c a s" . attrap-attrap)
              ))

;; Someday I'm going to start using Idris again.
(use-package idris-mode
  :disabled
  :bind (("C-c C-v" . idris-case-split)))

(use-package typescript-mode :defer)

(use-package protobuf-mode)

(use-package dtrace-script-mode)

(use-package rust-mode)

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
  (mapc 'kill-buffer-with-prejudice (buffer-list)))

(defun split-right-and-enter ()
  "Split the window to the right and enter it."
  (interactive)
  (split-window-right)
  (other-window 1))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun display-startup-echo-area-message ()
  "Overrides the normally tedious error message."
  (message "Welcome back."))

(defun eol-then-newline ()
  "Go to end of line then return."
  (interactive)
  (move-end-of-line nil)
  (newline)
  (indent-for-tab-command))

;; There is an extant bug where magit-refresh prompts to save files that haven't
;; been modified. We work around this with some defadvice over maybe-unset-buffer-modified. SO:
;; https://emacs.stackexchange.com/questions/24011/make-emacs-diff-files-before-asking-to-save

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

(bind-key "C-x k"      'kill-buffer-with-prejudice)
(bind-key "C-c e"      'open-init-file)
(bind-key "C-c k"      'kill-all-buffers)
(bind-key "s-<return>" 'eol-then-newline)
(bind-key "C-c 5"      'query-replace-regexp) ;; stupid vestigial binding
(bind-key "M-/"        'hippie-expand)
(bind-key "C-c '"      'switch-to-previous-buffer)
(bind-key "C-c \\"     'align-regexp)
(bind-key "C-c m"      'compile)
(bind-key "C-c 3"      'split-right-and-enter)
(bind-key "C-c /"      'comment-or-uncomment-region)
(bind-key "C-c x"      'ESC-prefix)
(bind-key "C-,"        'other-window)
(bind-key "M-,"        'other-window)
(bind-key "M-i"        'delete-indentation)

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
(bind-key "<home>" 'beginning-of-buffer)
(bind-key "<end>"  'end-of-buffer)
(bind-key "s->"    'end-of-buffer)
(bind-key "M-_"    'em-dash)
(bind-key "M-;"    'ellipsis)
(bind-key "C-="    'next-error)
(bind-key "s-{"    'previous-buffer)
(bind-key "s-}"    'next-buffer)

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
;; (ignore-errors (mac-auto-operator-composition-mode))

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
  user-full-name "Patrick Thomson"       ; it me
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

;; (setq debug-on-error nil)

;; goodbye, thanks for reading

(provide 'init)
;;; init.el ends here
