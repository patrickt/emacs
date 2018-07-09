;; init.el -- Patrick Thomson's emacs config

;;; Commentary:
;; This file is in the public domain.

;;; Code:

;; Temporarily disable GC limits.

(defvar old-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold 100000000)

(setq debug-on-error t
      max-list-eval-depth 2000)

;; Package-initialization preamble.

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package.

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
      use-package-verbose t)

;; Fullscreen by default, as early as possible.

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Start with split windows.

(split-window-horizontally)

;; Open my TODO on Mac

(when (eq system-type 'darwin)
  (other-window 1)
  (find-file "~/txt/semantic.org")
  (other-window 1))

;; Use Operator Mono, my favorite monospaced font, handling its absence gracefully.

(ignore-errors
  (set-frame-font "Fira Code Retina-14"))

;; Any Customize-based settings should live in custom.el, not here.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Important preamble settings: use lexical scope and prefer newer files.

(setq
 lexical-binding t
 load-prefer-newer t)

;; Disable otiose GUI settings: they just waste space.

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

(use-package diminish
  :diminish eldoc-mode)

;; Material is easy on the eyes.

(use-package material-theme
  :config
  (load-theme 'material))

;; Ace-window is a nice way to switch between frames quickly.

(use-package ace-window
  :bind (("C-," . ace-window)))

;; Ensure that items in the PATH are made available to Emacs. This should
;; probably just come with the main distribution.

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; Dim inactive buffers.

(use-package dimmer
  :config
  (dimmer-mode))

;; Recentf comes with Emacs but it should always be enabled.

(recentf-mode t)
(add-to-list 'recentf-exclude "\\.emacs.d")

;; I used to use helm, but it was too slow. Unfortunately org-ref
;; depends on it, but I never load it, so we good.

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-height 30)
  (setq ivy-use-virtual-buffers t)
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'word)))
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-c s"   . swiper-at-point)
         ("C-s"     . swiper))
  :diminish)

(use-package ivy-hydra)

(use-package counsel
  :ensure t
  :config
  (counsel-mode 1)
  (defun counsel-rg-at-point ()
    (interactive)
    (let ((selection (thing-at-point 'word)))
      (if (<= 4 (length selection))
          (counsel-rg selection)
        (counsel-rg))))
  :bind (("C-c ;" . counsel-M-x)
         ("C-c U" . counsel-unicode-char)
         ("C-c h" . counsel-rg-at-point)
         ("C-c H" . counsel-rg)
         ("C-c i" . counsel-imenu)
         ("C-c y" . counsel-yank-pop)
	 ("C-c r" . counsel-recentf)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :diminish)

(use-package projectile
  :config
  (setq projectile-enable-caching t)
  :diminish)

(use-package counsel-projectile
  :bind (("C-c f" . counsel-projectile))
  :config
  (counsel-projectile-mode))

;; If you don't use this, recent commands in ivy won't be shown first

(use-package smex)

;; Keychain stuff. Note to self: if you keep having to enter your
;; keychain password on OS X, make sure that you have the following in .ssh/config:
;; Host *
;;    UseKeychain yes

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; TODO: reinvestigate company and determine if it's slow

(use-package expand-region
  :bind (("C-c n" . er/expand-region)))

;; Magit is one of the best pieces of OSS I have ever used. It is truly esssential.

(use-package magit
  :bind (("C-c g" . magit-status))
  :config
  (magit-auto-revert-mode t)
  (advice-add 'magit-refresh :before #'maybe-unset-buffer-modified)
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (setq-default magit-last-seen-setup-instructions "1.4.0"))


;; Since I grew up on Textmate, I'm more-or-less reliant on snippets.

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-prompt-functions '(yas-completing-prompt))
  :diminish yas-minor-mode)

;; I usually don't edit very large files, but saveplace is nice on the occasions I do.

(use-package saveplace
  :config (setq-default save-place t))

;; Haskell and Elisp are made a lot easier when delimiters are nicely color-coded.

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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

;; C stuff.

(use-package cc-mode)

;; I do all of my writing in either org-mode or markdown-mode.

(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :hook (gfm-mode . auto-fill-mode))

;; Avy is better than ace-jump.
(use-package avy
  :defer ivy
  :bind (("C-l"   . avy-goto-line)
         ("C-c j" . avy-goto-word-1)
         ("C-'"   . ivy-avy)))

;; YAML is underappreciated.

(use-package yaml-mode)

;; Quickly duplicate whatever's under the cursor. I'm shocked this requires a
;; third-party package; it should be standard.

(use-package duplicate-thing
  :bind (("C-c u" . duplicate-thing)))

;; I can never remember the hierarchies of certain bindings, like C-x v for version control
;; stuff. Guide-key helps there. (TODO: figure out other places it'll help.)

(use-package guide-key
  :config
  (guide-key-mode t)
  (setq guide-key/guide-key-sequence '("C-x v"
                                       "C-c a"
                                       "C-c p"))
  :diminish guide-key-mode)

;; Since the in-emacs Dash browser doesn't owrk on OS X, we have to settle for dash-at-point.

(use-package dash-at-point
  :bind ("C-c d" . dash-at-point))

(use-package dumb-jump
  :bind (("C-c d" . dumb-jump-go)
	 ("C-c D" . dumb-jump-go-prompt))
  :config (setq dumb-jump-selector 'ivy))

;; OCaml.

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

;; I do a lot of writing in org-mode, though I have yet to truly take advantage
;; of its enormous power. It steps on a few of my keybindings, so we take care of
;; those with unbind-key.

(use-package org
  :defer
  :hook (org-mode . auto-fill-mode)
  :bind (:map org-mode-map
         ("M--" . em-dash)
         ("M-;" . ellipsis)
         ("C-c c" . org-mode-insert-code)
         ("C-c a s" . org-emphasize)
         ("C-c a r" . org-ref)
         ("C-c a e" . outline-show-all)
         ("C-c a l" . lambduh))
  :config
  (setq org-footnote-section ""
        org-startup-with-inline-images t)
  (defun org-mode-insert-code ()
    (interactive)
    (org-emphasize ?~))
  (defun my-org-mode-hook ()
    (visual-line-mode)
    (electric-pair-mode nil))
  (unbind-key "C-c ;" org-mode-map)
  (unbind-key "C-,"   org-mode-map)
  (unbind-key "M-<left>" org-mode-map)
  (unbind-key "M-<right>" org-mode-map)
  (add-hook 'org-mode-hook 'my-org-mode-hook)
  (setq org-agenda-files (list "~/Dropbox/todo.org")))

(use-package swift-mode
  :config
  (setq swift-mode:basic-offset 2))

(use-package org-ref
  :defer
  :config
  (setq reftex-default-bibliography '("~/src/rsbook/bibliography/references.bib")
        org-ref-bibliography-notes "~/src/rsbook/bibliography/notes.org"
        org-ref-default-bibliography '("~/src/rsbook/bibliography/references.bib")
        org-ref-pdf-director "~/src/rsbook/pdfs"))

(use-package wc-goal-mode
  :hook (org-mode . wc-goal-mode))

(use-package haskell-mode
  :config

  (defun haskell-right-arrow ()
    "Insert a right arrow."
    (interactive)
    (insert (if (eolp) " -> " "->")))

  (defun haskell-left-arrow ()
    "Insert a left arrow."
    (interactive)
    (insert (if (eolp) " <- " "<-")))

  (defun my-haskell-mode-hook ()
    "Make sure the compile command is right."
    (setq-local compile-command "stack build --fast")
    (mac-auto-operator-composition-mode))

  (defun my-lithaskell-mode-hook ()
    "Turn off auto-indent for Literate Haskell snippets."
    (setq-local yas-indent-line nil))

  :mode ("\\.hs$" . haskell-mode)

  :hook ((haskell-mode . my-haskell-mode-hook)
         (literate-haskell-mode-hook . my-lithaskell-mode-hook))

  :bind (:map haskell-mode-map
         ("C-c a c" . haskell-cabal-visit-file)
	 ("C-c a b" . haskell-mode-stylish-buffer)
         ("C-c a i" . haskell-navigate-imports)
         ("C-c a ," . haskell-left-arrow)
         ("C-c a ." . haskell-right-arrow)))

(use-package intero
  :bind (:map haskell-mode-map
         ("C-c a r" . intero-repl)
         ("C-c a j" . intero-goto-definition)
         ("C-c a n" . intero-info)
         ("C-c a t" . intero-type-at)
         ("C-c a u" . intero-uses-at)
         ("C-c a s" . intero-apply-suggestions)))

(use-package idris-mode
  :bind (("C-c C-v" . idris-case-split)))

(use-package typescript-mode)

(use-package protobuf-mode)

(defun my-elisp-mode-hook ()
  "My elisp customizations."
  (electric-pair-mode))

(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file user-init-file))

(defun open-semantic-notes ()
  "Open my notes file."
  (interactive)
  (find-file "~/txt/semantic.org"))

(defun kill-all-buffers ()
  "Close all buffers."
  (interactive)
  (maybe-unset-buffer-modified)
  (mapc 'kill-buffer (buffer-list)))

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

(defun kill-buffer-with-prejudice ()
  (interactive)
  (when (current-buffer-matches-file-p) (set-buffer-modified-p nil))
  (kill-buffer))

(bind-key "C-x k"      'kill-buffer-with-prejudice)
(bind-key "C-c e"      'open-init-file)
(bind-key "C-c k"      'kill-all-buffers)
(bind-key "C-c o"      'open-semantic-notes)
(bind-key "s-<return>" 'eol-then-newline)
(bind-key "C-c 5"      'query-replace-regexp)
(bind-key "M-/"        'hippie-expand)
(bind-key "C-c '"      'switch-to-previous-buffer)
(bind-key "C-c \\"     'align-regexp)
(bind-key "C-c m"      'compile)
(bind-key "C-c 3"      'split-right-and-enter)
(bind-key "C-c /"      'comment-or-uncomment-region)
(bind-key "C-c x"      'ESC-prefix)
(bind-key "C-c l"      'goto-line)

;; macOS-style bindings, too (no cua-mode, it's nasty)
(bind-key "s-+"		'text-scale-increase)
(bind-key "s-_"		'text-scale-decrease)
(bind-key "s-s"         'save-buffer)
(bind-key "s-c"		'kill-ring-save)
(bind-key "s-v"		'yank)
(bind-key "s-z"		'undo)
(bind-key "s-a"		'mark-whole-buffer)
(bind-key "s-<"         'beginning-of-buffer)
(bind-key "s-x"         'kill-region)
(bind-key "<home>"      'beginning-of-buffer)
(bind-key "<end>"       'end-of-buffer)
(bind-key "s->"         'end-of-buffer)
(bind-key "M-_"         'em-dash)
(bind-key "M-;"         'ellipsis)
(unbind-key "C-z")

(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)             ; Always highlight the current line.
(show-paren-mode t)                 ; And point out matching parentheses.
(delete-selection-mode t)           ; Behave like any other sensible text editor would.
(column-number-mode t)              ; Show column information in the modeline.
(prettify-symbols-mode)             ; Use pretty Unicode symbols where possible.
(global-display-line-numbers-mode)  ; Emacs has this builtin now, it's fast

(setq
  compilation-always-kill t              ; Never prompt to kill a compilation session.
  compilation-scroll-output 'first-error ; Always scroll to the bottom.
  make-backup-files nil                  ; No backups, thanks.
  create-lockfiles nil                   ; Emacs sure loves to put lockfiles everywhere.
  default-directory "~/src"              ; My code lives here.
  inhibit-startup-screen t               ; No need to see GNU agitprop.
  kill-whole-line t                      ; Delete the whole line if C-k is hit at the beginning of a line.
  mac-command-modifier 'super            ; I'm not sure this is the right toggle, but whatever.
  require-final-newline t                ; Auto-insert trailing newlines.
  ring-bell-function 'ignore             ; Do not ding. Ever.
  use-dialog-box nil                     ; Dialogues always go in the modeline.
  frame-title-format "emacs – %b"        ; Put something useful in the status bar.
  initial-scratch-message nil            ; SHUT UP SHUT UP SHUT UP
  mac-option-modifier 'meta              ; why isn't this the default
  save-interprogram-paste-before-kill t  ; preserve paste to system ring
  enable-recursive-minibuffers t         ; don't fucking freak out if I use the minibuffer twice
  )

(add-to-list 'electric-pair-pairs '(?` . ?`)) ; electric-quote backticks

;; Bar cursors everywhere.

(setq-default cursor-type 'bar)
(setq-default indent-tabs-mode nil)

;; Always trim trailing whitespace.

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq debug-on-error nil)

;; Restore original GC threshold.
(setq gc-cons-threshold old-cons-threshold)
(makunbound 'old-cons-threshold)

(provide 'init)

;; goodbye, thanks for reading
