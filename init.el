;;; init.el -- Patrick Thomson's emacs config

;;; Commentary:
;; This file is in the public domain.

;;; Code:

(setq gc-cons-threshold 100000000)

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(package-initialize)

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

 (use-package noctilux-theme
  :ensure t
  :init (load-theme 'noctilux))

(use-package powerline
  :ensure t
  :init (powerline-default-theme)
  :config (setq powerline-display-hud nil
                powerline-default-separator nil))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package recentf
  :init (recentf-mode t)
  :config (add-to-list 'recentf-exclude "\\.emacs.d"))

(use-package nix-mode
  :ensure t
  :mode ("\\.nix$" . nix-mode))

(use-package helm
  :ensure t
  :diminish helm-mode
  :bind (("C-c ;" . helm-M-x)
         ("C-c r" . helm-recentf)
         ("C-c y" . helm-show-kill-ring)
         ("C-c G" . helm-do-grep)
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
  (setq company-minimum-prefix-length 2)
  (define-key company-active-map (kbd "C-n") 'company-select-next))

(use-package prodigy
  :ensure t
  :bind (("C-c q" . prodigy))
  :config
  (progn
    (prodigy-define-service
      :name "snapboard: gulp watch"
      :command "gulp"
      :args '("watch")
      :cwd "~/src/snapboard/snapboard")
    (prodigy-define-service
      :name "snapboard"
      :command "stack"
      :args '("exec" "snapboard")
      :stop-signal 'kill
      :cwd "~/src/snapboard/snapboard")
    (prodigy-define-service
      :name "PostgreSQL"
      :command "postgres"
      :args '("-D" "/usr/local/var/postgres"))))

(use-package helm-make
  :ensure t
  :bind ("C-c m" . helm-make))

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

(use-package ace-jump-mode
  :ensure t
  :bind (("C-l"   . ace-jump-line-mode)
         ("C-c j" . ace-jump-mode)))

(use-package ace-jump-helm-line
  :defer helm
  :config (define-key helm-map (kbd "C-j") 'ace-jump-helm-line))

(use-package duplicate-thing
  :ensure t
  :bind (("C-c u" . duplicate-thing)))

(defun my-haskell-mode-hook ()
  "My haskell-mode configuration."
  (interactive-haskell-mode)
  (haskell-decl-scan-mode)
  (turn-on-haskell-indent)
  (haskell-doc-mode)

  (mapc 'diminish '(interactive-haskell-mode
                    haskell-doc-mode
                    haskell-indent-mode)))

(defun my-cabal-mode-hook ()
  "My cabal configuration."
  (electric-indent-local-mode -1))

(defun haskell-bring-and-compile ()
  "Bring the compilation window to front and compile."
  (interactive)
  (haskell-interactive-bring)
  (haskell-process-cabal-build))

(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (add-hook 'haskell-cabal-mode-hook 'my-cabal-mode-hook)
  :bind (("C-c a t" . haskell-process-do-type)
         ("C-c a i" . haskell-process-do-info)
         ("C-c a c" . haskell-cabal-visit-file)
         ("C-c a f" . haskell-interactive-bring)
         ("C-c a b" . haskell-mode-stylish-buffer)
         ("C-c c"   . haskell-bring-and-compile)
         ("C-c C-c" . haskell-bring-and-compile)
         ("C-c a i" . haskell-add-import)
         ("C-c a F" . haskell-session-kill)
         ("C-c a s" . haskell-hayoo)
         ("SPC" . haskell-mode-contextual-space))
  :mode ("\\.hs$" . haskell-mode)
  :config (setq
           haskell-notify-p t
           haskell-ask-also-kill-buffers nil
           haskell-font-lock-symbols t
           haskell-mode-contextual-import-completion nil
           haskell-process-load-or-reload-prompt t
           haskell-interactive-mode-scroll-to-bottom t
           haskell-process-type 'stack-ghci
           haskell-process-show-debug-tips nil
           haskell-process-suggest-remove-import-lines t
           haskell-process-log t
           haskell-doc-show-reserved nil
           haskell-doc-show-global-types t
           haskell-indent-after-keywords '(("where" 4 0)
                                           ("of" 4)
                                           ("do" 4)
                                           ("in" 4)
                                           "if"
                                           "then"
                                           "else"
                                           "let"))
  (defalias 'haskell-complete-module-read 'helm--completing-read-default))

(use-package xml-mode
  :config (setq-default nxml-child-indent 4)
  :mode ("\\.tpl$" . xml-mode))

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

(defun stop-using-minibuffer ()
  "Kill the minibuffer."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

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
(bind-key "C-,"    'other-window)
(bind-key "C-c /"  'comment-or-uncomment-region)
(bind-key "C-c x"  'ESC-prefix)
(bind-key "s-+"    'text-scale-increase)
(bind-key "s-_"    'text-scale-decrease)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(column-number-mode t)
(display-time-mode t)
(auto-save-mode -1)

(setq
 blink-matching-paren t
 compilation-always-kill t
 compilation-scroll-output t
 create-lockfiles nil
 default-directory "~/src"
 inhibit-startup-screen t
 initial-scratch-message nil
 kill-whole-line t
 make-backup-files nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 require-final-newline t
 ring-bell-function 'ignore
 use-dialog-box nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default
 cursor-type 'bar
 indent-tabs-mode nil)

(provide 'init)

;;; init.el ends here
