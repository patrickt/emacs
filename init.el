;;; init.el -- NixOS-specific init file

;;; Commentary:
;; seriously, fuck computers

;;; Code:

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(package-initialize)
(package-refresh-contents)

(global-set-key (kbd "C-c a") 'nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(package-install 'use-package)

(setq load-prefer-newer t
      warning-minimum-level :debug
      warning-minimum-log-level :debug)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(load-theme 'deeper-blue)

(use-package smart-mode-line
  :ensure t
  :init (sml/setup)
  :config (setq sml/theme 'respectful))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package recentf
  :init (recentf-mode t)
  :config
  (add-to-list 'recentf-exclude "\\.emacs.d"))

(use-package nix-mode
  :ensure t
  :mode ("\\.nix$" . nix-mode))

(use-package helm
  :ensure t
  :init (progn
	  (require 'helm-command)
          (helm-autoresize-mode t)
	  (helm-mode t))

  :bind (("C-c ;" . helm-M-x)
	 ("C-c r" . helm-recentf)
	 ("C-c y" . helm-show-kill-ring)
         ("C-c G" . helm-do-grep)
         ("C-c b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list))
  :config (setq-default helm-M-x-fuzzy-match t)
  :diminish helm-mode)

(use-package company
  :ensure t
  :init (global-company-mode 1)
  :bind (("M-/" . company-complete))
  :config (setq company-minimum-prefix-length 2)
  :diminish company-mode)

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
  :ensure t
  :bind (("C-c g" . magit-status))
  :init (global-auto-revert-mode t)
  :config (setq-default magit-last-seen-setup-instructions "1.4.0"))

(use-package git-gutter
  :ensure t
  :init (global-git-gutter-mode)
  :diminish git-gutter-mode)

(use-package yasnippet
  :ensure t
  :defer 1
  :diminish yas-minor-mode
  :config
  (yas-global-mode +1)
  (setq yas-verbosity +1
        yas-prompt-functions '(yas-completing-prompt)))

(use-package saveplace
  :config (setq-default save-place t))

(use-package haskell-snippets
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
  :mode ("\\.md$" . markdown-mode)
  :ensure t)

(use-package scss-mode
  :ensure t)

(use-package ace-jump-mode
  :ensure t
  :bind (("C-l"   . ace-jump-line-mode)
         ("C-c l" . ace-jump-mode)))

(use-package haskell-mode
  :ensure t
  :init (progn
	  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
          (add-hook 'haskell-mode-hook 'haskell-doc-mode)
	  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
          (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan))
  :bind (("C-c a t" . haskell-process-do-type)
	 ("C-c a i" . haskell-process-do-info)
	 ("C-c a c" . haskell-cabal-visit-file)
	 ("C-c a f" . haskell-interactive-bring)
         ("C-c a i" . haskell-add-import)
	 ("C-c a F" . haskell-session-kill)
	 ("SPC" . haskell-mode-contextual-space))
  :mode ("\\.hs$" . haskell-mode)
  :config (setq
           haskell-notify-p t
           haskell-ask-also-kill-buffers nil
           haskell-font-lock-symbols t
           haskell-mode-contextual-import-completion nil
	   haskell-process-type 'stack-ghci
	   haskell-process-load-or-reload-prompt nil
	   haskell-process-show-debug-tips nil
	   haskell-process-suggest-remove-import-lines t
	   haskell-process-log t
           haskell-doc-show-reserved nil
           haskell-doc-show-global-types t)
  (defalias 'haskell-complete-module-read 'helm--completing-read-default))

(use-package xml-mode
  :config (setq nxml-child-indent 4)
  :mode ("\\.tpl$" . xml-mode))

(add-hook 'emacs-lisp-mode 'flycheck-mode)
(add-hook 'emacs-lisp-mode 'eldoc-mode)
(add-hook 'emacs-lisp-mode 'electric-pair-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defadvice isearch-search (after isearch-no-fail activate)
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

(bind-key "C-c '" 'switch-to-previous-buffer)
(bind-key "C-c \\" 'align-regexp)
(bind-key "C-," 'other-window)
(bind-key "C-c /" 'comment-or-uncomment-region)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode t)
(show-paren-mode t)
(delete-selection-mode t)
(column-number-mode t)
(display-time-mode t)

(setq
 kill-whole-line t
 compilation-scroll-output t
 inhibit-startup-screen t
 initial-scratch-message nil
 blink-matching-paren t
 ring-bell-function 'ignore
 use-dialog-box nil
 make-backup-files nil
 compilation-always-kill t
 create-lockfiles nil
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 require-final-newline t)

(setq-default
 cursor-type 'bar
 indent-tabs-mode nil)

(provide 'init)

;;; init.el ends here
