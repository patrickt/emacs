;;; init.el --- Patrick Thomson's .emacs file

;;; This file is in the public domain.

;;; Commentary:
;; This should be portable between OS X and sane Linux distros.
;; Cask and Pallet must be installed first.

;;; Code:

;; global requirements
(require 'package)
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; load melpa and marmalade to start
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;; Load per-machine settings.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; GLOBAL MODES

;; Load paths from shell
(exec-path-from-shell-initialize)

;; Import facilities to get crap out of the menu bar.
(require 'diminish)

;; Smart modeline.
;; (sml/setup)

;; ECB.
(require 'ecb)

;; Guide-key.
(require 'guide-key)
(guide-key-mode)
(diminish 'guide-key-mode)

;; ghc-mod.
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(require 'xcscope)
(cscope-setup)

;; Automatically indent and insert completing characters.
(electric-indent-mode t)
(autopair-global-mode t)
(diminish 'autopair-mode)

;; Electric indent.
(electric-indent-mode t)

;; Track recent files.
(require 'recentf)
(recentf-mode t)

;; Tabs, please.
(tabbar-mode t)

;; Flycheck, where possible.
(global-flycheck-mode t)

;; Projectile, where possible.
(projectile-global-mode t)
(diminish 'projectile-mode)

;; no toolbar please
(tool-bar-mode -1)

;; blinky blinky
(blink-cursor-mode t)

;; git gutter is neat as heck
(require 'git-gutter-fringe)
(global-git-gutter-mode)
(diminish 'git-gutter-mode)

;; need company
(require 'company)

;; highlight parentheses
(show-paren-mode t)

;; autocomplete in minibuffers
(icomplete-mode 99)

;; delete selections, like LITERALLY EVERYWHERE ELSE
(delete-selection-mode t)

;; Highlight Fixmes and Todos.
(fic-ext-mode t)
(diminish 'fic-ext-mode)

;; Ido-mode
(ido-mode t)

;; Company mode
(global-company-mode t)
(diminish 'company-mode)

;; Eldoc
(eldoc-mode t)
(diminish 'eldoc-mode)

;; Semantic parsing for tags
(semantic-mode t)

;; Column numbers in the gutter
(column-number-mode 1)

;; Display time mode
(display-time-mode t)

;; Show the battery
;; This doesn't work on OSX for reasons that are still unclear
(unless (eq system-type 'darwin)
  (display-battery-mode t))

;; Line numbers everywhere
(require 'linum)
(global-linum-mode t)
(setq linum-format "%d ")

;; Highlight the current line
(global-hl-line-mode t)

;; Screw you, Emacs
(cua-mode t)

(electric-indent-mode 1)

;; Snippets
(require 'yasnippet)
(yas-global-mode t)
(diminish 'yas-minor-mode)

;; Autorevert
(global-auto-revert-mode t)

(require 'magit)
(diminish 'magit-auto-revert-mode)

;; Load keychain
(keychain-refresh-environment)

;; Undo trees
(undo-tree-mode +1)

;; y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; KEYBOARD SHORTCUTS

;; C-; is my namespace
(global-set-key (kbd "C-;") nil)

;; the bs package provides a nicer buffer list
(global-set-key (kbd "C-; b") 'bs-show)

;; C-; C-; is ESC-prefix
(global-set-key (kbd "C-; C-;") 'ESC-prefix)

;; C-; r is recentf
(global-set-key (kbd "C-; r") 'recentf-open-files)

;; C-; g is git
(global-set-key (kbd "C-; g") 'magit-status)

(global-set-key (kbd "C-; /") 'comment-or-uncomment-region)

;; C-; f is git-grep
(autoload 'magit-grep "magit" "Grep for files" t)
(global-set-key (kbd "C-; f") 'magit-grep)

(defun find-init-el ()
  "Open ~/.emacs.d/init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; C-; e edits my .emacs setup
(global-set-key (kbd "C-; e") 'find-init-el)

(defun find-zshrc ()
  "Open ~/.zshrc."
  (interactive)
  (find-file "~/.zshrc"))

;; C-; z edits .zshrc
(global-set-key (kbd "C-; z") 'find-zshrc)

;; I hate forward delete
(global-set-key (kbd "<deletechar>") 'autopair-backspace)

;; And I hate Insert even more
(global-unset-key (kbd "<insert>"))

;; C-; l is goto-line
(global-set-key (kbd "C-; l") 'goto-line)

; C-; s spawns a shell
(global-set-key (kbd "C-; s") 'eshell)

;; Compile current file
(global-set-key (kbd "C-; c") 'projectile-compile-project)

;; Goto next error
(global-set-key (kbd "C-; d") 'flycheck-tip-cycle)

;; Find file in project
(global-set-key (kbd "C-; p") 'projectile-find-file)

;; Projectile commander
(global-set-key (kbd "C-; P") 'projectile-commander)

;; Go to last change
(global-set-key (kbd "C-; .") 'goto-last-change)

;; Rebind everything to smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-; ;") 'smex)

;; Go to other related file
(global-set-key (kbd "C-; o") 'ff-find-other-file)

;; Keyspace for ace-jump
(global-set-key (kbd "C-; j") nil)
(global-set-key (kbd "C-; j j") 'ace-jump-word-mode)
(global-set-key (kbd "C-; j c") 'ace-jump-char-mode)
(global-set-key (kbd "C-; j l") 'ace-jump-line-mode)

;; Use company instead of dabbrev-expand or hippie-expand
(global-set-key (kbd "M-/") 'company-complete)
(global-set-key (kbd "C-.") 'company-complete)

;; Browse kill ring with C-; y (mnemonic: yank)
(global-set-key (kbd "C-; y") 'popup-kill-ring)

(defun insert-em-dash ()
  "Insert an em-dash."
  (interactive)
  (insert-char ?—))

(global-set-key (kbd "M-_") 'insert-em-dash)

(defun kill-all-buffers ()
  "Close all buffers."
  (interactive)
  (ecb-deactivate)
  (mapc 'kill-buffer (buffer-list)))

(global-set-key (kbd "C-; k") 'kill-all-buffers)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-'") 'switch-to-previous-buffer)

(global-set-key (kbd "C-; SPC") 'yas-expand-from-trigger-key)


(defun eol-then-newline ()
  "Move to EOL then insert a newline, a la Cmd-Ret in Textmate."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key (kbd "s-<return>") 'eol-then-newline)

;; SETTINGS

(setq ring-bell-function 'ignore)

;; oh my god shut up ECB
(setq ecb-tip-of-the-day nil)

;; no backup files at all
(setq make-backup-files nil)

;; Bar cursor please
(setq-default cursor-type 'bar)

;; yasnippet, please be quiet
(setq yas-verbosity 1)

;; please don't scroll so hard
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; no dinging please
(setq-default visual-bell t)

;; NEVER TABS. NEVER
(setq-default indent-tabs-mode nil)

;; fullscreen plz
(unless (eql system-type 'darwin)
  (add-to-list 'default-frame-alist '(fullscreen . fullboth)))

;; emacs kindly stop leaving your trash everywhere
(setq create-lockfiles nil)
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/backups" t)))

;; Ensuring Unicode compliance (may not be necessary)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; point erlang flycheck in the right direction
(setq-default flycheck-erlang-executable "/usr/local/erl/bin/erlc")

;; Modeline customization
(setq-default rm-blacklist nil)

(require 'linum)

;; require a final newline because POSIX, motherfuckers
(setq require-final-newline t)

;; avoid the silly buffer<2> things
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

; default directory for minibuffer
(setq default-directory "~/src")

;; blink matching parens please
(setq blink-matching-paren t)

;; Save place in the file
(setq-default save-place t)

;; I don't care what version of Emacs this is.
(setq inhibit-startup-screen t)

;; Exclude all of emacs's garbage from the recentf list
(add-to-list 'recentf-exclude "\\.emacs.d")
(add-to-list 'recentf-exclude "\\.ido\\.last")

;; Enable guide-key mode for my namespace
(setq guide-key/guide-key-sequence '("C-c" "C-;"))

;; Don't try to use graphical boxes, ever.
;; They don't work at all on OS X.
(setq-default use-dialog-box nil)

;; TABS

(defun my-tabbar-buffer-groups ()
  "Group tabs by whether they are Emacs-local or user-opened.
There are two groups: Emacs buffers (those whose name starts with '*', plus
dired buffers), and the rest.  This works at least with Emacs v24.2 using
tabbar.el v1.7."
   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
               ((eq major-mode 'dired-mode) "emacs")
               (t "user"))))

(setq-default tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; HOOKS AND AUTO-MODES

;; load ghc-mod and add hooks appropriately
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-to-list 'company-backends 'company-ghc)

;; highlight indentation
;; (add-hook 'prog-mode-hook '(lambda ()
;;                              (highlight-indentation-mode +1)
;;                              (diminish 'highlight-indentation-mode)
;;                              (highlight-indentation-current-column-mode +1)
;;                              (diminish 'highlight-indentation-current-column-mode)
;;                              ))

;; execute erlang-mode when encountering .erl files
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))

;; haskell
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; ruby and gemfiles
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))

;; run go-fmt before saving go code
(add-hook 'go-mode-hook '(lambda ()
                           (add-hook 'before-save-hook 'gofmt-before-save)))

;; erlang indentation is fucky so don't do that
(add-hook 'erlang-mode-hook (lambda () (electric-indent-mode 0)))

;; C eldoc mode
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; Word wrap when writing Markdown
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook '(lambda ()
                                 (local-unset-key (kbd "M-<left>"))
                                 (local-unset-key (kbd "M-<right>"))))

(provide 'init)
;;; init.el ends here
