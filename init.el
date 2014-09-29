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

;; Smart modeline.
(require 'smart-mode-line)
(sml/setup)

;; Automatically indent and insert completing characters.
(electric-indent-mode t)
(autopair-global-mode t)

;; Track recent files.
(recentf-mode t)

;; Tabs, please.
(tabbar-mode t)

;; Flycheck, where possible.
(global-flycheck-mode t)

;; Projectile, where possible.
(projectile-global-mode t)

;; blinky blinky
(blink-cursor-mode t)

;; highlight parentheses
(show-paren-mode t)

;; autocomplete in minibuffers
(icomplete-mode 99)

;; delete selections, like LITERALLY EVERYWHERE ELSE
(delete-selection-mode t)

;; Highlight Fixmes and Todos.
(fic-ext-mode t)

;; Ido-mode
(ido-mode t)

;; Semantic parsing for tags
(semantic-mode t)

;; Column numbers in the gutter
(column-number-mode t)

;; Display time mode
(display-time-mode t)

;; Show the battery
(display-battery-mode t)

;; Line numbers everywhere
(global-linum-mode t)

;; Highlight the current line
(global-hl-line-mode t)

;; Screw you, Emacs
(cua-mode t)

;; Snippets
(require 'yasnippet)
(yas-global-mode t)

;; Autorevert
(global-auto-revert-mode t)

;; Load keychain
(keychain-refresh-environment)

;; y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; KEYBOARD SHORTCUTS

;; C-; is my namespace
(global-set-key (kbd "C-;") nil)

;; C-; C-; is ESC-prefix
(global-set-key (kbd "C-; C-;") 'ESC-prefix)

;; C-; r is recentf
(global-set-key (kbd "C-; r") 'recentf-open-files)

;; C-; g is git
(global-set-key (kbd "C-; g") 'magit-status)

;; C-; f is git-grep
(autoload 'magit-grep "magit" "Grep for files" t)
(global-set-key (kbd "C-; f") 'magit-grep)

;; C-; e edits my .emacs setup
(global-set-key (kbd "C-; e")
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "C-; z")
                (lambda ()
                  (interactive)
                  (find-file "~/.zshrc")))

;; C-; l is goto-line
(global-set-key (kbd "C-; l") 'goto-line)

; C-; s spawns a shell
(global-set-key (kbd "C-; s") 'ansi-term)

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

;; Shortcut for M-x
(global-set-key (kbd "C-; ;") 'execute-extended-command)

;; Go to other related file
(global-set-key (kbd "C-; o") 'ff-find-other-file)

(defun close-all-buffers ()
  "Close all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
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

;; don't alarm bell when going to end of document
(defun my-bell-function ()
  "Prevents the bell from going off during scroll events."
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;; oh my god shut up ECB
(require 'ecb)
(setq ecb-tip-of-the-day nil)

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
(setq-default rm-blacklist '(" yas"
                             " pair"
                             " guru"
                             " |"
                             " ||"
                             " AC"
                             " MRev"
                             ))

(require 'linum)
(setq linum-format "%d ")

;; require a final newline because POSIX, motherfuckers
(setq require-final-newline t)

; default directory for minibuffer
(setq default-directory "~/src")

;; blink matching parens please
(setq blink-matching-paren t)

;; Save place in the file
(setq-default save-place t)

;; I don't care what version of Emacs this is.
(setq inhibit-startup-screen t)

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

;; highlight indentation
(add-hook 'prog-mode-hook '(lambda ()
                             (highlight-indentation-mode +1)
                             (highlight-indentation-current-column-mode +1)
                             (auto-complete-mode +1)))

;; execute erlang-mode when encountering .erl files
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))

;; haskell
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; run go-fmt before saving go code
(add-hook 'go-mode-hook '(lambda ()
                           (add-hook 'before-save-hook 'gofmt-before-save)))

;; erlang indentation is fucky so don't do that
(add-hook 'erlang-mode-hook (lambda () (electric-indent-mode 0)))

;; TODO: look into eldoc for other languages.
(add-hook 'emacs-lisp-hook 'eldoc-mode)

;; C eldoc mode
(add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)

;; Word wrap when writing Markdown
(add-hook 'markdown-mode-hook 'visual-line-mode)

(provide 'init)
;;; init.el ends here
