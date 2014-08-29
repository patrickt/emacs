;; Patrick Thomson's .emacs file.

;; global requirements
(mapcar 'require '(erlang recentf go-mode linum package saveplace))

;; load melpa and marmalade to start
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

;; GLOBAL MODES

;; Automatically indent and insert completing characters.
(electric-indent-mode +1)
(electric-pair-mode +1)

;; ECB
(ecb-minor-mode +1)

;; Persist window setups

;; Track recent files.
(recentf-mode +1)

;; Tabs, please.
(tabbar-mode +1)

;; blinky blinky
(blink-cursor-mode +1)

;; autocomplete in minibuffers
(icomplete-mode +1)
; (setq-default icicle-expand-input-to-common-match 4)



;; y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; TABS

 (defun my-tabbar-buffer-groups ()
   "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
               ((eq major-mode 'dired-mode) "emacs")
               (t "user"))))

(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; KEYBOARD SHORTCUTS

;; C-c C-r opens recent files. 
(global-set-key "\C-c\C-r" 'recentf-open-files)

;; C-c g (g for git) is magit
;; C-c C-g is git-grep
(global-set-key (kbd "\C-c g") 'magit-status)
(global-set-key (kbd "\C-c\C-g") 'magit-grep)

(global-set-key "\C-c\C-e"
                (lambda ()
                  (interactive)
                  (find-file "~/.emacs.d/init.el")))

;; M-g is now the same as M-x goto-line
(global-set-key "\eg" 'goto-line)

; spawn term with C-c C-s (s for shell)
(global-set-key "\C-c\C-s" 'term)

;; start ecb with C-c C-p
(global-set-key "\C-c\C-p" 'ecb-minor-mode)

;; SETTINGS

;; Bar cursor please
(setq-default cursor-type 'bar)

;; NEVER TABS. NEVER
(setq-default indent-tabs-mode nil)

; fullscreen plz
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

; emacs kindly stop leaving your trash everywhere
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/backups" t)))

; Ensuring Unicode compliance (may not be necessary)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; execute erlang-mode when encountering .erl files
(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))

; haskell
(add-to-list 'auto-mode-alist '("\\.hs?$" . haskell-mode))
(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

; run go-fmt before saving go code
(add-hook 'before-save-hook 'gofmt-before-save)

;; erlang indentation is fucky so don't do that

(add-hook 'erlang-mode-hook (lambda () (electric-indent-mode 0)))

(projectile-global-mode +1)

;; line numbers and line highlights
(column-number-mode)
(global-linum-mode)
(global-hl-line-mode)
(setq linum-format "%d ")

; does these do anything? unclear
(require 'uniquify)
(defconst font-lock-maximum-decoration t)

; default directory for minibuffer
(setq default-directory "~/src")

; blink matching parens
(show-paren-mode)
(setq blink-matching-paren t)

; miscellaneous modes
(icicle-mode t)
(semantic-mode t)
(achievements-mode t)
(setq-default save-place t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (badger)))
 '(custom-safe-themes (quote ("ad9fc392386f4859d28fe4ef3803585b51557838dbc072762117adad37e83585" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" default)))
 '(ecb-options-version "2.40")
 '(fci-rule-color "#2e2e2e")
 '(inhibit-startup-screen t)
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map (quote ((20 . "#dd5542") (40 . "#CC5542") (60 . "#fb8512") (80 . "#baba36") (100 . "#bdbc61") (120 . "#7d7c61") (140 . "#6abd50") (160 . "#6aaf50") (180 . "#6aa350") (200 . "#6a9550") (220 . "#6a8550") (240 . "#6a7550") (260 . "#9b55c3") (280 . "#6CA0A3") (300 . "#528fd1") (320 . "#5180b3") (340 . "#6380b3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(vc-follow-symlinks nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


