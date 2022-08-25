;;; init.el --- Patrick Thomson's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file loads use-package, org-mode, and compiles and executes readme.org
;;
;;; Code:

(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(setq package-native-compile t)
(unless (package-installed-p 'use-package)
  (message "refreshing contents")
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'org-install)
  (require 'ob-tangle))

(defun reload-config ()
  "Reload the literate config from ~/.config/emacs/readme.org."
  (interactive)
  (org-babel-load-file "~/.config/emacs/readme.org"))

(setq max-lisp-eval-depth 2000)

(reload-config)
(provide 'init)
;;; init.el ends here
