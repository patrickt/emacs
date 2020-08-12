;;; init.el --- Patrick Thomson's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; This file loads use-package, org-mode, and compiles and executes readme.org
;;
;;; Code:

(require 'package)

(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'org-install)
(require 'ob-tangle)

(defun reload-config ()
  "Reload the literate config from ~/.config/emacs/readme.org."
  (interactive)
  (org-babel-load-file "~/.config/emacs/readme.org"))

(setq max-lisp-eval-depth 2000)

(reload-config)

;;; init.el ends here
