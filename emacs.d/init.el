;;; package --- init.el
;;; Commentary:

;;; Code:

;; do this, since I usually run fish
(setq shell-file-name "/bin/bash")

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)

(add-to-list 'package-archives
	     '("mepla" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(use-package better-defaults)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("C-x g" . helm-browse-project)))

(use-package helm-ls-git)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; (use-package lsp-mode)
;; (with-eval-after-load 'lsp-mode
;;   (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;;   (require 'lsp-rust))

;; (use-package lsp-rust)

;; (use-package lsp-go)
;; (use-package lsp-haskell)
;; (use-package lsp-java)
;; (use-package lsp-python)

(use-package cider
  :config
  (setq clojure-indent-style :always-indent))

(use-package company
  :config
  (setq company-idle-delay nil)
  :bind (("M-SPC" . company-complete)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package highlight-indentation
  :ensure t
  :hook prog-mode)

;;;;;;;;;;;
;; Modes ;;
;;;;;;;;;;;

(global-linum-mode 1)
(global-hl-line-mode 1)
(tool-bar-mode -1)
(global-subword-mode 1)
(column-number-mode 1)

;;;;;;;;;;;;;;;;
;; Mode Hooks ;;
;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings (and some functions) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(windmove-default-keybindings)

(defun beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation."
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    ;; now if point is the same as when we started, we're already at indent start
    (if (= start (point))
        (beginning-of-line))))
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

(defun insert-line-before-and-indent ()
  "Insert a new line before the current line, keeping both at the same level of indentation."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))
(global-set-key (kbd "C-o") 'insert-line-before-and-indent)

(global-set-key (kbd "M-;") 'comment-line)

(global-set-key
 (kbd "C-v")
 '(lambda ()
    (interactive)
    (scroll-up-command 16)))

(global-set-key
 (kbd "M-v")
 '(lambda ()
    (interactive)
    (scroll-up-command -16)))

;;;;;;;;;;;;;;;;;;;
;; Misc settings ;;
;;;;;;;;;;;;;;;;;;;

(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono 8"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-set-variables - Dont edit below here! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(global-company-mode t)
 '(global-subword-mode t)
 '(package-selected-packages
   (quote
    (fish-completion fish-mode company-lsp lsp-go racket-mode web-beautify better-defaults centered-cursor-mode cider dockerfile-mode expand-region ggtags haskell-mode helm helm-ls-git helm-ls-hg highlight-indentation js2-mode lsp-mode lsp-rust markdown-mode company slime yaml-mode use-package solarized-theme slime-company rust-mode lua-mode flycheck)))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-ff-dotted-directory ((t (:background "gainsboro" :foreground "black"))))
 '(helm-ls-git-added-copied-face ((t (:foreground "forest green"))))
 '(helm-ls-git-added-modified-face ((t (:foreground "forest green"))))
 '(helm-ls-git-deleted-and-staged-face ((t (:foreground "brown"))))
 '(helm-ls-git-deleted-not-staged-face ((t (:foreground "brown"))))
 '(helm-ls-git-modified-and-staged-face ((t (:foreground "deep sky blue"))))
 '(helm-ls-git-modified-not-staged-face ((t (:foreground "deep sky blue"))))
 '(helm-ls-git-renamed-modified-face ((t (:foreground "dark orange"))))
 '(helm-ls-git-untracked-face ((t (:foreground "firebrick3"))))
 '(helm-selection ((t (:background "LightYellow3" :distant-foreground "black"))))
 '(hl-line ((t (:background "AntiqueWhite2")))))
