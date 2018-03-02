;;; package --- init.el
;;; Commentary:

;;; Code:

;;; Packages

(require 'package)

(add-to-list 'package-archives
	     '("mepla" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

(use-package better-defaults)

(setq shell-file-name "/bin/bash")

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
;; (use-package lsp-go)
;; (use-package lsp-haskell)
;; (use-package lsp-java)
;; (use-package lsp-python)
;; (use-package lsp-rust)

(use-package cider
  :config
  (setq clojure-indent-style :always-indent))

;;; Modes

(global-linum-mode 1)
(global-hl-line-mode 1)
(tool-bar-mode -1)

;;; Mode Hooks

(add-hook 'prog-mode-hook 'highlight-indentation-mode)

;;; Keybindings (and some functions)

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


;;; Misc settings

(setq inhibit-startup-screen t)
(set-frame-font "Roboto Mono 10")

;;; custom-set-variables - Dont edit below here!

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (dockerfile-mode highlight-indentation flycheck-rust lsp-rust rust-mode slime slime-company markdown-preview-mode markdown-mode yaml-mode js2-mode flycheck-clojure flycheck-gometalinter flycheck-haskell flycheck-kotlin helm-clojuredocs expand-region solarized-theme ac-cider cider cider-decompile cider-eval-sexp-fu helm-cider helm-cider-history ggtags centered-cursor-mode use-package lua-mode helm-ls-svn helm-ls-hg helm-ls-git better-defaults)))
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