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

(use-package better-defaults
  :ensure t)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("C-x g" . helm-browse-project)))

(use-package helm-ls-git
  :ensure t)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package cider
  :config
  (setq clojure-indent-style :always-indent))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay nil)
  :bind (("M-SPC" . company-complete)))

(use-package helm-company
  :bind (("M-S-SPC" . helm-company)))

(use-package projectile)
(use-package helm-projectile
  :bind (("C-c p s" . helm-projectile-grep)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :bind (("C-." . js2-next-error))
  :config
  (setq js2-global-externs
        '("ontraport" "describe" "it" "beforeEach" "afterEach" "beforeAll" "afterAll" "expect" "jasmine"
          "test_runner" "steal" "$" "$l" "_")))

(use-package elpy
  :ensure t
  :config
  (setq elpy-rpc-python-command "python3")
  :init (elpy-enable))

(use-package web-mode
  :mode "\\.ejs\\'")

(use-package typescript-mode
  :mode "\\.tsx\\'")

(use-package tide
  :config
  (defun setup-tide-mode ()
    "Set up tide mode in the current buffer.  Have hooks call this."
    (tide-setup)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (tide-hl-identifier-mode +1))
  ;; (add-hook 'web-mode-hook
  ;;           (lambda ()
  ;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
  ;;               (setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

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

(add-hook 'prog-mode-hook 'highlight-indentation-mode)

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

(defun copy-file-name ()
  "Copy the current file's name to the clipboard."
  (interactive)
  (let ((begin (point))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "No file name for current buffer")
      (progn
        (insert filename)
        (kill-region begin (point))
        (message filename)))))

;;;;;;;;;;;;;;;;;;;
;; Misc settings ;;
;;;;;;;;;;;;;;;;;;;

(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(font . "Roboto Mono 9"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; (add-to-list 'default-frame-alist '(font . "Noto Sans Mono 8"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom values worth noting ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for use with solarized-light theme

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(helm-ff-dotted-directory ((t (:background "gainsboro" :foreground "black"))))
;;  '(helm-ls-git-added-copied-face ((t (:foreground "forest green"))))
;;  '(helm-ls-git-added-modified-face ((t (:foreground "forest green"))))
;;  '(helm-ls-git-deleted-and-staged-face ((t (:foreground "brown"))))
;;  '(helm-ls-git-deleted-not-staged-face ((t (:foreground "brown"))))
;;  '(helm-ls-git-modified-and-staged-face ((t (:foreground "deep sky blue"))))
;;  '(helm-ls-git-modified-not-staged-face ((t (:foreground "deep sky blue"))))
;;  '(helm-ls-git-renamed-modified-face ((t (:foreground "dark orange"))))
;;  '(helm-ls-git-untracked-face ((t (:foreground "firebrick3"))))
;;  '(helm-selection ((t (:background "LightYellow3" :distant-foreground "black"))))
;;  '(hl-line ((t (:background "AntiqueWhite2")))))
