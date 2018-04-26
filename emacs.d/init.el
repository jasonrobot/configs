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

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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

(use-package nlinum
  :config (global-nlinum-mode))

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

;;;;;;;;;;;
;; Modes ;;
;;;;;;;;;;;
;; Any external packages should be in the previous section.
;; This is just for built-in modes.

(global-hl-line-mode 1)
(tool-bar-mode -1)
(global-subword-mode 1)
(column-number-mode 1)

(add-hook 'prog-mode-hook 'highlight-indentation-mode)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(defun beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation."
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    ;; now if point is the same as when we started, we're already at indent start
    (if (= start (point))
        (beginning-of-line))))

(defun insert-line-before-and-indent ()
  "Insert a new line before the current line, keeping both at the same level of indentation."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keybindings (and some functions) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(windmove-default-keybindings)

(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

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
(add-to-list 'default-frame-alist '(font . "Roboto Mono 9"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;; (add-to-list 'default-frame-alist '(font . "Noto Sans Mono 8"))

(provide 'init)
;;; init.el ends here

