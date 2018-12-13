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

(use-package diminish)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js2-mode))

(use-package anzu
  :diminish anzu-mode
  :config (global-anzu-mode))

;; (use-package cider
;;   :config
;;   (setq clojure-indent-style :always-indent))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay nil)
  :bind (("M-SPC" . company-complete)))

(use-package helm-company
  :bind (("M-S-SPC" . helm-company)))

(use-package projectile
  :config
  (unbind-key "C-c p l" projectile-mode-map)
  (unbind-key "C-c p f" projectile-mode-map))

(use-package helm-projectile
  :bind (("C-c p s" . helm-projectile-rg)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :bind (("C-." . js2-next-error))
  :config
  (setq js2-global-externs
        '("ontraport"
          "setTimeout" "setInterval"
          "describe" "it" "beforeEach" "afterEach" "beforeAll" "afterAll" "expect" "jasmine"
          "test_runner" "steal" "$" "$l" "_")))

;; elpy and tide are disabled, try to replace them with eglot or lsp-mode! Except for JS2. for now.
;; (use-package elpy
;;   :ensure t
;;   :config
;;   (setq elpy-rpc-python-command "python3")
;;   :init (elpy-enable))

(use-package php-mode
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook (lambda () (flycheck-mode -1))))

(use-package web-mode
  :mode "\\.ejs\\'"
  :config
  (add-hook 'web-mode-hook 'emmet-mode))

;; (use-package typescript-mode
;;   :mode "\\.tsx\\'")

;; (use-package tide
;;   :config
;;   (defun setup-tide-mode ()
;;     "Set up tide mode in the current buffer.  Have hooks call this."
;;     (tide-setup)
;;     (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;     (tide-hl-identifier-mode +1))
;;   ;; (add-hook 'web-mode-hook
;;   ;;           (lambda ()
;;   ;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;   ;;               (setup-tide-mode))))
;;   (flycheck-add-mode 'typescript-tslint 'web-mode)
;;   (setq company-tooltip-align-annotations t)
;;   (add-hook 'before-save-hook 'tide-format-before-save)
;;   (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

;; (use-package flymake
;;   :bind (("M-n" . flymake-goto-next-error)
;;          ("M-p" . flymake-goto-prev-error)))

;; keep this package after projectile, so these key bindings are last
(use-package helm-ls-git
  :ensure t
  :bind (("C-c p f" . helm-ls-git-ls)))

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
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "No file name for current buffer")
      (kill-new filename))))

(defun insert-branch-name ()
  "When making a commit in magit, insert the branch's name as the first word of the commit."
  (interactive)
  (unless (string= (thing-at-point 'word) "Merge")
    (save-excursion
      (forward-line 4)
      (let ((branch-line (thing-at-point 'line))
            (branch-name-regexp "On branch \\(\\w*/\\)?\\(\\w+-?[0-9]*\\)"))
        (string-match branch-name-regexp branch-line)
        (let ((ticket-name (match-string 2 branch-line)))
          (when ticket-name
            (goto-char 0)
            (insert ticket-name)))))
    (end-of-line)
    (insert " ")))

;; (defun position-cursor-in-commit-buffer ()
;;   "Move the cursor to the end of any text in the first line of the commit buffer."
;;   (interactive)
;;   (move-end-of-line nil))

;; Add a newline as line 2, then move to end of line 1 (assuming starting at pos 0)
;; (fset 'fix-commit-editmsg
;;       [?\C-e return ?\C-b ? ])

(defun fix-commit-editmsg ()
  "Add an empty line as line 2, move cursor in to position for typing."
  (interactive)
  (move-end-of-line nil)
  (newline)
  (move-end-of-line 0))

;; A macro/function to find the next function definition in a JS file.
(fset 'js-next-function
   [?\C-s return ?: ?  ?? ?f ?u ?n ?c ?t ?i ?o ?n ?  ?? ?\( return])

(fset 'js-prev-function
   [?\C-r return ?: ?  ?? ?f ?u ?n ?c ?t ?i ?o ?n ?  ?? ?\( return])

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
;; (add-hook 'prog-mode-hook 'auto-save-mode)

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(add-hook 'git-commit-setup-hook
          'fix-commit-editmsg)
;;           'insert-branch-name)

(let ((disable-line-numbers
       '(lambda ()
          (display-line-numbers-mode -1))))
  (add-hook 'git-commit-setup-hook disable-line-numbers)
  (add-hook 'magit-mode-hook disable-line-numbers)
  (add-hook 'term-mode-hook disable-line-numbers))



;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

(windmove-default-keybindings)

(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

(global-set-key (kbd "C-o") 'insert-line-before-and-indent)

(global-set-key (kbd "M-;") 'comment-line)

(global-set-key (kbd "C-x g") 'magit-status)

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

(eval-after-load 'js2-mode
  '(progn
     (define-key js-mode-map (kbd "C-c f n") 'js-next-function)
     (define-key js-mode-map (kbd "C-c f p") 'js-prev-function)))

;;;;;;;;;;;;;;;;;;;
;; Misc settings ;;
;;;;;;;;;;;;;;;;;;;

;; (setq display-line-numbers 1)

(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(font . "Roboto Mono 9"))
;; (add-to-list 'default-frame-alist '(font . "Noto Sans Mono 8"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(provide 'init)
;;; init.el ends here

