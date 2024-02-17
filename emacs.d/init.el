;;; package --- init.el
;;; Commentary:

;;; Code:

;; do this, since I usually run fish
(setq shell-file-name "/bin/bash")

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)

;; (add-to-list 'package-archives
;;              '("mepla" . "http://melpa.milkbox.net/packages/")
;;              t)

;;temp fix till elpa stops being weird
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("mepla" . "http://melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package avy
  :ensure t
  :bind (("C-:" . 'avy-goto-char)
         ("C-M-:" . 'avy-goto-char-2)))

(use-package better-defaults
  :ensure t)

(use-package cider)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("C-h o" . helm-apropos)))

(defun -strip-newlines-from-json (args) ;(output checker buffer)
  "Remove newlines from the first element of ARGS."
  (cons (replace-regexp-in-string "\n" "" (car args))
        (cdr args)))

(defun -strip-warning-message-from-tslint (args) ;(output checker buffer)
  "Remove a tslint error from ARGS that causes them to be invalid JSON."
  (cons (replace-regexp-in-string "=\\{13\\}\n\\(.*\n\\)*=\\{13\\}\n"
                                  ""
                                  (car args))
        (cdr args)))

(use-package fish-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (advice-add 'flycheck-parse-eslint
              :filter-args
              #'-strip-newlines-from-json)
  (advice-add 'flycheck-parse-eslint
              :filter-args
              #'-strip-warning-message-from-tslint))

(use-package anzu
  :ensure t
  :delight anzu-mode
  :config (global-anzu-mode))

(use-package cider)

(use-package clojure-mode
  :config (define-clojure-indent ; this is for routes in compojure
            (defroutes 'defun)
            (GET 2)
            (POST 2)
            (PUT 2)
            (DELETE 2)
            (HEAD 2)
            (ANY 2)
            (OPTIONS 2)
            (PATCH 2)
            (rfn 2)
            (let-routes 1)
            (context 2))
  :bind (("C-:" . 'avy-goto-char)))

(use-package company
  :ensure t
  :delight company-mode
  :config (setq company-idle-delay nil)
  :hook ((typescript-ts-mode . company-mode))
  :bind (("M-SPC" . company-complete)))

(use-package crystal-mode
  :ensure t
  :bind (("C-c C-f" . crystal-tool-format)))

(use-package dash
  :ensure t)

(use-package emmet-mode
  :ensure t
  :hook web-mode)

(use-package forge
  :ensure t
  :after magit)

(use-package gradle-mode
  ;; :ensure t
  :mode "\\.gradle\\'")

(use-package helm
  :ensure t)

(use-package helm-company
  :ensure t
  :bind (("M-S-SPC" . helm-company)))

(use-package helm-ls-git
  :ensure t
  :after (helm projectile) ;after projectile, so these key bindings are last
  :bind (("C-c p f" . helm-ls-git)))

(use-package helm-projectile
  :ensure t
  :after (projectile) ; Must be set up after projectile
  :bind (("C-c p s" . helm-projectile-rg)))

(use-package highlight-indentation
  :ensure t
  :delight highlight-indentation-mode)

(use-package js2-mode
  :ensure t
  :mode "\\.m?js\\'"
  :delight "JS²"
  ;; :bind (("C-." . js2-next-error))
  :config
  (setq js2-global-externs
        '("setTimeout" "setInterval" "clearTimeout" "clearInterval"
          "describe" "it" "beforeEach" "afterEach" "beforeAll" "afterAll" "expect" "jasmine")))

(use-package kotlin-mode
  :ensure t
  :mode "\\.kt\\'")

(load "~/.emacs.d/helm-magit-recent-branches.el")

(use-package magit
  :ensure t
  :bind (("C-c b" . helm-magit-recent-branches))
  :after (s)
  :config
  (advice-add 'magit-diff--dwim
              :filter-return
              #'my-fix-diff-range)
  (advice-add 'magit-branch-maybe-adjust-upstream
              :override
              #'my-always-make-upstream-origin)
  (advice-add 'vc-git-mode-line-string
            :filter-return
            (lambda (ret)
              (s-replace-regexp ".*\\(PORT-[0-9]\\{4,5\\}\\).*" "\\1" ret))))

(use-package projectile
  :ensure t
  :delight
  '(:eval (let ((mode-string " プロジ"))
            (if (projectile-project-p)
                (let ((name (projectile-project-name))
                      (type (symbol-name (projectile-project-type))))
                  (concat mode-string
                          (concat "《" name ":" type "》"))
                  mode-string))))
  :config
  ;; These are some projectile functions I dont care about, and I want them for other uses.
  (unbind-key "C-c p l" projectile-mode-map)
  (unbind-key "C-c p f" projectile-mode-map))

(use-package php-mode
  :mode "\\.php\\'"
  :config
  (add-hook 'php-mode-hook (lambda () (flycheck-mode -1))))

(use-package request
  :ensure t)

;; hyper-useful string library
(use-package s
  :ensure t)

;; (use-package slime
;;   :ensure t
;;   :config
;;   (setq inferior-lisp-program "sbcl"))

(use-package tide
  :ensure t
  :after (typescript-ts-mode company flycheck)
  :init
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
  :config
  (setq tide-format-options '(:indentSize 2)))

  ;; :config
  ;; (advice-add 'tide-format
  ;;             :override
  ;;             (lambda () nil))) ;TODO use eslint to format this.

;; (use-package typescript-mode
;;   :ensure t
;;   :after (js2-mode)
;;   ;; :bind (("<RET>" . c-indent-new-comment-line)))
;;   :bind (("M-j" . js2-line-break)))

(use-package typescript-ts-mode
  :mode "\\.ts\\'")

(use-package web-mode
  :ensure t
  :mode "\\.\\(ejs\\|erb\\|ecr\\|html\\)\\'"
  :config
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package which-key
  :ensure t
  :delight which-key-mode
  :init
  (which-key-mode))

(use-package yaml-mode)

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :bind (("<f12>" . yas-expand))
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

(load "~/.emacs.d/functions.el")
(load "~/.emacs.d/fish-colors.el")

;;;;;;;;;;;
;; Modes ;;
;;;;;;;;;;;
;; Any external packages should be in the previous section.
;; This is just for built-in modes.

(global-hl-line-mode)
(tool-bar-mode -1)
(global-subword-mode)
(column-number-mode)
(recentf-mode)

(add-hook 'prog-mode-hook 'highlight-indentation-mode)
;; (add-hook 'prog-mode-hook 'auto-save-mode)

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

(add-hook 'org-mode-hook
          #'(lambda ()
              ;; (refill-mode 1)
              (toggle-truncate-lines -1)
              (toggle-word-wrap 1)))

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(add-hook 'git-commit-setup-hook
          'fix-commit-editmsg
          'insert-branch-name)

(let ((disable-line-numbers
       '(lambda ()
          (display-line-numbers-mode -1))))
  (add-hook 'git-commit-setup-hook disable-line-numbers)
  (add-hook 'magit-mode-hook disable-line-numbers)
  (add-hook 'term-mode-hook disable-line-numbers))

(delight '((subword-mode "" "subword")))

;;;;;;;;;;;;;;;;;
;; Keybindings ;;
;;;;;;;;;;;;;;;;;

(windmove-default-keybindings)

(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

(global-set-key (kbd "C-o") 'insert-line-before-and-indent)

(global-set-key (kbd "M-;") 'comment-line)
(global-set-key (kbd "C-M-;") 'copy-and-comment-line)

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x C-g") 'keyboard-quit)

(global-set-key
 (kbd "C-v")
 #'(lambda ()
     (interactive)
     (scroll-up-command 16)))

(global-set-key
 (kbd "M-v")
 #'(lambda ()
     (interactive)
     (scroll-up-command -16)))

;; Suck it hard, apple
(global-set-key (kbd "<home>") 'beginning-of-line-or-indentation)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-x C-c") 'ask-before-closing)



;;;;;;;;;;;;;;;;;;;
;; Misc settings ;;
;;;;;;;;;;;;;;;;;;;

(set-fill-column 90)

(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-screen t)
;; (add-to-list 'default-frame-alist '(font . "Roboto Mono 10"))
;; (add-to-list 'default-frame-alist '(font . "Noto Sans Mono 8"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
