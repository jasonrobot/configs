;;; package --- init.el
;;; Commentary:

;; TODO:
;; set truncate lines and word wrap in org mode

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
        ("mepla" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package better-defaults
  :ensure t)

(use-package delight
  :ensure t)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)))

(defun -strip-newlines-from-json (args) ;(output checker buffer)
  "Remove newlines from the first element of ARGS."
  (setcar args (replace-regexp-in-string "\n" "" (car args)))
  args)

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
              #'-strip-newlines-from-json))

(use-package anzu
  :delight anzu-mode
  :config (global-anzu-mode))

;; (use-package cider
;;   :config
;;   (setq clojure-indent-style :always-indent))

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
            (context 2)))

(use-package company
  :ensure t
  :delight company-mode
  :config (setq company-idle-delay nil)
  :bind (("M-SPC" . company-complete)))

(use-package crystal-mode
  :bind (("C-c C-f" . crystal-tool-format)))

(use-package emmet-mode
  :hook web-mode)

(use-package helm-company
  :bind (("M-S-SPC" . helm-company)))

(defun -fix-diff-range (range)
  "Change the diff RANGE from using three dots (useless) to two (normal)."
  (when (stringp range)
    (s-replace "..." ".." range)))

(use-package highlight-indentation
  :delight highlight-indentation-mode)

(use-package magit
  :config
  (advice-add 'magit-diff--dwim
              :filter-return
              #'-fix-diff-range))

(use-package projectile
  :delight
  '(:eval (let ((mode-string " プロジ"))
            (if (projectile-project-p)
                (concat mode-string
                        (concat "《" (projectile-project-name) ":" (symbol-name (projectile-project-type)) "》"))
              mode-string)))
  :config
  (unbind-key "C-c p l" projectile-mode-map)
  (unbind-key "C-c p f" projectile-mode-map))

(use-package helm-projectile
  :bind (("C-c p s" . helm-projectile-rg)))

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :delight "JS²"
  :bind (("C-." . js2-next-error))
  :config
  (setq js2-global-externs
        '("ontraport"
          "setTimeout" "setInterval" "clearTimeout" "clearInterval"
          "describe" "it" "beforeEach" "afterEach" "beforeAll" "afterAll" "expect" "jasmine"
          "Globalize"
          "test_runner" "steal" "$" "$l" "_" "go" "ObjectAnimate")))

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

;; hyper-useful
(use-package s)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package web-mode
  :mode "\\.\\(jsx\\|html?\\|ejs\\|ecr\\|erb\\)\\'")

(use-package yasnippet
  :init (yas-global-mode 1)
  :bind (("C-c tab" . yas-next-field-or-maybe-expand)))

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

(defun copy-file-path ()
  "Copy the current file's path to the clipboard."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "No file name for current buffer")
      (kill-new filename))))

(defun copy-file-name ()
  "Copy the current file's name to the clipboard."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not filename)
        (message "No file name for current buffer")
      (kill-new (file-name-nondirectory filename)))))

(defun fix-commit-editmsg ()
  "Add an empty line as line 2, move cursor in to position for typing."
  (interactive)
  (move-end-of-line nil)
  (newline)
  (move-end-of-line 0))

(defun wrap-in-$l ()
  "Wrap a string in $l()."
  (interactive)
  (save-excursion
    (search-forward "'")
    (backward-char)
    (insert "$l( ")
    (forward-sexp)
    (insert " )")))

;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint
;;           (and root
;;                (expand-file-name "node_modules/.bin/eslint"
;;                                  root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))



;; A macro/function to find the next function definition in a JS file.
(fset 'js-next-function
      [?\C-s return ?: ?  ?? ?f ?u ?n ?c ?t ?i ?o ?n ?  ?? ?\( return])

;; would be better to use a regex for this
;; var let or const, space, word, colon or (equals with maybe spaces around), function or =>
;; '(var|let|const)\s\w+(:\s*|\s*=\s*)(function|.*=>)

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

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

(add-hook 'org-mode-hook
          '(lambda ()
             (toggle-truncate-lines -1)
             (toggle-word-wrap 1)
             ;; (refill-mode 1)
             ))

(add-hook 'prog-mode-hook 'highlight-indentation-mode)
;; (add-hook 'prog-mode-hook 'auto-save-mode)

(add-hook 'before-save-hook
          'delete-trailing-whitespace)

(add-hook 'git-commit-setup-hook
          'fix-commit-editmsg)

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

(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x C-g") 'keyboard-quit)

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
     (define-key js-mode-map (kbd "C-c f p") 'js-prev-function)
     (define-key js-mode-map (kbd "C-c l") 'wrap-in-$l)))

;;;;;;;;;;;;;;;;;;;
;; Misc settings ;;
;;;;;;;;;;;;;;;;;;;

(set-fill-column 80)

(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(font . "Roboto Mono 9"))
;; (add-to-list 'default-frame-alist '(font . "Noto Sans Mono 8"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(provide 'init)
;;; init.el ends here
