;;; package --- init.el
;;; Commentary:

;;; Code:

;; do this, since I usually run fish
(setq shell-file-name "/bin/bash")

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(load "~/.emacs.d/helm-magit-recent-branches.el")

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

(use-package avy
  :bind (("C-:" . 'avy-goto-char)))

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
  (cons (replace-regexp-in-string "\n" "" (car args))
        (cdr args)))

(defun -strip-warning-message-from-tslint (args) ;(output checker buffer)
  "Remove a tslint error from ARGS that causes them to be invalid JSON."
  (cons (replace-regexp-in-string "=\\{13\\}\n\\(.*\n\\)*=\\{13\\}\n"
                                  ""
                                  (car args))
        (cdr args)))

(use-package flycheck
  :ensure t
  :bind
  (("M-g n" . 'flycheck-next-error)
   ("M-g p" . 'flycheck-previous-error))
  :init
  (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (advice-add 'flycheck-parse-eslint
              :filter-args
              (lambda (args)
                (-strip-warning-message-from-tslint
                 (-strip-newlines-from-json args)))))

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

(use-package helm
  :ensure t)

(use-package helm-company
  :bind (("M-S-SPC" . helm-company)))

(use-package helm-ls-git
  :ensure t
  :after (helm projectile) ;after projectile, so these key bindings are last
  :bind (("C-c p f" . helm-ls-git-ls)))

(use-package helm-projectile
  :after (projectile) ; Must be set up after projectile
  :bind (("C-c p s" . helm-projectile-rg)))

(use-package highlight-indentation
  :delight highlight-indentation-mode)

(use-package js2-mode
  :ensure t
  :mode "\\.m?js\\'"
  :delight "JS²"
  :bind (("C-." . js2-next-error))
  :config
  (setq js2-global-externs
        '("ontraport"
          "setTimeout" "setInterval" "clearTimeout" "clearInterval"
          "describe" "it" "beforeEach" "afterEach" "beforeAll" "afterAll" "expect" "jasmine"
          "Globalize"
          "test_runner" "steal" "$" "$l" "_" "go" "ObjectAnimate")))

(use-package magit
  :bind (("C-c b" . helm-magit-recent-branches))
  :config
  (advice-add 'magit-diff--dwim
              :filter-return
              #'my-fix-diff-range)
  (advice-add 'magit-branch-maybe-adjust-upstream
              :override
              #'my-always-make-upstream-origin))

(use-package projectile
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

;; hyper-useful string library
(use-package s)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  ;; (before-save . tide-format-before-save)))
  :config
  (advice-add 'tide-format
            :override
            (lambda () nil))) ;TODO use eslint to format this.

(use-package typescript-mode
  :bind (("M-j" . js2-line-break)))

(use-package web-mode
  :mode "\\.\\(ejs\\|erb\\|ecr\\|html\\)\\'"
  :config
  (add-hook 'web-mode-hook 'emmet-mode))

(use-package which-key
  :delight which-key-mode)

(use-package yasnippet
  :init (yas-global-mode 1)
  :bind (("<f12>" . yas-expand))
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))

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

(defun wrap-in-$l ()
  "Wrap a string in $l()."
  (interactive)
  (save-excursion
    (search-forward "'")
    (backward-char)
    (insert "$l( ")
    (forward-sexp)
    (insert " )")))

(defun my-fix-diff-range (range)
  "Change the diff RANGE from using three dots (useless) to two (normal)."
  (when (stringp range)
    (s-replace "..." ".." range)))

(defun my-always-make-upstream-origin (branch start-point)
  "None of your shenanigans, Magit! I ALWAYS want BRANCH upstream to be origin!

This is used to override \"magit-branch-maybe-adjust-upstream\", and it just
always sets branch.NAME.remote to origin. START-POINT is ignored."
    (magit-call-git "branch" "--set-upstream-to=origin" branch))

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

(global-hl-line-mode)
(tool-bar-mode -1)
(global-subword-mode)
(column-number-mode)
(recentf-mode)

(add-hook 'prog-mode-hook 'highlight-indentation-mode)
;; (add-hook 'prog-mode-hook 'auto-save-mode)

(add-hook 'emacs-lisp-mode-hook 'flymake-mode)

(add-hook 'org-mode-hook
          '(lambda ()
             ;; (refill-mode 1)
             (toggle-truncate-lines -1)
             (toggle-word-wrap 1)))

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

(set-fill-column 120)

(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(font . "Roboto Mono 9"))
;; (add-to-list 'default-frame-alist '(font . "Noto Sans Mono 8"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(put 'upcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here
