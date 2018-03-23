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
  :init (global-flycheck-mode)
  :bind (("C-." . next-error)
         ("C-," . previous-error)))

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
  :mode "\\.js\\'"
  :bind (("C-." . js2-next-error)))

(use-package elpy
  :ensure t
  :config
  (setq elpy-rpc-python-command "python3")
  :init (elpy-enable))

;; (use-package web-mode
;;   :mode "\\.tsx\\'"
;;   :hook )

(use-package typescript-mode
  :mode "\\.tsx\\'")

(use-package tide
  :config
  (defun setup-tide-mode ()
    "Set up tide mode in the current buffer.  Have hooks call this."
    (tide-setup)
    ;; (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    ;; (eldoc-mode +1)
    (tide-hl-identifier-mode +1))
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    ;; (company-mode +1))
  ;; (add-hook 'web-mode-hook
  ;;           (lambda ()
  ;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
  ;;               (setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

;; (use-package highlight-indentation
;;   :ensure t
;;   :hook prog-mode)

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
        (kill-region begin (point))))))

;;;;;;;;;;;;;;;;;;;
;; Misc settings ;;
;;;;;;;;;;;;;;;;;;;

(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(font . "Ubuntu Mono 8"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-set-variables - Dont edit below here! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(beacon-color "#F8BBD0")
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "bc4b650c41b16b98166b35da94b366c6a9e1e7883bbf4937c897fb7bd05aa619" "bea5fd3610ed135e6ecc35bf8a9c27277d50336455dbdd2969809f7d7c1f7d79" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)))
 '(evil-insert-state-cursor (quote ("#D50000" bar)))
 '(evil-normal-state-cursor (quote ("#F57F17" box)))
 '(evil-visual-state-cursor (quote ("#66BB6A" box)))
 '(fci-rule-color "#073642")
 '(global-company-mode t)
 '(global-subword-mode t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (web-mode tide xkcd yasnippet molokai-theme ample-theme apropospriate-theme anti-zenburn-theme labburn-theme zenburn-theme elpy magit fish-completion fish-mode company-lsp lsp-go racket-mode web-beautify better-defaults centered-cursor-mode cider dockerfile-mode expand-region ggtags haskell-mode helm helm-ls-git helm-ls-hg highlight-indentation js2-mode lsp-mode lsp-rust markdown-mode company slime yaml-mode use-package solarized-theme slime-company rust-mode lua-mode flycheck)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face) t)
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024 t)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80 t)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25 t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(tab-width 4)
 '(tabbar-background-color "#ffffff")
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
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
 '(helm-ls-git-untracked-face ((t (:foreground "firebrick3")))))
