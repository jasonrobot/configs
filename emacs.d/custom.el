;;; package --- Summary

;; its not a package, its just all my customs

;;; Commentary:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-set-variables - Dont edit below here! ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("a4df5d4a4c343b2712a8ed16bc1488807cd71b25e3108e648d4a26b02bc990b3" "62c81ae32320ceff5228edceeaa6895c029cc8f43c8c98a023f91b5b339d633f" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "c856158cc996d52e2f48190b02f6b6f26b7a9abd5fea0c6ffca6740a1003b333" "42b9d85321f5a152a6aef0cc8173e701f572175d6711361955ecfb4943fe93af" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "bc4b650c41b16b98166b35da94b366c6a9e1e7883bbf4937c897fb7bd05aa619" "bea5fd3610ed135e6ecc35bf8a9c27277d50336455dbdd2969809f7d7c1f7d79" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(delete-trailing-lines nil)
 '(display-line-numbers t)
 '(global-anzu-mode t)
 '(global-company-mode t)
 '(global-subword-mode t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.hi" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.wp.js" "*.wp.css" "*.sql" "*.dat" "*.min.js" "*.csv")))
 '(highlight-indent-guides-auto-enabled nil)
 '(magit-diff-use-overlays nil)
 '(mode-require-final-newline nil)
 '(package-selected-packages
   (quote
    (slack php-mode helm-rg eglot helm-projectile lsp-mode magit racket-mode solarized-theme web-mode clojure-mode company company-lsp fish-completion flycheck gruvbox-theme haskell-mode helm helm-core helm-ls-git js2-mode lsp-python markdown-mode projectile pyvenv rust-mode slime tide typescript-mode use-package with-editor zenburn-theme flycheck-crystal crystal-mode diminish anzu lsp-hack lsp-haskell toml-mode dracula-theme flycheck-rust rust-playground json-mode helm-company xkcd molokai-theme ample-theme apropospriate-theme anti-zenburn-theme labburn-theme fish-mode lsp-go web-beautify better-defaults centered-cursor-mode dockerfile-mode expand-region ggtags helm-ls-hg highlight-indentation lsp-rust yaml-mode slime-company lua-mode)))
 '(projectile-mode t nil (projectile))
 '(require-final-newline nil)
 '(scroll-margin 2)
 '(tab-width 4)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil))
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(beacon-color "#F8BBD0")
 '(c-basic-offset 4)
 '(c-default-style
   (quote
    ((c-mode . "bsd")
     (c++-mode . "bsd")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(column-number-mode t)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-search-regexp-function (quote company-search-flex-regexp))
 '(company-show-numbers t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes
   (quote
    ("ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "bc4b650c41b16b98166b35da94b366c6a9e1e7883bbf4937c897fb7bd05aa619" "bea5fd3610ed135e6ecc35bf8a9c27277d50336455dbdd2969809f7d7c1f7d79" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(delete-trailing-lines nil)
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(display-line-numbers t)
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(erc-nick "beizhia")
 '(evil-emacs-state-cursor (quote ("#D50000" hbar)))
 '(evil-insert-state-cursor (quote ("#D50000" bar)))
 '(evil-normal-state-cursor (quote ("#F57F17" box)))
 '(evil-visual-state-cursor (quote ("#66BB6A" box)))
 '(fci-rule-color "#eee8d5")
 '(git-commit-summary-max-length 120)
 '(global-company-mode t)
 '(global-subword-mode t)
 '(gnus-logo-colors (quote ("#528d8d" "#c0c0c0")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1fb3b3\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(org-startup-truncated t)
 '(package-selected-packages
   (quote
    (helm-projectile helm anzu helm-rg package-build shut-up epl git commander f dash s crystal-mode haxe-mode diminish eglot helm-tramp rust-playground toml-mode cargo company-anaconda alect-themes anaconda-mode arjen-grey-theme clojure-mode flycheck-clojure flycheck-rust helm-cider helm-company hy-mode cask cask-mode json-mode web-mode tide xkcd yasnippet molokai-theme ample-theme apropospriate-theme anti-zenburn-theme labburn-theme zenburn-theme elpy magit fish-completion fish-mode company-lsp lsp-go racket-mode web-beautify better-defaults centered-cursor-mode cider dockerfile-mode expand-region ggtags haskell-mode helm-ls-git helm-ls-hg highlight-indentation js2-mode lsp-mode lsp-rust markdown-mode company slime yaml-mode use-package solarized-theme slime-company rust-mode lua-mode flycheck)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face) t)
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024 t)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80 t)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25 t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(tab-width 4)
 '(tabbar-background-color "#ffffff")
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c8805d801780")
     (60 . "#bec073400bc0")
     (80 . "#b58900")
     (100 . "#a5008e550000")
     (120 . "#9d0091000000")
     (140 . "#950093aa0000")
     (160 . "#8d0096550000")
     (180 . "#859900")
     (200 . "#66aa9baa32aa")
     (220 . "#57809d004c00")
     (240 . "#48559e556555")
     (260 . "#392a9faa7eaa")
     (280 . "#2aa198")
     (300 . "#28669833af33")
     (320 . "#279993ccbacc")
     (340 . "#26cc8f66c666")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
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
