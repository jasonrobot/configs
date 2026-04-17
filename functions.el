;;; functions --- useful functions
;;; Commentary:
;;; Code:
(require 's)

(defun my-insert-uuid ()
  "Insert a UUID."
  (interactive)
  (let ((uuid (shell-command-to-string "uuidgen")))
    (insert (s-trim uuid))))

(defun my-url-unhex ()
  "Un-hex a url, as an interactive function."
  (interactive)
  (insert "\n")
  (insert (url-unhex-string (buffer-substring-no-properties (mark) (point)))))

(defun my-url-encode ()
  "url-hexify-string as an interactive function."
  (interactive)
  (insert "\n")
  (insert (url-hexify-string (buffer-substring-no-properties (mark) (point)))))

(defun beginning-of-line-or-indentation ()
  "Move to beginning of line, or indentation."
  (interactive)
  (let ((start (point)))
    (back-to-indentation)
    ;; now if point is the same as when we started, we're already at indent start
    (if (= start (point))
        (beginning-of-line))))

(defun copy-and-comment-line ()
  "Duplicate the line at point, and comment out the first copy."
  (interactive)
  (kill-whole-line)
  (yank)
  (yank)
  (forward-line -2)
  (comment-line 1))

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
      ;; move to the line with the branch name
      (re-search-forward "On branch")
      (let ((branch-line (thing-at-point 'line))
            (branch-name-regexp "On branch \\(\\w*/\\)?\\(\\w+-?[0-9]*\\)"))
        (string-match branch-name-regexp branch-line)
        (let ((ticket-name (match-string 2 branch-line)))
          (when ticket-name
            (goto-char 0)
            (insert "[" ticket-name "]")))))
    (end-of-line)
    (insert " ")))

(defun my-fix-diff-range (range)
  "Change the diff RANGE from using three dots (useless) to two (normal)."
  (when (stringp range)
    (s-replace "..." ".." range)))

(defun my-always-make-upstream-origin (branch start-point)
  "None of your shenanigans, Magit! I ALWAYS want BRANCH upstream to be origin!

This is used to override \"magit-branch-maybe-adjust-upstream\", and it just
always sets branch.NAME.remote to origin. START-POINT is ignored."
    (magit-call-git "branch" "--set-upstream-to=origin" branch))

(defun setup-tide-mode ()
  "Used as a hook for typescript mode to enable tide mode with all the nice extras."
  (interactive)
  (message "setting up tide mode")
  (tide-setup)
  (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(defun ask-before-closing ()
  "Get confirmation before closing Emacs."
  (interactive)
  (if (yes-or-no-p (format "Are you sure you want to close this frame? "))
      (save-buffers-kill-terminal)
    (message "Canceled frame close")))

(defun my-scratch-today ()
  "Make a break and insert the date."
  (interactive)
  (let ((date-string (s-trim (shell-command-to-string "date +%Y-%m-%d")))
        (separator-string "================================================================"))
    (insert date-string " " separator-string)
    (newline)))

(fset 'my-insert-doc-comment
  (kmacro-lambda-form [tab ?/ ?* ?* return ?* ?  tab return tab ?* ?/ ?\C-p] 0 "%d"))

(fset 'wrap-async
      (kmacro-lambda-form [?w ?a ?i ?t ?F ?o ?r ?A ?s ?y ?n ?c ?\( ?a ?s ?y ?n ?c ?  ? ? ?\) ?] 0 "%d"))

(defun my-objectify ()
  "Turn a class style definition into an object one."
  (interactive)
  (let* ((start (region-beginning))
         (end (region-end)))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char start)
        (re-search-forward (rx (group-n 1 (or "let" "var" "const"))
                               (1+ space)
                               ;; this is the variable name
                               (group-n 2 (1+ word))
                               ;; could potentially have a type declaration
                               (? (? space) ":" (? space) (1+ word))
                               (* space)
                               "="
                               (* space)
                               "new"
                               (1+ space)
                               ;; this is the constructor invocation, with our typename
                               (group-n 3 (1+ word)) "()" (? ";"))
                           end
                           t)
        (let ((variable-declaration (match-string 1))
              (variable-name (match-string 2))
              (type-name (match-string 3))
              (body-start (save-excursion
                            (goto-char 0)
                            (line-beginning-position 2))))
          ;; start on line 1, fix the declaration
          (goto-char 0)
          (kill-line)
          (insert (format "%s %s: %s = {" variable-declaration variable-name type-name))

          ;; start on the body, fix the assignment
          (replace-regexp-in-region
           (rx-to-string
            `(seq
              (* space)
              ,variable-name "."
              ;; property name
              (group-n 1 (1+ (or word "_")))
              (* space)
              "="
              (* space)
              (group-n 2 (* nonl))
              ";"))
           "\\1: \\2,"
           body-start
           )

          (goto-char (point-max))
          (insert "}")
          (newline)
          (indent-region 0 (point-max))
          )))))


(defun my-save-ediff-last-windows ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-ediff-last-windows ()
  (set-window-configuration my-ediff-last-windows))

(defun my-set-font-size ()
  "Prompt user for a font size to set for this frame."
  (interactive)
  (let ((size (read-number "Font size:")))
    (set-frame-font (format "-*-JetBrains Mono-regular-normal-normal-*-%d-*-*-*-m-0-iso10646-1" size))))

;;; Angular component navigation helper functions

(defun my-angular-open-other (suffix)
  "Open another file for this angular component ending in SUFFIX."
  (let ((filename (buffer-file-name)))
    (save-match-data
      (let ((component-regexp (rx (group (1+ (in alnum "-")))
                                  "."
                                  (group (or "component" "service"))
                                  "."
                                  (group (1+ word)))))
        (if (string-match component-regexp filename)
            (let ((component-name (match-string 1 filename))
                  (component-or-service (match-string 2 filename))
                  (component-suffix (match-string 3 filename)))
              (if (and (string-equal component-or-service "service")
                       (or (string-equal suffix "html") (string-equal suffix "scss")))
                  (message (format "Cannot open for %s service" suffix))
                (find-file (concat component-name "." component-or-service "." suffix))))
          (message "Not an angular component"))))))

(defun my-angular-open-template ()
  "Open the html template for the current component."
  (interactive)
  (my-angular-open-other "html"))

(defun my-angular-open-stylesheet ()
  "Open the stylesheet for the current component."
  (interactive)
  (my-angular-open-other "scss"))

(defun my-angular-open-component ()
  "Open the component code for the current component."
  (interactive)
  (my-angular-open-other "ts"))

(defun my-angular-open-spec ()
  "Open the spec/test for the current component."
  (interactive)
  (my-angular-open-other "spec.ts"))

(defvar my-gc-daily-timer)

;; (defun my-start-gc-daily ()
;;   "Start a timer to collect garbage once a day."
;;   (setq my-gc-daily-timer (run-at-time t (* 60 60 24) #'(lambda () (garbage-collect)))))

;; (defun my-stop-gc-daily ()
;;   (cancel-timer my-gc-daily-timer))

;; ^.+m?js.+
(defun my-clean-stacktrace ()
  "Trim a stacktrace down to just our frames."
  (interactive)

  (let* ((start (region-beginning))
         (end (region-end)))
    (replace-regexp-in-region
     (rx line-start (1+ nonl) (? "m") "js" (1+ nonl) "
")
     ""
     start
     end)))

(provide 'functions)
;;; functions.el ends here
