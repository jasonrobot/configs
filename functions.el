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
  (let ((start (region-beginning))
        (end (region-end)))
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char start)
        (re-search-forward "\\(let\\|var\\|const\\) \\([[:word:]]+\\).*new \\([[:word:]]+\\)()" end t)
        (let ((variable-name (match-string 2)))

          ;; operate on the middle, properties being set
          (let ((body-start (save-excursion
                              (goto-char 0)
                              (+ 1 (line-end-position))))
                (body-end (save-excursion
                            (goto-char (1- (point-max)))
                            (1- (line-beginning-position)))))
            (save-restriction
              (narrow-to-region body-start body-end)
              (message "name is: %s" variable-name)
              (replace-string-in-region (format "%s." variable-name) "")
              (replace-string-in-region " =" ":")
              (replace-string-in-region ";" ",")))

          ;; operate on the first and last lines to return the object
          (goto-char start)
          (beginning-of-line)
          (kill-line)
          (insert "return {")
          (goto-char end)
          (beginning-of-line)
          (kill-line)
          (insert "};")

          (indent-region start end))))))

(provide 'functions)
;;; functions.el ends here
