;;; functions --- useful functions
;;; Commentary:
;;; Code:

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


(provide 'functions)
;;; functions.el ends here