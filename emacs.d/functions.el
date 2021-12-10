;;; functions - useful functions
;;; Commentary:
;;; Code

(defun my-insert-uuid ()
  "Insert a UUID."
  (interactive)
  (let ((uuid (shell-command-to-string "uuidgen")))
    (insert (s-trim uuid))))

(provide 'functions)
;;; functions.el ends here