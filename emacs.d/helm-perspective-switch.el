;;; helm-perspective-switch --- Summary -*- lexical-binding: t; -*-
;; Use helm to switch between perspectives

;;; Commentary:
;; perspective.el has some builtin support for ido, but I use helm, so I want a switcher
;; that works with that.

;;; Code:

(defvar helm-source-perspective-not-found
  (helm-build-dummy-source
   "Create perspective"
   :action (helm-make-actions
            "Create perspective"
            #'persp-switch)))

(defun helm-perspective-swtich ()
  "Get perspectives and switch between them."
  (interactive)
  (let* ((helm-candidates (persp-all-names))
         (helm-source-perspective-list (helm-build-sync-source "perspectives"
                                         :candidates helm-candidates
                                         :fuzzy-match nil))
         (helm-selection (helm :sources '(helm-source-perspective-list
                                          helm-source-perspective-not-found)
                               :buffer "*perspectives*")))
    (when helm-selection
      (persp-switch helm-selection))))


(provide 'helm-perspective-switch)
;;; helm-perspective-switch.el ends here
