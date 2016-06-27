;;; company-pollen.el --- company-mode completion backend for pollen

;; Copyright (C) 2016 Junsong Li

;;; Commentary:

;; TODO:
;; - Generate annotation.
;; - Remove pollen.rkt dependencies in racket code.  (require instead
;;   some .pm file in the same directory as the opened pollen file)
;; - Error handling

;;; Code:

(require 'company)
(require 'cl-lib)

(defconst pollen-fetch-id-code
  (concat
   "(require \"pollen.rkt\")"
   "(define pollen-file \"pollen.rkt\")"
   "(define-values (l1 l2) (module->exports pollen-file))"
   "(define (get-module-path mod-idx)
  (define-values (path sub) (module-path-index-split mod-idx))
  path)"
   "(define (id-info id)
  (let ((name (first id))
        (idx (and (not (empty? (second id)))
                  (first (second id)))))
    (cons (symbol->string name)
          (if (module-path-index? idx)
              (get-module-path idx)
              pollen-file))))"
   "(printf \"~S\" (append (map id-info (rest (first l1)))
                           (map id-info (rest (first l2)))))"))


(defun pollen-all-ids ()
  "Return all exported user defined identifiers in `pollen.rkt'.

Return nil if identifiers not available (e.g. pollen.rkt has errors)

Note: ID is a pair (identifier . FROM-MODULE)."
  ;; TODO error handling here.
  (when (file-exists-p "pollen.rkt")
    (let ((ids-str (shell-command-to-string
                    (concat "racket -e '" pollen-fetch-id-code "'"))))
      (read ids-str))))

(defvar-local pollen-id-caches nil
  "Cache for pollen identifiers.")

(defvar-local pollen-id-cache-initialized nil
  "Non-nil if `pollen-id-caches' has been initialized.")


(defun pollen-tag-completions ()
  "Return a list of avaiable pollen tags."
  (when (null pollen-id-cache-initialized)
    (message "Initialize company pollen backend ...")
    (setq pollen-id-cache-initialized t)
    (let ((ids (pollen-all-ids)))
      (setq pollen-id-caches ids))
    (message "Done."))
  pollen-id-caches)

(defun pollen-find-tag-fuzzy-match (prefix candidate)
  "Fuzzy match PREFIX with CANDIDATE."
  ;; TODO: improve accuracy.
  (cl-subsetp (string-to-list prefix)
              (string-to-list candidate)))

(defun pollen-find-tag-info (prefix)
  "Given a PREFIX, return tag info."
  (remove-if-not
   (lambda (info)
     (pollen-find-tag-fuzzy-match prefix (car info)))
   (pollen-tag-completions)))

(defun company-grab-pollen-tag ()
  "Return tag name if point is on a pollen tag, nil otherwise.

Note: this function uses one function from `pollen-mode'."
  (let ((tag (pollen-tag-at-point t)))
    (when tag
      (substring tag 1))))

(defun company-pollen-backend (command &optional arg &rest ignored)
  "The main function for backend.

If pollen identifiers not available, let other backends take over."
  (interactive (list 'interactive))
  (pollen-tag-completions) ; initialize tag list
  (case command
    (interactive (company-begin-backend 'company-pollen-backend))
    (prefix (and (eq major-mode 'pollen-mode)
                 (and pollen-id-cache-initialized pollen-id-caches)
                 (company-grab-pollen-tag)))
    (candidates
     (mapcar 'car (pollen-find-tag-info arg)))))

(add-to-list 'company-backends 'company-pollen-backend)

(provide 'company-pollen)

;;; company-pollen.el ends here
