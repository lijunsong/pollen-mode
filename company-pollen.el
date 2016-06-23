;;; company-pollen.el --- company-mode completion backend for pollen

;; Copyright (C) 2016 Junsong Li

;;; Commentary:

;; TODO:
;; - Generate annotation.
;; - Remove pollen.rkt dependencies in racket code.  (require instead
;;   some .pm file in the same directory as the opened pollen file)

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
  "Return all user defined identifiers in `pollen.rkt'.

Note: ID is a pair (identifier . FROM-MODULE)."
  (if (file-exists-p "pollen.rkt")
      (shell-command-to-string
       (concat "racket -e '" pollen-fetch-id-code "'"))
    '()))

(defvar pollen-id-caches nil
  "Cache for pollen identifiers.")

(defun pollen-tag-completions ()
  "Return a list of avaiable pollen tags."
  (if (null pollen-id-caches)
      (setq pollen-id-caches (read (pollen-all-ids))))
  pollen-id-caches)

(defun pollen-find-tag-fuzzy-match (prefix candidate)
  "Fuzzy match PREFIX with CANDIDATE."
  ;; TODO: improve accuracy.
  (cl-subsetp (string-to-list prefix)
              (string-to-list candidate)))

(defun pollen-find-tag-info (prefix)
  "Given a PREFIX, return tag info."
  (message "called")
  (remove-if-not
   (lambda (info)
     (message "fuzz match prefix %S with info %S" prefix info)
     (pollen-find-tag-fuzzy-match prefix (car info)))
   (pollen-tag-completions)))

(defun company-pollen-backend (command &optional arg &rest ignored)
  "The main function for backend."
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-pollen-backend))
    (prefix (and (eq major-mode 'pollen-mode)
                 (company-grab-symbol)))
    (candidates
     (message "candidate %S" (pollen-find-tag-info arg))
     (mapcar 'car (pollen-find-tag-info arg)))))

(add-to-list 'company-backends 'company-pollen-backend)

(provide 'company-pollen)

;;; company-pollen.el ends here
