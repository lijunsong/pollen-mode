;;; pollen-mode.el --- major mode for editing pollen files
;;
;; Copyright (C) 2016 Junsong Li
;; Author: Junsong Li <ljs.darkfish AT GMAIL>
;; Maintainer: Junsong Li
;; Created: 11 June 2016
;; Keywords: languages, pollen, pollenpub
;; License: LGPL
;; Distribution: This file is not part of Emacs
;;
;;; Commentary:
;; This file provides editing assistant for pollen, the digital-publishing
;; tools
;;
;; Glossary:
;; - Command Char: also referred to as the lozenge
;; - Top Level: A position NOT inside braces
;; - Pollen Tag: command char followed by racket id or char `;'
;;
;; TODO:
;; - provide a brace matcher
;; - support comment-dwim
;;; Code:

(defvar pollen-command-char "◊")
(defvar pollen-command-char-code ?\u25CA)
(defvar pollen-command-char-target "@")

(defvar pollen-racket-id-reg "[a-zA-Z][a-zA-Z0-9-]*")
(defvar pollen-header-reg "^#lang .*$")

(defun pollen-gen-highlights (command-char)
  "Generate highlight given the pollen COMMAND-CHAR."
  (let ((id (concat command-char pollen-racket-id-reg))
        (malform1 (concat command-char "[ \\n]+"))
        (malform2 (concat command-char "{")))
    `((,id . font-lock-variable-name-face)
      (,pollen-header-reg . font-lock-comment-face)
      (,malform1 . font-lock-warning-face)
      (,malform2 . font-lock-warning-face))))

(defvar pollen-highlights
  (pollen-gen-highlights pollen-command-char)
  "Font lock in pollen.")

;;; Pollen model
(defun pollen--make-tag (name lb rb)
  "Create tag object.

A tag object has NAME (command char excluded), left brace LB and
right brace RB pos.

Note: for |{, LB points to |."
  (list name lb rb))

(defun pollen--tag-name (tag)
  "Get the name of a TAG."
  (car tag))

(defun pollen--tag-lbraces (tag)
  "Get left brace pos of a TAG."
  (cadr tag))

(defun pollen--tag-rbraces (tag)
  "Get right brace pos of a TAG."
  (caddr tag))

(defun pollen--matched-right-brace-pos (pos)
  "Get to the right brace matched with a left brace at position POS.

Return the position of the right brace,  nil if the given pos is
not a left brace or there is no matched one.

Note: this function jumps over the \"|\" of \"|{\"."
  (let (p
        ;; since we're sometimes treating left braces as comment
        ;; delimiters, it is important froward-sexp not obey syntax
        ;; table
        (parse-sexp-lookup-properties nil))
    (save-excursion
      (goto-char pos)
      (if (looking-at "|")
          (forward-char))
      (when (char-equal (char-after) ?\{)
        (ignore-errors
          (forward-sexp 1)
          (backward-char 1)
          (setq p (point)))))
    p))

;; special case for things at point
(put 'pollen--tag 'bounds-of-thing-at-point 'pollen--bounds-of-tag-at-point)
(defun pollen--bounds-of-tag-at-point ()
  "Move point to the beginning of the current tag.

Note: This function returns nil if the point is not on a tag name
starting with command char."
  (let* ((forward-allowed "A-Za-z0-9-*=:$")
         (backward-allowed (concat
                            pollen-command-char
                            forward-allowed ";|"))
         (beg  (save-excursion
                 (skip-chars-backward backward-allowed)
                 (if (looking-at pollen-command-char)
                     (point)
                   nil)))
         (end (save-excursion
                (skip-chars-forward forward-allowed)
                (if (and beg (eq (- (point) beg) 1) (looking-at ";"))
                    (1+ (point))
                  (point)))))
    (if (and beg end)
        (cons beg end)
      nil)))

(defun pollen-tag-at-point (&optional no-properties)
  "Return the tag at point, or nil if none is found.
NO-PROPERTIES will be passed to `thing-at-point'."
  (thing-at-point 'pollen--tag no-properties))

(defun pollen--get-current-tagobj ()
  "Get a tag object under the cursor.

It returns nil when the cursor is on the toplevel.  It returns
the tag object of the enclosing tag otherwise.

Implementation Caveat: This function jumps to comment start using
parsing state from `syntax-ppss'.  Use it with causion in
`syntax-propertize-function', because when the position is right
after a comment start character, the parsing state may not mark
it a comment start yet, in this case the tagobj found is a tag
prior to current tag."
  (let* ((tag (pollen-tag-at-point t))
         (parse-sexp-lookup-properties nil)
         (make-tag
          #'(lambda ()
              (let ((tag (pollen-tag-at-point t)))
                (when tag
                  ;; make sure bounds is not nil
                  (let ((bounds (bounds-of-thing-at-point 'pollen--tag))
                        (name (substring tag 1)))
                    (unless (string-empty-p name)
                      (let ((lb-pos (if (char-equal (char-after (cdr bounds)) ?\|)
                                        (1+ (cdr bounds))
                                      (cdr bounds))))
                        (pollen--make-tag
                         name lb-pos
                         (pollen--matched-right-brace-pos lb-pos))))))))))
    (cond (tag
           ;; in the front or the middle of a tag name
           (funcall make-tag))
          (t
           ;; search backward
           (save-excursion
             (let (result
                   (ttt 0))
               (while (and (null result)
                           (progn
                             (skip-chars-backward  "^{}")
                             (not (= (point) (point-min))))
                           (< ttt 1000))
                 (setq ttt (1+ ttt))
                 (cond ((char-equal ?\} (char-before))
                        (backward-sexp 1))
                       ((char-equal ?\{ (char-before))
                        (backward-char 2)
                        (setq result (funcall make-tag)))))
               result))))))

(defun pollen-insert-tab-or-command-char (&optional arg)
  "Insert a tab or a command char in the document.

Making it easy to insert a command char in the document.  If the
preceding char is @, replace it with the command char.  ARG is
the same ARG for indent"
  (interactive)
  (cond ((string= (string (preceding-char))
                  pollen-command-char-target)
         (delete-char -1)
         (insert pollen-command-char))
        (t (indent-for-tab-command arg))))

(defun pollen--goto-enclosing-left-brace ()
  "Go to the left brace enclosing current point."
  (interactive)
  (let ((tag (pollen--get-current-tagobj)))
    (let ((lb-pos (pollen--tag-lbraces tag)))
      (if lb-pos
          (goto-char lb-pos)
        (message "unbalanced braces."))))  )

(defun pollen--goto-enclosing-right-brace ()
  "Go to the right brace enclosing current point.

Return t if succeed, nil otherwise."
  (interactive)
  (let ((tag (pollen--get-current-tagobj)))
    (let ((rb-pos (pollen--tag-rbraces tag)))
      (if rb-pos
          (goto-char rb-pos)
        (message "unbalanced braces.")))))

(defun pollen-edit-block-other-window ()
  "This command does similar thing as org-edit-special in \"org-mode\".

When the cursor inside any block enclosed by braces, this
function will pop up another buffer containing only the content
of that block. Feel free to change the new buffer's mode.

To exit the editing, just kill the newly poped out buffer. It is
less powerful, but this working flow provide flexibility for
pollen."
  (interactive)
  (let ((tag (pollen--get-current-tagobj)))
    (if (null tag)
        (message "Are you really inside a block?")
      (let ((l (pollen--tag-lbraces tag))
            (r (pollen--tag-rbraces tag)))
        (if (and l r)
            (let* ((l (1+ l))
                   (text (buffer-substring-no-properties l r)))
              (let* ((cur-buf (current-buffer))
                     (new-buf (make-indirect-buffer cur-buf "*pollen-editing*")))
                (switch-to-buffer-other-window new-buf)
                (narrow-to-region l r)))
          (message "Unbalanced braces."))))))

(defvar pollen-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'pollen-insert-tab-or-command-char)
    (define-key map (kbd "C-c '") 'pollen-edit-block-other-window)
    map
    ))

(define-minor-mode pollen-minor-mode
  "pollen minor mode.

Keybindings for editing pollen file."
  nil
  " PM"
  :keymap pollen-mode-map
  :group 'pollen)

(defun pollen-minor-mode-on ()
  "Turn on pollen minor mode."
  (pollen-minor-mode 1))

;; together with (add-hook * * * t) pollen will be always on.
(put 'pollen-minor-mode-on 'permanent-local-hook t)

(defun pollen--mark-comment-delimiter-at (pos ty)
  "A warper for put comment text property at POS and (POS+1) of type TY."
  (put-text-property pos (1+ pos) 'syntax-table (string-to-syntax ty)))

(defun pollen--propertize-comment ()
  "Fix pollen comments in syntax table."
  (let* ((pos (match-end 0))
         (tag (pollen--get-current-tagobj))
         (beg (pollen--tag-lbraces tag))
         (end (pollen--tag-rbraces tag)))
    (when (and tag (string-equal (pollen--tag-name tag) ";") beg end)
      (pollen--mark-comment-delimiter-at beg "< bn")
      (pollen--mark-comment-delimiter-at end "> bn")
      (when (< end pos)
        ;; previous comment is not propertized yet, do it again
        (pollen--propertize-comment)))))


(defconst pollen--syntax-propertize-function
  (syntax-propertize-rules
   ("◊;{" (0 (ignore
              (pollen--propertize-comment))))
   ("}" (0 (when (nth 7 (syntax-ppss))
             ;; when modifying text at the same line of the comment
             ;; closing "}", its text property will be cleaned. Mark
             ;; "}" as a comment delimiter again.
             (let ((end (match-end 0)))
               (save-excursion
                 (let ((parse-sexp-lookup-properties nil))
                   (backward-sexp 1)
                   (when (char-equal ?\; (char-before))
                     ;; if its counterpart is a start pos of comment,
                     ;; mark it as comment end
                     (pollen--mark-comment-delimiter-at (1- end) "> bn"))))))))))

(define-derived-mode pollen-mode fundamental-mode
  "pollen"
  "Major mode for pollen file"
  ;; syntax highlights
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (setq-local parse-sexp-lookup-properties nil)

  (set (make-local-variable 'font-lock-defaults)
       '((pollen-highlights) nil nil))
  (set (make-local-variable 'syntax-propertize-function)
       pollen--syntax-propertize-function)
  ;; make the minor mode available across all major modes (even if major
  ;; mode falls through)
  (add-hook 'after-change-major-mode-hook 'pollen-minor-mode-on t t))

;;;; Bind pollen mode with file suffix
(add-to-list 'auto-mode-alist '("\\.pm$" . pollen-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" pollen-mode t))
(add-to-list 'auto-mode-alist '("\\.p$"  pollen-mode t))


(provide 'pollen-mode)

;;; pollen-mode.el ends here
