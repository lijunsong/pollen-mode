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
;;; Code:

(defvar pollen-command-char "◊")
(defvar pollen-command-char-target "@")

(defvar pollen-racket-id-reg "[a-zA-Z][a-zA-Z0-9-]*")
(defvar pollen-header-reg "#lang .*$")

(defun pollen-gen-highlights (command-char)
  "Generate highlight given the pollen COMMAND-CHAR."
  (let ((id (concat command-char pollen-racket-id-reg))
        (malform (concat command-char "[ \\n]+"))
        (comment1 (concat command-char ";\\s\(.*\)"))
        (comment2 (concat command-char ";.*$")))
    `((,id . font-lock-variable-name-face)
      (,pollen-header-reg . font-lock-comment-face)
      (,malform . font-lock-warning-face)
      (,comment1 . font-lock-comment-face)
      (,comment2 . font-lock-comment-face))))

(defvar pollen-highlights
  (pollen-gen-highlights pollen-command-char)
  "Regexp for font lock in pollen.")

;;; Pollen model
(defun pollen--make-tag (name lb rb)
  "Create tag object.

A tag object has NAME (command char excluded), left brace LB and
right brace RB pos."
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
not a left brace or there is no matched one."
  (let (p)
    (save-excursion
      (goto-char pos)
      (when (char-equal (char-after pos) ?\{)
        (condition-case nil
            (progn
              (forward-sexp 1)
              (backward-char 1)
              (setq p (point)))
          (error nil))))
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
                            forward-allowed))
         (beg  (save-excursion
                 (skip-chars-backward backward-allowed)
                 (if (looking-at pollen-command-char)
                     (point)
                   nil)))
         (end (save-excursion
                (skip-chars-forward forward-allowed)
                (if (and (= (- (point) beg) 1) (looking-at ";"))
                    (1+ (point))
                  (point)))))
    (message "beg %d. end %d." beg end)
    (if (and beg end)
        (cons beg end)
      nil)))

(defun pollen-tag-at-point (&optional no-properties)
  "Return the tag at point, or nil if none is found.
NO-PROPERTIES will be passed to `thing-at-point'."
  (thing-at-point 'pollen--tag no-properties))


(defun pollen--get-current-tagobj ()
  "Get a tag object under the cursor.

It returns nil when the cursor is on the toplevel, the tag object
of the enclosing tag otherwise."
  (let* ((tag (thing-at-point 'symbol t))
         (ppss (syntax-ppss)))
    (cond ((and tag (string-prefix-p pollen-command-char tag))
           ;; in the front or the middle of a tag name
           (let ((bounds (bounds-of-thing-at-point 'symbol))
                 (name (substring tag 1)))
             (unless (string= name "")
               (let ((lb-pos (cdr bounds)))
                 (when (char-equal (char-after lb-pos) ?\{)
                   (pollen--make-tag
                    name lb-pos
                    (pollen--matched-right-brace-pos lb-pos)))))))
          ((null (nth 1 ppss))
           ;; this could happen is the cursor is on the toplevel
           nil)
          (t
           ;; inside braces or tag is null (This could happend when
           ;; the cursor is between punctuation and a space)
           (save-excursion
             (goto-char (nth 1 ppss))
             (pollen--get-current-tagobj))))))

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

(defun pollen--fix-comment-left ()
  "Test."
  ;; (point) inside this function always points to a pos right after
  ;; REGEXP
  (let ((comment-beg (point))
        (comment-end (when (pollen--goto-enclosing-right-brace)
                       (point))))
    (message "%d -- %d" comment-beg comment-end)
    (put-text-property comment-beg (1+ comment-beg)
                       'syntax-table (string-to-syntax "!"))
    (when comment-end
      (put-text-property comment-end (1+ comment-end)
                         'syntax-table (string-to-syntax "!")))))
(defun pollen--fix-comment-right ()
  "Test."
  (backward-char 1)
  (let ((comment-beg ()))))

(defconst pollen--syntax-propertize-function
  (syntax-propertize-rules
   ("◊;{" (0 (ignore (pollen--fix-comment))))
   ("}" (0 (ignore (pollen--fix-comment))))))

(define-derived-mode pollen-mode fundamental-mode
  "pollen"
  "Major mode for pollen file"
  ;; syntax highlights
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'font-lock-defaults) '(pollen-highlights))
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
