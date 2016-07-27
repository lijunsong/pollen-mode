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

(defvar pollen-command-char-code ?\u25CA)
(defvar pollen-command-char (char-to-string pollen-command-char-code))
(defvar pollen-command-char-target "@")

;; Racket identifier see racket document syntax-overview 2.2.3.
(defvar pollen-racket-id-reg "[^][:space:]\n()[{}\",'`;#|\\]+")

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

(defun pollen-tag (name)
  "Concat commond char to the given NAME."
  (concat pollen-command-char name))

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
  ;; Specify \n explicitly because \n is not in [:space:] syntax class anymore.
  (let* ((skip-allowed "^[:space:]()[]{}\",'`;#|\\\n")
         (beg  (save-excursion
                 (skip-chars-backward skip-allowed)
                 (if (looking-at pollen-command-char)
                     (point)
                   nil)))
         (end (save-excursion
                (skip-chars-forward skip-allowed)
                (if (and beg (eq (- (point) beg) 1) (looking-at ";"))
                    (1+ (point))
                  (point)))))
    (if (and beg end (not (= beg end)))
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
  (let* ((parse-sexp-lookup-properties nil)
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
    (cond ((pollen-tag-at-point t)
           ;; in the front or the middle of a tag name
           (funcall make-tag))
          (t
           ;; search backward
           (save-excursion
             (let (result)
               (while (and (null result)
                           (progn
                             (skip-chars-backward  "^{}")
                             (not (= (point) (point-min)))))
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
of that block. Feel free to change the new buffer's mode."
  (interactive)
  (let ((tag (pollen--get-current-tagobj)))
    (if (null tag)
        (message "Are you really inside a block?")
      (let ((l (pollen--tag-lbraces tag))
            (r (pollen--tag-rbraces tag))
            (tag-name (pollen--tag-name tag)))
        (if (and l r)
            (let* ((l (1+ l))
                   (text (buffer-substring-no-properties l r))
                   ;; for restore window
                   (win-config (current-window-configuration)))
              (let* ((cur-buf (current-buffer))
                     (new-buf (make-indirect-buffer cur-buf "*pollen-editing*")))
                (switch-to-buffer-other-window new-buf)
                ;; working on the new buffer from this point
                (local-set-key (kbd "C-c '")
                               `(lambda ()
                                  (interactive)
                                  (kill-buffer-and-window)
                                  (set-window-configuration ,win-config)))
                (narrow-to-region l r)
                (setq-local header-line-format
                            (substitute-command-keys
                             (format "Editing %s%s. Close the window with <C-c '>"
                                     pollen-command-char tag-name)))))
          (message "Tag %s%s has unbalanced braces." pollen-command-char tag-name))))))

(defvar pollen-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") 'pollen-insert-tab-or-command-char)
    (define-key map (kbd "C-c '") 'pollen-edit-block-other-window)
    map
    ))

(defconst pollen-minor-mode-indicator
  (concat " " pollen-command-char-target "/" pollen-command-char))

(define-minor-mode pollen-minor-mode
  "pollen minor mode.

Keybindings for editing pollen file."
  nil
  pollen-minor-mode-indicator
  :keymap pollen-mode-map
  :group 'pollen)

(defun pollen-minor-mode-on ()
  "Turn on pollen minor mode."
  (pollen-minor-mode 1))

;; together with (add-hook * * * t) pollen will be always on.
(put 'pollen-minor-mode-on 'permanent-local-hook t)

(defun pollen--put-text-property-at (pos ty)
  "A warper for put at POS and (POS+1) text property of comment type TY."
  (if (null ty)
      (put-text-property pos (1+ pos) 'syntax-table nil)
    (put-text-property pos (1+ pos) 'syntax-table (string-to-syntax ty))))

(defun pollen--propertize-comment (semicolon-pos)
  "Fix pollen comments in syntax table at given SEMICOLON-POS."
  (goto-char semicolon-pos)
  ;; (assert (equal (char-after semicolon-pos) ?\;))
  (let* ((tag (pollen--get-current-tagobj))
         (beg (pollen--tag-lbraces tag))
         (end (pollen--tag-rbraces tag)))
    (when end
      (pollen--put-text-property-at semicolon-pos "."))
    (when (and tag (string-equal (pollen--tag-name tag) ";") beg end)
      (pollen--put-text-property-at beg "< bn")
      (pollen--put-text-property-at end "> bn"))))

;; Caveat: call (syntax-ppss (match-beginning 0)) will cause infinite loop.
;; Caveat: jump back without using save-excursion will cause infinite loop.
(defconst pollen--syntax-propertize-function
  (syntax-propertize-rules
   ("◊;{"
    (0
     (let ((beg-ppss (prog2
                         (backward-char 3)
                         (syntax-ppss)
                       (forward-char 3))))
       (unless (and (nth 4 beg-ppss)
                    (null (nth 7 beg-ppss)))
         ;; first unmark ; to normal punctuation
         (pollen--propertize-comment (1+ (match-beginning 0)))
         ))))
   ("}" (0
         (save-excursion
           (let ((parse-sexp-lookup-properties nil)
                 (ppss (syntax-ppss)))
             (ignore-errors
               (backward-sexp 1)
               (when (char-equal (char-before (point)) ?\;)
                 (cond ((equal 1 (nth 7 ppss))
                        ;; When } is in comment starting with ;{, mark
                        ;; it as endcomment.  When modifying text at
                        ;; the same line as the endcomment "}",
                        ;; its text property will be cleaned. Mark "}"
                        ;; as a comment delimiter again.
                        (pollen--put-text-property-at (match-beginning 0) "> bn"))
                       ((null (nth 4 ppss))
                        ;; Not in a comment, so add comment
                        (pollen--propertize-comment (1- (point)))))))))))))

(defvar pollen-syntax-table
  (let ((tb (make-syntax-table)))
    (modify-syntax-entry pollen-command-char-code ". 1")
    (modify-syntax-entry ?\; ". 2")
    (modify-syntax-entry ?\n ">")
    tb))

(define-derived-mode pollen-mode fundamental-mode
  "pollen"
  "Major mode for pollen file"
  (set (make-local-variable 'parse-sexp-ignore-comments) nil)
  (set (make-local-variable 'parse-sexp-lookup-properties) nil)
  (set-syntax-table pollen-syntax-table)
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
