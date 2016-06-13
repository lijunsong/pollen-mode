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
;;
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
  "Go to the left brace enclosing current point.

Return t if succeed, nil otherwise."
  (interactive)
  (condition-case nil
      (if (looking-at "{")
          t
        (while (not (looking-at "{"))
          (if (looking-at "}")
              (progn (forward-char 1)
                     (backward-sexp)))
          (backward-char 1))
        t)
    (error nil)))

(defun pollen--goto-enclosing-right-brace ()
  "Go to the left brace enclosing current point.

Return t if succeed, nil otherwise."
  (interactive)
  (condition-case nil
      (progn
        (if (looking-at "}")
            t
          (while (not (looking-at "}"))
            (if (looking-at "{")
                (forward-sexp))
            (forward-char 1)))
        ;; foward one more
        (forward-char 1)
        t)
    (error nil)))

(defun pollen-edit-block-other-window ()
  "This command does similar thing as org-edit-special in \"org-mode\".

When the cursor inside any block enclosed by braces, this
function will pop up another buffer containing only the content
of that block. Feel free to change the new buffer's mode.

To exit the editing, just kill the newly poped out buffer. It is
less powerful, but this working flow provide flexibility for
pollen."
  (interactive)
  (let ((l nil)
        (r nil))
    (save-excursion
      (if (pollen--goto-enclosing-left-brace)
          (setq l (point))))
    (save-excursion
      (if (pollen--goto-enclosing-right-brace)
          (setq r (point))))
    (if (and l r)
        (let* ((l (1+ l))
               (r (1- r))
               (text (buffer-substring-no-properties l r)))
          (let* ((cur-buf (current-buffer))
                 (new-buf (make-indirect-buffer cur-buf "*pollen-editing*")))
            (switch-to-buffer-other-window new-buf)
            (narrow-to-region l r)))
      (message "Are you really inside a block?"))))

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

(define-derived-mode pollen-mode fundamental-mode
  "pollen"
  "Major mode for pollen file"
  ;; syntax highlights
  (setq font-lock-defaults '(pollen-highlights))
  ;; make the minor mode available across all major modes (even if major
  ;; mode falls through)
  (add-hook 'after-change-major-mode-hook 'pollen-minor-mode-on t t))

;;;; Bind pollen mode with file suffix
(add-to-list 'auto-mode-alist '("\\.pm$" . pollen-mode))
(add-to-list 'auto-mode-alist '("\\.pp$" pollen-mode t))
(add-to-list 'auto-mode-alist '("\\.p$"  pollen-mode t))


(provide 'pollen-mode)

;;; pollen-mode.el ends here
