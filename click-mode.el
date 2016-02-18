;;; click-mode.el --- Major mode for the Click Modular Router Project

;; Copyright (c) 2016 Brian Malehorn. All rights reserved.
;; Use of this source code is governed by a MIT-style
;; license that can be found in the LICENSE.txt file.

;; Author: Brian Malehorn <bmalehorn@gmail.com>
;; Version: 0.0.2
;; Package-Requires: ((emacs "24"))
;; Keywords: click router
;; URL: https://github.com/bmalehorn/click-mode

;; This file is not part of Emacs.


;;; Commentary:

;; See https://github.com/bmalehorn/click-mode for more.

;;; Code:

;; pilfered from https://www.emacswiki.org/emacs/wpdl-mode.el
(defvar click-mode-syntax-table
  (let ((click-mode-syntax-table (make-syntax-table)))
    ;; Comment styles are same as C++
    (modify-syntax-entry ?/ ". 124b" click-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" click-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" click-mode-syntax-table)
    click-mode-syntax-table)
  "Syntax table for `click-mode'.")

(defvar click-arguments
  "[A-Z][A-Z_0-9]*\\|init\\|setq\\|set\\|writeq\\|write\\|goto\\|label\\|end"
  "Argument names for `click-mode', e.g.
    Paint(ANNO FOO, COLOR BAR)
          ----      -----
We also include

   Script(write foo.x 1, write foo.y 2)

...Script arguments, since those are typically non-capitalized."
  )

(defun click-indent-line ()
  "\"Correct\" the indentation for the current line."
  (save-excursion
    (let* ((line (click-what-line))
           (changer (click-previous-indentation line)))
      (or (click-indent-copycat line changer "\\[")
          (click-indent-copycat line changer "->")
          (click-indent-copycat line changer "=>")))))

(defun click-indent-copycat (line changer regexp)
  "Indent the same as the previous line.
e.g. (click-indent-copycat 1 2 [) will indent this:

1:    => ( [0] -> foo;
2:      [1] -> bar; )

to this:

1:    => ( [0] -> foo;
2:         [1] -> bar; )

If the line with different indentation does not contain REGEXP,
returns nil. Otherwise, returns the new indentation.

"
  (save-excursion
    (back-to-indentation)
    (when (and (looking-at regexp)
               (progn
                 (goto-line changer)
                 (back-to-indentation)
                 (looking-at (concat ".*" regexp))))
      (while (not (looking-at regexp))
        (forward-char))
      (let* ((bracket (point))
             (bol (progn (beginning-of-line) (point)))
             (indent (- bracket bol)))
        (goto-line line)
        (indent-line-to indent)
        indent))))

(defun click-previous-indentation (line)
  "
1:    foo {
2:        bar;
3:        ack;
4:    }

(click-previous-indentation 3) => 1
"
  (save-excursion
    (goto-line line)
    (let* ((indentation (current-indentation)))
      (while (and (<= indentation (current-indentation)) (not (bobp)))
        (next-line -1))
      (click-what-line))))

(defun click-what-line ()
  "Returns the current line."
  (save-excursion
    (beginning-of-line)
    (+ 1 (count-lines 1 (point)))))

(defvar click-basic-offset 4
  "How many spaces to \"correct\" indentation to.
Analogous to `c-basic-offset'.")

(defvar click-highlights
  `(
    ;; #define FOO 5
    ("#\\s-*[a-z]*" . font-lock-preprocessor-face)
    ;; elementclass
    (,(concat
       "\\(^\\|[^a-zA-Z_0-9.]\\)\\("
       "elementclass\\|output\\|input\\|require"
       "\\)\\([^a-zA-Z_0-9.]\\|$\\)")
     . (2 font-lock-keyword-face))
    ;; Foo(
    ("\\([a-zA-Z_][a-zA-Z_0-9]*\\)("
     . (1 font-lock-function-name-face))
    ;; :: Foo
    ("::\\s-*\\([a-zA-Z_][a-zA-Z_0-9]*\\)"
     . (1 font-lock-function-name-face))
    ;; -> Foo
    ("->\\s-*\\([A-Z][a-zA-Z_0-9]*\\)"
     . (1 font-lock-function-name-face))
    ;; Foo ->
    ("\\([A-Z][a-zA-Z_0-9]*\\)\\s-*->"
     . (1 font-lock-function-name-face))
    ;; foo
    ("^\\s-*\\([a-z_][a-zA-Z_0-9]*\\)\\s-*$"
     . font-lock-variable-name-face)
    ;; foo ::
    ("\\([a-zA-Z_][a-zA-Z_0-9]*\\) *\\(, *[a-zA-Z_][a-zA-Z_0-9]*\\)* *::"
     . (1 font-lock-variable-name-face))
    ;; [0,1,2] foo
    ("\\(\\[[0-9, ]*\\]\\) *\\([a-z_][a-zA-Z_0-9]*\\)"
     . (2 font-lock-variable-name-face))
    ;; foo [0,1,2]
    ("\\([a-z_][a-zA-Z_0-9]*\\) *\\(\\[[0-9, ]*\\]\\)"
     . (1 font-lock-variable-name-face))
    ;; -> foo
    ("-> *\\([a-z_][a-zA-Z_0-9]*\\)"
     . (1 font-lock-variable-name-face))
    ;; foo ->
    ("\\([a-z_][a-zA-Z_0-9]*\\) *->"
     . (1 font-lock-variable-name-face))
    ;; elementclass Foo {
    ("elementclass *\\([a-zA-Z_][a-zA-Z_0-9]*\\) "
     . (1 font-lock-type-face))
    ;; Foo(BAR bar)
    (,(concat "(\\(" click-arguments "\\) ")
     . (1 font-lock-constant-face))
    ;; Foo(BAR bar, ACK ack)
    (,(concat ", *\\(" click-arguments "\\) ")
     . (1 font-lock-constant-face))
    ;; ACTIVE false,
    (,(concat
       "^\\s-*\\(" click-arguments "\\) .*\\(,\\|);?\\) *$")
     . (1 font-lock-constant-face))
    )

  "Syntax highlighting for `click-mode'."
  )


;;;###autoload
(define-derived-mode click-mode prog-mode "Click"
  (setq comment-start "// ")
  (setq comment-start-skip "//+\\s-*")
  (set-syntax-table click-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'click-indent-line)
  (setq font-lock-defaults '(click-highlights)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.click\\'" . click-mode))


(provide 'click-mode)

;;; click-mode.el ends here
