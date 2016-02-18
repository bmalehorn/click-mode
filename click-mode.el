;;; click-mode.el --- Major mode for the Click Modular Router Project

;; Syntax highlighting for Click templates.
;; See ./example.click for all possible syntax.

;; http://read.cs.ucla.edu/click/click

;; pilfered from https://www.emacswiki.org/emacs/wpdl-mode.el
(defvar click-mode-syntax-table
  (let ((click-mode-syntax-table (make-syntax-table)))

    ; This is added so entity names with underscores can be more easily parsed
        (modify-syntax-entry ?_ "w" click-mode-syntax-table)

        ; Comment styles are same as C++
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

(defvar click-highlights
  `(
    ;; #define FOO 5
    ("#\\s-*[a-z]*" . font-lock-preprocessor-face)
    ;; elementclass
    (,(concat
       "\\(^\\|[^a-zA-Z_0-9.]\\)\\("
       "elementclass\\|output\\|input"
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
(define-derived-mode click-mode fundamental-mode
  (setq comment-start "// ")
  (setq comment-start-skip "//+\\s-*")
  (set-syntax-table click-mode-syntax-table)
  (setq font-lock-defaults '(click-highlights))
  (setq mode-name "Click"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.click\\'" . click-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.template\\'" . click-mode))

(provide 'click-mode)
;;; click-mode.el ends here
