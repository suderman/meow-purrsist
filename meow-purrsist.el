;;; meow-purrsist.el --- Persistent selections for Meow -*- lexical-binding: t; -*-

;; Author: Jon Suderman
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (meow "20260714.1200"))
;; Keywords: convenience, editing
;; URL: https://github.com/suderman/meow-purrsist

;;; Commentary:
;; Meow gives word, line, and other selection types distinct motion behavior.
;; Here semantic selectors establish initial bounds, then ordinary motion can
;; reshape the same region.  Select a word, expand to a symbol or block, move
;; by word or line, reverse, and keep adjusting the active endpoint.  Line
;; selections retain their spatial, whole-line behavior.  Fine movement does
;; not add a history entry for each endpoint change; semantic selections do.
;;
;; `meow-purrsist-adopt-region' also accepts native Emacs regions, including
;; regions produced by other packages.  Searches, line and buffer motions,
;; and find or till repeats follow the same model.  Bind the commands you
;; want directly; `meow-purrsist-mode' only makes Meow's thing selectors,
;; Surround, and touch selection enter this model.
;;
;; Meow has no public API for constructing typed selections or maintaining
;; their history.  This package uses its private selection constructors,
;; selection type, history-free update, cancellation, direction, search
;; ring and highlighting, and redisplay backups.
;; The minimum Meow release includes these internals.  Check compatibility when
;; upgrading Meow.

;;; Code:

(require 'meow)
(require 'rect)
(require 'seq)
(require 'subr-x)

(declare-function outline-back-to-heading "outline")
(declare-function outline-end-of-subtree "outline")
(declare-function outline-up-heading "outline")
(declare-function org-element-context "org-element")
(declare-function org-element-at-point "org-element")
(declare-function org-element-property "org-element-ast")
(declare-function org-element-type "org-element-ast")
(declare-function treesit-parser-list "treesit.c")
(declare-function treesit-local-parsers-at "treesit")
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-children "treesit.c")
(declare-function treesit-node-type "treesit.c")
(declare-function treesit-node-start "treesit.c")
(declare-function treesit-node-end "treesit.c")
(declare-function treesit-node-parent "treesit.c")

(dolist (function '(meow--make-selection meow--select
                    meow--select-without-history meow--selection-type
                    meow--cancel-selection meow--direction-forward
                    meow--direction-backward meow--push-search
                    meow--highlight-regexp-in-buffer
                    meow--remove-search-indicator))
  (unless (fboundp function)
    (error "Meow lacks %s, which meow-purrsist requires" function)))
(dolist (variable '(meow--selection meow--selection-history
                    meow--last-find meow--last-till))
  (unless (boundp variable)
    (error "Meow lacks %s, which meow-purrsist requires" variable)))

(defvar-local meow-purrsist--line-position nil
  "Point where `meow-purrsist-line-or-rectangle' began a line selection.
The rectangle that follows on the next call starts here.")

(defun meow-purrsist--select (anchor target &optional type history)
  "Select from ANCHOR to TARGET with TYPE, recording HISTORY if non-nil."
  (let ((selection (meow--make-selection (or type '(expand . char))
                                         anchor target)))
    (if history
        (meow--select selection t)
      (meow--select-without-history selection))))

(defun meow-purrsist-adopt-region (&optional line)
  "Adopt the active Emacs region as an expandable Meow selection.
Preserve its bounds and point/mark direction.  With LINE non-nil, retain
line selection type for a region representing complete lines.  No-op if
there is no active region or rectangle mode is active."
  (when (and (region-active-p)
             (not (bound-and-true-p rectangle-mark-mode)))
    (let ((anchor (mark))
          (target (point)))
      (meow-purrsist--select anchor target
                             (if line '(expand . line) '(expand . char))
                             (not (meow--selection-type))))))

(defun meow-purrsist-move-to (target)
  "Move to TARGET, keeping the active selection's anchor fixed."
  (if (region-active-p)
      (meow-purrsist--select (mark) target)
    (goto-char target)))

(defun meow-purrsist-extend-with-motion (motion)
  "Run MOTION and reshape the active selection to its destination.
Without a region, just run MOTION.  MOTION is a function of no arguments."
  (if (not (region-active-p))
      (funcall motion)
    (let ((anchor (mark))
          target)
      (save-mark-and-excursion
        (let ((mark-active nil)
              (meow--selection nil)
              (meow--selection-history nil))
          (funcall motion)
          (setq target (point))))
      (meow-purrsist--select anchor target))))

(defun meow-purrsist--move-thing (thing count)
  "Move COUNT instances of THING, keeping an active anchor."
  (meow-purrsist-move-to
   (save-excursion (forward-thing thing count) (point))))

(defun meow-purrsist-next-word (n)
  "Move forward N words, extending the current selection if active."
  (interactive "p")
  (meow-purrsist--move-thing meow-word-thing n))

(defun meow-purrsist-back-word (n)
  "Move backward N words, extending the current selection if active."
  (interactive "p")
  (meow-purrsist--move-thing meow-word-thing (- n)))

(defun meow-purrsist-next-symbol (n)
  "Move forward N symbols, extending the current selection if active."
  (interactive "p")
  (meow-purrsist--move-thing meow-symbol-thing n))

(defun meow-purrsist-back-symbol (n)
  "Move backward N symbols, extending the current selection if active."
  (interactive "p")
  (meow-purrsist--move-thing meow-symbol-thing (- n)))

(defun meow-purrsist--next-start (syntax n)
  "Move to the next N starts of SYNTAX constituents."
  (meow-purrsist-move-to
   (save-excursion
     (if (> n 0)
         (dotimes (_ n)
           (skip-syntax-forward syntax)
           (skip-syntax-forward (concat "^" syntax)))
       (dotimes (_ (- n))
         (skip-syntax-backward (concat "^" syntax))
         (skip-syntax-backward syntax)))
     (point))))

(defun meow-purrsist-next-word-start (n)
  "Move to the start of the next N words, extending a selection."
  (interactive "p")
  (meow-purrsist--next-start "w" n))

(defun meow-purrsist-next-symbol-start (n)
  "Move to the start of the next N symbols, extending a selection."
  (interactive "p")
  (meow-purrsist--next-start "w_" n))

(defun meow-purrsist-beginning-of-line ()
  "Move to indentation, or to the line beginning when already there.
Extend an active selection."
  (interactive)
  (meow-purrsist-move-to
   (save-excursion
     (let ((origin (point)))
       (back-to-indentation)
       (if (= origin (point)) (line-beginning-position) (point))))))

(defun meow-purrsist-end-of-line ()
  "Move to the end of code before a trailing comment, else the line end.
At the end of code, move on to the line end.  Extend an active selection."
  (interactive)
  (let* ((eol (line-end-position))
         (code-end (save-excursion
                     (comment-normalize-vars)
                     (goto-char eol)
                     (when-let* ((comment (comment-beginning)))
                       (goto-char comment)
                       (skip-chars-backward " \t")
                       (point)))))
    (meow-purrsist-move-to
     (if (and code-end
              (> code-end (line-beginning-position))
              (< (point) code-end))
         code-end
       eol))))

(defun meow-purrsist-beginning-of-buffer ()
  "Move to the beginning of the buffer, extending an active selection."
  (interactive)
  (meow-purrsist-move-to (point-min)))

(defun meow-purrsist-end-of-buffer ()
  "Move to the end of the buffer, extending an active selection."
  (interactive)
  (meow-purrsist-move-to (point-max)))

(defun meow-purrsist-end-or-beginning-of-buffer ()
  "Move to the buffer end, or the beginning when already on the last line.
Extend an active selection."
  (interactive)
  (if (= (line-end-position) (point-max))
      (meow-purrsist-beginning-of-buffer)
    (meow-purrsist-end-of-buffer)))

(defun meow-purrsist--hidden-line-p (bol)
  "Return non-nil if the line starting at BOL shows on the line above.
Its start may be hidden, or only the newline before it, as for the blank
line Org leaves after a folded body."
  (or (invisible-p bol)
      (and (> bol (point-min)) (invisible-p (1- bol)))))

(defun meow-purrsist--line-start (pos)
  "Return the start of the shown line holding POS.
Hidden lines, such as a folded body, belong to the shown line above."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (while (and (not (bobp)) (meow-purrsist--hidden-line-p (point)))
      (forward-line -1))
    (point)))

(defun meow-purrsist--line-end (pos)
  "Return the end of the shown line holding POS, past any hidden text."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (while (and (not (eobp)) (invisible-p (point)))
      (goto-char (next-single-char-property-change (point) 'invisible))
      (end-of-line))
    (point)))

(defun meow-purrsist--line-start-from (pos n)
  "Return the start of the shown line N physical lines from POS."
  (save-excursion
    (goto-char (meow-purrsist--line-start pos))
    (let ((step (if (< n 0) -1 1)))
      (dotimes (_ (abs n))
        (forward-line step)
        (while (and (meow-purrsist--hidden-line-p (point))
                    (zerop (forward-line step))))))
    (meow-purrsist--line-start (point))))

(defun meow-purrsist--keep-line-end ()
  "Keep point at a line end where hidden text begins, such as a fold.
Emacs would otherwise move point out of the hidden text and back before
the last shown character."
  (setq disable-point-adjustment t))

(defun meow-purrsist--move-line-selection (n)
  "Move the active line-selection endpoint N physical lines.
Hidden lines are skipped, and a folded line is selected with its body."
  (meow-purrsist--keep-line-end)
  (let* ((anchor (mark))
         (anchor-beg (meow-purrsist--line-start anchor))
         (target-beg (meow-purrsist--line-start-from (point) n)))
    (cond
     ((> target-beg anchor-beg)
      (meow-purrsist--select anchor-beg (meow-purrsist--line-end target-beg)
                             '(expand . line)))
     ((< target-beg anchor-beg)
      (meow-purrsist--select (meow-purrsist--line-end anchor) target-beg
                             '(expand . line)))
     (t
      (meow-purrsist--select anchor-beg
                             (save-excursion
                               (goto-char anchor-beg) (line-end-position))
                             '(expand . line))))))

(defun meow-purrsist-next (arg)
  "Move down ARG lines, adjusting line selections in physical direction."
  (interactive "P")
  (if (equal (meow--selection-type) '(expand . line))
      (meow-purrsist--move-line-selection (prefix-numeric-value arg))
    (meow-next arg)))

(defun meow-purrsist-prev (arg)
  "Move up ARG lines, adjusting line selections in physical direction."
  (interactive "P")
  (if (equal (meow--selection-type) '(expand . line))
      (meow-purrsist--move-line-selection (- (prefix-numeric-value arg)))
    (meow-prev arg)))

(defun meow-purrsist--trim (bounds)
  "Return BOUNDS without surrounding whitespace, or nil if nothing remains."
  (when bounds
    (save-excursion
      (let ((beg (progn (goto-char (car bounds))
                        (skip-chars-forward " \t\n")
                        (point)))
            (end (progn (goto-char (cdr bounds))
                        (skip-chars-backward " \t\n")
                        (point))))
        (when (< beg end) (cons beg end))))))

(defun meow-purrsist--thing-bounds (thing pos)
  "Return the bounds of THING at POS."
  (save-excursion (goto-char pos) (bounds-of-thing-at-point thing)))

(defun meow-purrsist--subtree-bounds ()
  "Return the bounds of the outline subtree at point."
  (cons (point) (save-excursion (outline-end-of-subtree) (point))))

(defun meow-purrsist--path-bounds (pos)
  "Return the file path around POS, without trailing punctuation.
Only text with a slash or dot counts as a path."
  (when-let* ((bounds (meow-purrsist--thing-bounds 'filename pos)))
    (save-excursion
      (goto-char (cdr bounds))
      (skip-chars-backward ".,:;" (car bounds))
      (when (and (< pos (point))
                 (string-match-p "[/.]" (buffer-substring (car bounds) (point))))
        (cons (car bounds) (point))))))

(defun meow-purrsist--treesit-parsers ()
  "Return the buffer's tree-sitter parsers, or nil without tree-sitter."
  (and (fboundp 'treesit-parser-list) (treesit-parser-list)))

(defun meow-purrsist--in-code-block-p (pos)
  "Return non-nil if POS is in an Org block or a tree-sitter code block."
  (if (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char pos)
        (memq (org-element-type (org-element-at-point))
              '(src-block example-block export-block comment-block)))
    (seq-some
     (lambda (parser)
       (let ((node (treesit-node-at pos parser)))
         (while (and node (not (string-match-p
                                "code_block" (treesit-node-type node))))
           (setq node (treesit-node-parent node)))
         node))
     (meow-purrsist--treesit-parsers))))

(defun meow-purrsist--delimiter-node-p (node)
  "Return non-nil if NODE delimits its parent, like a fence or backtick.
A fenced code block's info string belongs to its opening fence."
  (string-match-p "_delimiter\\'\\|\\`info_string\\'" (treesit-node-type node)))

(defun meow-purrsist--delimited-nodes (pos)
  "Return bounds of tree-sitter nodes at POS wrapped in delimiter nodes.
Each node gives its inside, then itself.  Markdown code spans, emphasis,
and fenced code blocks mark their delimiters this way."
  (when-let* ((parsers (meow-purrsist--treesit-parsers)))
    (let (candidates)
      (dolist (parser (append (and (fboundp 'treesit-local-parsers-at)
                                   (treesit-local-parsers-at pos))
                              parsers))
        (let ((node (treesit-node-at pos parser)))
          (while node
            (let ((children (treesit-node-children node))
                  beg end)
              (while (and children
                          (meow-purrsist--delimiter-node-p (car children)))
                (setq beg (treesit-node-end (pop children))))
              (setq children (nreverse children))
              (while (and children
                          (meow-purrsist--delimiter-node-p (car children)))
                (setq end (treesit-node-start (pop children))))
              (when (and beg end)
                (push (cons beg end) candidates)
                (push (cons (treesit-node-start node) (treesit-node-end node))
                      candidates)))
            (setq node (treesit-node-parent node)))))
      candidates)))

(defun meow-purrsist--visual-candidates (pos)
  "Return regions that may enclose a selection starting at POS.
Code offers symbols, strings, brackets, and functions.  Prose offers
sentences and paragraphs instead of functions.  Org and tree-sitter
markup such as bold and code spans add their inside and outside, and
outlines add each containing subtree."
  (let* ((ppss (save-excursion (syntax-ppss pos)))
         (symbol (meow-purrsist--thing-bounds meow-symbol-thing pos))
         (symbols (list symbol))
         (path (meow-purrsist--path-bounds pos))
         (candidates (meow-purrsist--delimited-nodes pos))
         paragraph)
    ;; Markup such as Org's /either/or/ has symbol syntax, so offer the
    ;; symbol between its first and last word characters first.  A path
    ;; offers its own parts instead.
    (when (and symbol (not path))
      (save-excursion
        (let ((beg (progn (goto-char (car symbol))
                          (skip-syntax-forward "^w" (cdr symbol))
                          (point)))
              (end (progn (goto-char (cdr symbol))
                          (skip-syntax-backward "^w" (car symbol))
                          (point))))
          (when (< beg end) (push (cons beg end) symbols)))))
    (when path
      ;; The path's file name, then that name without extensions.
      (save-excursion
        (let ((beg (progn (goto-char pos)
                          (skip-chars-backward "^/" (car path))
                          (point)))
              (end (progn (goto-char pos)
                          (skip-chars-forward "^/" (cdr path))
                          (point))))
          (push (cons beg end) candidates)
          (goto-char pos)
          (skip-chars-forward "^./" end)
          (push (cons beg (point)) candidates)))
      (push path candidates)
      ;; A symbol such as scripts/name in scripts/name.py is no unit of
      ;; the path, so keep only symbols that start and end on its slashes.
      (when (string-match-p "/" (buffer-substring (car path) (cdr path)))
        (setq symbols
              (seq-filter
               (lambda (bounds)
                 (and bounds
                      (or (<= (car bounds) (car path))
                          (eq (char-before (car bounds)) ?/))
                      (or (>= (cdr bounds) (cdr path))
                          (eq (char-after (cdr bounds)) ?/))))
               symbols))))
    (setq candidates (append symbols candidates))
    ;; Strings and brackets, first without and then with delimiters.
    (dolist (open (append (and (nth 3 ppss) (list (nth 8 ppss)))
                          (nth 9 ppss)))
      (when-let* ((close (ignore-errors (scan-sexps open 1))))
        (push (cons (1+ open) (1- close)) candidates)
        (push (cons open close) candidates)))
    ;; Org knows its own structure: markup, items, drawers, blocks, and
    ;; headings each offer their contents, then themselves.
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (let ((object (progn (goto-char pos) (org-element-context))))
          (while object
            (let ((type (org-element-type object))
                  (beg (org-element-property :begin object))
                  (end (org-element-property :end object)))
              (when (memq type '(verbatim code))
                (let ((end (- end (org-element-property :post-blank object))))
                  (push (cons (1+ beg) (1- end)) candidates)))
              ;; Blocks keep their lines in a value, not contents.
              (when (memq type '(src-block example-block export-block
                                 comment-block))
                (let ((first (progn (goto-char beg) (forward-line 1) (point)))
                      (last (progn (goto-char end)
                                   (skip-chars-backward " \t\n")
                                   (forward-line 0)
                                   (point))))
                  (push (cons first last) candidates)))
              (when (eq type 'paragraph)
                (setq paragraph t))
              (when-let* ((contents (org-element-property :contents-begin object)))
                (push (cons contents (org-element-property :contents-end object))
                      candidates))
              (when beg
                (push (cons beg end) candidates)))
            (setq object (org-element-property :parent object))))))
    (cond
     ((derived-mode-p 'prog-mode)
      (push (meow-purrsist--thing-bounds 'defun pos) candidates))
     ;; Sentences and paragraphs mean nothing inside a prose file's code.
     ((meow-purrsist--in-code-block-p pos)
      (push (meow-purrsist--thing-bounds 'line pos) candidates))
     ;; Org's parser already offered paragraphs; sentences belong in them.
     ((derived-mode-p 'org-mode)
      (when paragraph
        (push (meow-purrsist--thing-bounds 'sentence pos) candidates)))
     (t
      (push (meow-purrsist--thing-bounds 'sentence pos) candidates)
      (push (meow-purrsist--thing-bounds 'paragraph pos) candidates)))
    (when (or (derived-mode-p 'outline-mode)
              (bound-and-true-p outline-minor-mode))
      (save-excursion
        (goto-char pos)
        (when (ignore-errors (outline-back-to-heading t) t)
          (push (meow-purrsist--subtree-bounds) candidates)
          (let ((last (point)))
            (while (and (ignore-errors (outline-up-heading 1 t) t)
                        (< (point) last))
              (setq last (point))
              (push (meow-purrsist--subtree-bounds) candidates))))))
    (delq nil (mapcar #'meow-purrsist--trim candidates))))

(defun meow-purrsist--expand ()
  "Select the smallest candidate that contains and grows the selection.
Keep the selection's direction and record it in Meow's history."
  (let ((beg (region-beginning))
        (end (region-end))
        best)
    (dolist (bounds (meow-purrsist--visual-candidates beg))
      (when (and (<= (car bounds) beg) (>= (cdr bounds) end)
                 (> (- (cdr bounds) (car bounds)) (- end beg))
                 (or (null best)
                     (< (- (cdr bounds) (car bounds))
                        (- (cdr best) (car best)))))
        (setq best bounds)))
    (when best
      (if (< (point) (mark))
          (meow-purrsist--select (cdr best) (car best) nil t)
        (meow-purrsist--select (car best) (cdr best) nil t)))))

(defun meow-purrsist-visual ()
  "Select a word, then grow the selection on consecutive calls.
Each call takes the smallest enclosing symbol, string, bracket,
sentence, paragraph, function, or outline subtree.  Every step is
recorded, so `meow-pop-selection' steps back.  At the largest
candidate, further calls keep the selection."
  (interactive)
  (if (and (eq last-command 'meow-purrsist-visual) (region-active-p))
      (meow-purrsist--expand)
    (when (bound-and-true-p rectangle-mark-mode)
      (meow--cancel-selection)
      (rectangle-mark-mode -1))
    (meow-mark-word 1)
    (meow-purrsist-adopt-region)))

(defun meow-purrsist-line-or-rectangle (n)
  "Select N lines, then rectangle, then entire buffer on repetition."
  (interactive "p")
  (cond
   ((not (eq last-command 'meow-purrsist-line-or-rectangle))
    (when (bound-and-true-p rectangle-mark-mode)
      (rectangle-mark-mode -1))
    ;; Point past a folded line's hidden body sits on a hidden line.
    (let ((start (meow-purrsist--line-start (point))))
      (unless (= start (line-beginning-position))
        (goto-char start)))
    (setq meow-purrsist--line-position (point))
    (meow-line n)
    (meow-purrsist--keep-line-end))
   ((equal (meow--selection-type) '(expand . line))
    (meow--cancel-selection)
    (goto-char meow-purrsist--line-position)
    (rectangle-mark-mode 1))
   (t
    (when (bound-and-true-p rectangle-mark-mode)
      (meow--cancel-selection)
      (rectangle-mark-mode -1))
    (meow-purrsist--select (point-min) (point-max) nil t))))

(defun meow-purrsist--find-motion (command n char)
  "Run Meow COMMAND for N instances of CHAR, keeping an existing anchor."
  (let ((anchor (and (region-active-p) (mark)))
        ;; Meow records the temporary find selection.  Keep only the prior
        ;; semantic history when this is an endpoint movement.
        (history meow--selection-history))
    ;; Forget the previous character, so a failed search leaves nothing
    ;; for `meow-purrsist-repeat' to repeat.
    (if (eq command 'meow-find)
        (setq meow--last-find nil)
      (setq meow--last-till nil))
    (funcall command n char t)
    (when (region-active-p)
      (let ((target (point)))
        (if anchor
            (progn
              (setq meow--selection-history history)
              (meow-purrsist--select anchor target))
          (meow--cancel-selection)
          (goto-char target))))))

(defun meow-purrsist-find (n char)
  "Find CHAR forward N times, extending an existing selection."
  (interactive "p\ncFind: ")
  (meow-purrsist--find-motion 'meow-find n char))

(defun meow-purrsist-find-backward (n char)
  "Find CHAR backward N times, extending an existing selection."
  (interactive "p\ncFind backward: ")
  (meow-purrsist-find (- n) char))

(defun meow-purrsist-till (n char)
  "Move until CHAR forward N times, extending an existing selection."
  (interactive "p\ncTill: ")
  (meow-purrsist--find-motion 'meow-till n char))

(defun meow-purrsist-till-backward (n char)
  "Move until CHAR backward N times, extending an existing selection."
  (interactive "p\ncTill backward: ")
  (meow-purrsist-till (- n) char))

(defcustom meow-purrsist-repeat-function nil
  "Function `meow-purrsist-repeat' calls with the count to repeat an edit.
It runs when the last command was not a find or till.  When nil, there
is nothing else to repeat."
  :type '(choice (const :tag "None" nil) function)
  :group 'meow)

(defun meow-purrsist-repeat (n)
  "Repeat the last find or till motion N times.
After any other command, call `meow-purrsist-repeat-function' with N."
  (interactive "p")
  (let ((char (pcase last-command
                ((or 'meow-purrsist-find 'meow-purrsist-find-backward)
                 meow--last-find)
                ((or 'meow-purrsist-till 'meow-purrsist-till-backward)
                 meow--last-till))))
    (cond
     (char
      (setq this-command last-command)
      (funcall last-command n char))
     (meow-purrsist-repeat-function
      (funcall meow-purrsist-repeat-function n))
     (t (user-error "Nothing to repeat")))))

(defun meow-purrsist--smartcase-regexp (string)
  "Return a regexp for STRING that ignores case when STRING is lowercase.
Meow searches with `case-fold-search' nil, so its search motions need
the case folding written into the regexp."
  (if (string= string (downcase string))
      (mapconcat (lambda (char)
                   (if (= char (upcase char))
                       (regexp-quote (char-to-string char))
                     (regexp-opt-charset (list char (upcase char)))))
                 string "")
    (regexp-quote string)))

(defun meow-purrsist--finish-isearch ()
  "Select a successful Isearch match and hand it to Meow's search motions."
  (remove-hook 'isearch-mode-end-hook #'meow-purrsist--finish-isearch t)
  (when (and (not isearch-mode-end-hook-quit)
             isearch-success
             (not (string-empty-p isearch-string))
             isearch-other-end)
    (meow--push-search (meow-purrsist--smartcase-regexp isearch-string))
    (set-mark isearch-other-end)
    (activate-mark)
    (meow-purrsist-adopt-region)
    (meow--highlight-regexp-in-buffer (car regexp-search-ring))))

(defun meow-purrsist-isearch ()
  "Search incrementally for literal text, then select the match.
A lowercase query ignores case.  `meow-purrsist-search' and
`meow-purrsist-search-backward' then step through the matches."
  (interactive)
  (when (region-active-p)
    (meow--cancel-selection))
  (meow--remove-search-indicator)
  (add-hook 'isearch-mode-end-hook #'meow-purrsist--finish-isearch nil t)
  ;; No recursive edit, as for interactive C-s.  Leaving one skips the
  ;; rest of `isearch-exit', which closes or opens folds the search showed.
  (let ((case-fold-search t)
        (search-upper-case t)
        (search-default-mode nil))
    (isearch-forward nil t)))

(defun meow-purrsist-search (&optional backward)
  "Select the next match of the last search, or the previous if BACKWARD.
When the selected text is not a match, search for that text instead.
Searches wrap, and the match can then be adjusted by motion."
  (interactive)
  (let ((selecting (region-active-p)))
    (when selecting
      (if backward (meow--direction-backward) (meow--direction-forward)))
    (meow-search (and backward (not selecting) -1)))
  (meow-purrsist-adopt-region))

(defun meow-purrsist-search-backward ()
  "Select the previous match of the last search."
  (interactive)
  (meow-purrsist-search t))

(defun meow-purrsist--adopt-meow-thing (&rest _)
  "Make a newly selected Meow thing adjustable by ordinary motion."
  (when (region-active-p)
    (meow-purrsist-adopt-region)))

(defun meow-purrsist--adopt-normal-region (&rest _)
  "Make a region another package selected adjustable in Normal state."
  (when (bound-and-true-p meow-normal-mode)
    (meow-purrsist-adopt-region)))

(defconst meow-purrsist--thing-commands
  '(meow-beginning-of-thing meow-end-of-thing
    meow-inner-of-thing meow-bounds-of-thing))

(defconst meow-purrsist--region-commands
  '(surround--op-mark touch-screen-hold)
  "Commands from other packages whose regions Normal state adopts.
Surround marks a pair's contents, and a long press on a touch screen
selects a word.")

(defvar meow-purrsist--rectangle-highlight-added nil)
(defvar meow-purrsist--rectangle-unhighlight-added nil)

;;;###autoload
(define-minor-mode meow-purrsist-mode
  "Keep thing, Surround, and touch selections adjustable; show rectangles.
This global integration installs narrow advice; it does not bind keys."
  :global t
  :group 'meow
  (dolist (command meow-purrsist--thing-commands)
    (if meow-purrsist-mode
        (advice-add command :after #'meow-purrsist--adopt-meow-thing)
      (advice-remove command #'meow-purrsist--adopt-meow-thing)))
  (dolist (command meow-purrsist--region-commands)
    (if meow-purrsist-mode
        (advice-add command :after #'meow-purrsist--adopt-normal-region)
      (advice-remove command #'meow-purrsist--adopt-normal-region)))
  ;; Meow replaces the redisplay functions used by rectangle-mark-mode when
  ;; its cursor-position hack is on.  Keep rectangle rendering in its backups.
  (if meow-purrsist-mode
      (when meow-use-cursor-position-hack
        (unless (advice-function-member-p
                 #'rectangle--highlight-for-redisplay
                 meow--backup-redisplay-highlight-region-function)
          (add-function :around meow--backup-redisplay-highlight-region-function
                        #'rectangle--highlight-for-redisplay)
          (setq meow-purrsist--rectangle-highlight-added t))
        (unless (advice-function-member-p
                 #'rectangle--unhighlight-for-redisplay
                 meow--backup-redisplay-unhighlight-region-function)
          (add-function :around meow--backup-redisplay-unhighlight-region-function
                        #'rectangle--unhighlight-for-redisplay)
          (setq meow-purrsist--rectangle-unhighlight-added t)))
    (when meow-purrsist--rectangle-highlight-added
      (remove-function meow--backup-redisplay-highlight-region-function
                       #'rectangle--highlight-for-redisplay)
      (setq meow-purrsist--rectangle-highlight-added nil))
    (when meow-purrsist--rectangle-unhighlight-added
      (remove-function meow--backup-redisplay-unhighlight-region-function
                       #'rectangle--unhighlight-for-redisplay)
      (setq meow-purrsist--rectangle-unhighlight-added nil))))

(provide 'meow-purrsist)
;;; meow-purrsist.el ends here
