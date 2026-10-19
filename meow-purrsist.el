;;; meow-purrsist.el --- Persistent selections for Meow -*- lexical-binding: t; -*-

;; Author: Jon Suderman
;; Version: 0.1.0
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
;; regions produced by other packages.  Bind the commands you want directly;
;; `meow-purrsist-mode' only makes Meow's thing selectors enter this model.
;;
;; Meow has no public API for constructing typed selections or maintaining
;; their history.  This package uses its private selection constructors,
;; selection type, history-free update, cancellation, and redisplay backups.
;; The minimum Meow release includes these internals.  Check compatibility when
;; upgrading Meow.

;;; Code:

(require 'meow)
(require 'rect)

(dolist (function '(meow--make-selection meow--select
                    meow--select-without-history meow--selection-type
                    meow--cancel-selection))
  (unless (fboundp function)
    (error "meow-purrsist requires Meow capability %s" function)))
(dolist (variable '(meow--selection meow--selection-history
                    meow--last-find meow--last-till))
  (unless (boundp variable)
    (error "meow-purrsist requires Meow state %s" variable)))

(defvar-local meow-purrsist--visual-stage 0)
(defvar-local meow-purrsist--line-position nil)

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

(defun meow-purrsist--line-start (pos)
  "Return the start of the shown line holding POS.
Hidden lines, such as a folded body, belong to the shown line above."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (while (and (not (bobp)) (invisible-p (point)))
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
        (while (and (invisible-p (point)) (zerop (forward-line step))))))
    (meow-purrsist--line-start (point))))

(defun meow-purrsist--move-line-selection (n)
  "Move the active line-selection endpoint N physical lines.
Hidden lines are skipped, and a folded line is selected with its body."
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
  "Move down, adjusting line selections in physical direction."
  (interactive "P")
  (if (equal (meow--selection-type) '(expand . line))
      (meow-purrsist--move-line-selection (prefix-numeric-value arg))
    (meow-next arg)))

(defun meow-purrsist-prev (arg)
  "Move up, adjusting line selections in physical direction."
  (interactive "P")
  (if (equal (meow--selection-type) '(expand . line))
      (meow-purrsist--move-line-selection (- (prefix-numeric-value arg)))
    (meow-prev arg)))

(defun meow-purrsist-visual ()
  "Select word, symbol, then enclosing block on consecutive calls.
After the third call, further consecutive calls keep the block."
  (interactive)
  (setq meow-purrsist--visual-stage
        (if (eq last-command 'meow-purrsist-visual)
            (min 3 (1+ meow-purrsist--visual-stage))
          1))
  (pcase meow-purrsist--visual-stage
    (1 (when (bound-and-true-p rectangle-mark-mode)
         (meow--cancel-selection)
         (rectangle-mark-mode -1))
       (meow-mark-word 1)
       (meow-purrsist-adopt-region))
    (2 (goto-char (region-beginning))
       (meow-mark-symbol 1)
       (meow-purrsist-adopt-region))
    (3 (meow-block nil)
       (meow-purrsist-adopt-region))))

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
    (meow-line n))
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

(defun meow-purrsist--adopt-meow-thing (&rest _)
  "Make a newly selected Meow thing adjustable by ordinary motion."
  (when (region-active-p)
    (meow-purrsist-adopt-region)))

(defconst meow-purrsist--thing-commands
  '(meow-beginning-of-thing meow-end-of-thing
    meow-inner-of-thing meow-bounds-of-thing))

(defvar meow-purrsist--rectangle-highlight-added nil)
(defvar meow-purrsist--rectangle-unhighlight-added nil)

;;;###autoload
(define-minor-mode meow-purrsist-mode
  "Keep Meow thing selections adjustable and rectangles visible.
This global integration installs narrow advice; it does not bind keys."
  :global t
  :group 'meow
  (dolist (command meow-purrsist--thing-commands)
    (if meow-purrsist-mode
        (progn
          (advice-remove command #'meow-purrsist--adopt-meow-thing)
          (advice-add command :after #'meow-purrsist--adopt-meow-thing))
      (advice-remove command #'meow-purrsist--adopt-meow-thing)))
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
