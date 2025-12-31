;;; treesit-fold-indicators.el --- Display indicators for folding range  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026  emacs-tree-sitter maintainers

;; Created date 2021-10-04 20:03:12

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Display indicators for folding range
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'treesit-fold-util)
(require 'treesit-fold)

(defcustom treesit-fold-indicators-fringe 'left-fringe
  "Display indicators on the left/right fringe."
  :type '(choice (const :tag "On the right fringe" right-fringe)
                 (const :tag "On the left fringe" left-fringe))
  :group 'treesit-fold)

(defcustom treesit-fold-indicators-priority 30
  "Indicators overlay's priority."
  :type 'integer
  :group 'treesit-fold)

(defcustom treesit-fold-indicators-face-function nil
  "Function call when apply to indicators face."
  :type 'function
  :group 'treesit-fold)

;; TODO: We eventually want to remove this. Therefore, we get fast and
;; accurate results!
(defcustom treesit-fold-indicators-render-method 'partial
  "Method used to display indicators."
  :type '(choice (const :tag "Accurate rendering but cost more performance" full)
                 (const :tag "Inaccurate rendering but fast" partial))
  :group 'treesit-fold)

(defcustom treesit-fold-indicators-refresh-hook nil
  "Hook run after indicators refresh."
  :type 'hook
  :group 'treesit-fold)

(define-fringe-bitmap 'treesit-fold-indicators-fr-plus
  (vector #b1111111
          #b1000001
          #b1001001
          #b1011101
          #b1001001
          #b1000001
          #b1111111))

(define-fringe-bitmap 'treesit-fold-indicators-fr-minus-tail
  (vector #b00000000 #b00000000 #b00000000 #b00000000 #b00000000
          #b00000000 #b00000000 #b00000000 #b00000000 #b00000000
          #b1111111
          #b1000001
          #b1000001
          #b1011101
          #b1000001
          #b1000001
          #b1111111
          #b00011000 #b00011000 #b00011000 #b00011000 #b00011000
          #b00011000 #b00011000 #b00011000 #b00011000 #b00011000))

(define-fringe-bitmap 'treesit-fold-indicators-fr-center
  (vector #b00011000 #b00011000 #b00011000 #b00011000 #b00011000
          #b00011000 #b00011000 #b00011000 #b00011000 #b00011000
          #b00011000 #b00011000 #b00011000 #b00011000 #b00011000
          #b00011000 #b00011000 #b00011000 #b00011000 #b00011000
          #b00011000 #b00011000 #b00011000))

(define-fringe-bitmap 'treesit-fold-indicators-fr-end-left
  (vector #b00011000 #b00011000 #b00011000 #b00011000 #b00011000
          #b00011000 #b00011000 #b00011000 #b00011000 #b00011000
          #b00011000 #b00011111 #b00011111
          #b00000000 #b00000000 #b00000000 #b00000000 #b00000000
          #b00000000 #b00000000 #b00000000 #b00000000 #b00000000))

(define-fringe-bitmap 'treesit-fold-indicators-fr-end-right
  (vector #b00011000 #b00011000 #b00011000 #b00011000 #b00011000
          #b00011000 #b00011000 #b00011000 #b00011000 #b00011000
          #b00011000 #b11111000 #b11111000
          #b00000000 #b00000000 #b00000000 #b00000000 #b00000000
          #b00000000 #b00000000 #b00000000 #b00000000 #b00000000))

;;
;; (@* "Entry" )
;;

(defvar treesit-fold-indicators-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left-fringe mouse-1] #'treesit-fold-indicators-click-fringe)
    (define-key map [right-fringe mouse-1] #'treesit-fold-indicators-click-fringe)
    map)
  "Keymap for function `treesit-fold-indicators-mode'.")

(defun treesit-fold-indicators--enable ()
  "Enable `treesit-fold-indicators' mode."
  (add-hook 'after-change-functions #'treesit-fold-indicators--trigger-render nil t)
  (add-hook 'after-save-hook #'treesit-fold-indicators--trigger-render nil t)
  (add-hook 'post-command-hook #'treesit-fold-indicators--post-command nil t)
  (add-hook 'window-size-change-functions #'treesit-fold-indicators--size-change)
  (add-hook 'window-scroll-functions #'treesit-fold-indicators--scroll)
  (treesit-fold-indicators--render-buffer))

(defun treesit-fold-indicators--disable ()
  "Disable `treesit-fold-indicators' mode."
  (remove-hook 'after-change-functions #'treesit-fold-indicators--trigger-render t)
  (remove-hook 'after-save-hook #'treesit-fold-indicators--trigger-render t)
  (remove-hook 'post-command-hook #'treesit-fold-indicators--post-command t)
  (remove-hook 'window-size-change-functions #'treesit-fold-indicators--size-change)
  (remove-hook 'window-scroll-functions #'treesit-fold-indicators--scroll)
  (treesit-fold-indicators--remove-ovs-buffer))

;;;###autoload
(define-minor-mode treesit-fold-indicators-mode
  "Minor mode for display fringe folding indicators."
  :group 'treesit-fold
  :lighter nil
  :keymap treesit-fold-indicators-mode-map
  :init-value nil
  (cond
   ((not (and (or treesit-fold-mode (treesit-fold-mode 1))
              treesit-fold-indicators-mode))
    (when treesit-fold-indicators-mode
      (treesit-fold-indicators-mode -1)))
   (treesit-fold-indicators-mode
    (treesit-fold-indicators--enable) t)
   (t
    (treesit-fold-indicators--disable))))

;;;###autoload
(define-globalized-minor-mode global-treesit-fold-indicators-mode
  treesit-fold-indicators-mode treesit-fold-indicators--trigger
  :group 'treesit-fold)

(defun treesit-fold-indicators--trigger ()
  "Enable `treesit-fold-indicators-mode' when the `treesit-fold-mode' can
be enabled."
  (when (or treesit-fold-mode
            (treesit-fold-mode 1))
    (treesit-fold-indicators-mode 1)))
;;
;; (@* "Events" )
;;

(defun treesit-fold-indicators-click-fringe (event)
  "EVENT click on fringe."
  (interactive "e")
  (let ((current-fringe (nth 1 (car (cdr event)))) ovs ov cur-ln)
    (when (eq current-fringe treesit-fold-indicators-fringe)
      (mouse-set-point event)
      (beginning-of-line)
      (setq cur-ln (line-number-at-pos (point)))
      (setq ovs (append (treesit-fold--overlays-in 'type 'treesit-fold-indicators-fr-plus)
                        (treesit-fold--overlays-in 'type 'treesit-fold-indicators-fr-minus-tail)))
      (when ovs
        (setq ov (cl-some
                  (lambda (ov) (= cur-ln (line-number-at-pos (overlay-start ov))))
                  ovs))
        (when ov
          (or (save-excursion
                (end-of-line)
                (when (nth 4 (syntax-ppss)) (back-to-indentation))
                (treesit-fold-toggle))
              (treesit-fold-toggle)))))))

;;
;; (@* "Core" )
;;

(defun treesit-fold-indicators--create-overlay-at-point ()
  "Create indicator overlay at current point."
  (let* ((pos (line-beginning-position))
         (ov (make-overlay pos (1+ pos)))
         (window (selected-window)))
    (overlay-put ov 'treesit-fold-indicators-window window)
    (overlay-put ov 'window window)
    ov))

(defun treesit-fold-indicators--create-overlays (beg end folded)
  "Create indicators overlays in range of BEG to END.

If argument FOLDED is non-nil, means the region is close/hidden (overlay
is created); this is used to determie what indicators' bitmap to use."
  (let (ov-lst)
    (save-excursion
      (goto-char beg)
      (while (and (<= (line-beginning-position) end) (not (eobp)))
        (push (treesit-fold-indicators--create-overlay-at-point) ov-lst)
        (forward-line 1)))
    (treesit-fold-indicators--update-overlays (reverse ov-lst) folded)))

(defun treesit-fold-indicators--get-priority (bitmap)
  "Return the priority integer depends on the type of the BITMAP.

This is a static/constant method."
  (let ((prior treesit-fold-indicators-priority))
    (cl-case bitmap
      (treesit-fold-indicators-fr-plus (+ prior 2))
      (treesit-fold-indicators-fr-minus-tail (+ prior 2))
      (treesit-fold-indicators-fr-end-left (+ prior 1))
      (treesit-fold-indicators-fr-end-right (+ prior 1))
      (t prior))))

(defun treesit-fold-indicators--get-string (folded ov bitmap)
  "Return a string or nil for indicators overlay (OV).

If argument FOLDED is nil, it must return a string so all indicators are shown
in range.  Otherwise, we should only return string only when BITMAP is the
head (first line) of the region."
  (let* ((face (or (and (functionp treesit-fold-indicators-face-function)
                        (funcall treesit-fold-indicators-face-function (overlay-start ov)))
                   'treesit-fold-fringe-face))
         (str (propertize "." 'display `(,treesit-fold-indicators-fringe ,bitmap ,face))))
    (if (not folded) str
      (cl-case bitmap
        (treesit-fold-indicators-fr-plus str)  ; return string only in head
        (treesit-fold-indicators-fr-minus-tail nil)
        (treesit-fold-indicators-fr-end-left nil)
        (treesit-fold-indicators-fr-end-right nil)
        (t nil)))))

(defun treesit-fold-indicators--active-ov (folded ov bitmap)
  "SHOW the indicator OV with BITMAP.

Argument FOLDED holds folding state; it's a boolean."
  (when (overlayp ov)
    (overlay-put ov 'treesit-fold-indicators-active folded)
    (overlay-put ov 'type bitmap)
    (overlay-put ov 'priority (treesit-fold-indicators--get-priority bitmap))
    (overlay-put ov 'before-string (treesit-fold-indicators--get-string folded ov bitmap))))

(defun treesit-fold-indicators--get-end-fringe ()
  "Return end fringe bitmap according to variable `treesit-fold-indicators-fringe'."
  (cl-case treesit-fold-indicators-fringe
    (left-fringe 'treesit-fold-indicators-fr-end-left)
    (right-fringe 'treesit-fold-indicators-fr-end-right)
    (t (user-error "Invalid indicators fringe type: %s" treesit-fold-indicators-fringe))))

(defun treesit-fold-indicators--update-overlays (ov-lst folded)
  "SHOW indicators overlays OV-LST depends on FOLDED."
  (when-let* ((len (length ov-lst))
              ((> len 1))
              (len-1 (1- len))
              (first-ov (nth 0 ov-lst))
              (last-ov (nth len-1 ov-lst))
              (index 1))
    ;; Head
    (treesit-fold-indicators--active-ov
     folded first-ov
     (if folded 'treesit-fold-indicators-fr-plus
       'treesit-fold-indicators-fr-minus-tail))
    ;; Last
    (treesit-fold-indicators--active-ov folded last-ov (treesit-fold-indicators--get-end-fringe))
    ;; In between `head' and `last'
    (while (< index len-1)
      (treesit-fold-indicators--active-ov folded (nth index ov-lst) 'treesit-fold-indicators-fr-center)
      (cl-incf index)))
  ov-lst)

;;
;; (@* "Update" )
;;

(defvar-local treesit-fold-indicators--render-this-command-p nil
  "Set to non-nil if render current command.")

(defun treesit-fold-indicators--create (node)
  "Create indicators using NODE."
  (when-let* ((range (treesit-fold--get-fold-range node))
              (beg (car range)) (end (cdr range)))
    (let ((folded (treesit-fold-overlay-at node)))
      (treesit-fold-indicators--create-overlays beg end folded))))

(defun treesit-fold-indicators--size-change (&optional frame &rest _)
  "Render indicators for all visible windows from FRAME."
  (treesit-fold--with-no-redisplay
    (dolist (win (window-list frame)) (treesit-fold-indicators--render-window win))))

(defun treesit-fold-indicators--scroll (&optional window &rest _)
  "Render indicators on WINDOW."
  (treesit-fold--with-no-redisplay
    (treesit-fold-indicators--render-window window)))

(defun treesit-fold-indicators--render-buffer ()
  "Render indicators for current buffer."
  (dolist (window (get-buffer-window-list nil nil t))
    (treesit-fold-indicators--render-window window)))

(defun treesit-fold-indicators--render-window (window)
  "Render indicators for WINDOW."
  (treesit-fold--with-selected-window window
    (ignore-errors (treesit-fold-indicators-refresh))))

(defun treesit-fold-indicators--trigger-render (&rest _)
  "Trigger rendering on the next redisplay."
  (setq treesit-fold-indicators--render-this-command-p t))  ; Trigger render at the end.

(defun treesit-fold-indicators--post-command ()
  "Post command."
  (when treesit-fold-indicators--render-this-command-p
    (treesit-fold-indicators-refresh)
    (setq treesit-fold-indicators--render-this-command-p nil)))

(defun treesit-fold-indicators--within-window (node wend wstart)
  "Return nil if NODE is not within the current window display range.

Arguments WEND and WSTART are the range for caching."
  (when-let*
      ((range (cl-case treesit-fold-indicators-render-method
                (`full
                 (ignore-errors (treesit-fold--get-fold-range node)))
                (`partial (cons (treesit-node-start node)
                                (treesit-node-end node)))
                (t
                 (user-error "Invalid render method: %s" treesit-fold-indicators-render-method))))
       (start (car range))
       (end (cdr range))
       ((or (and (<= wstart start) (<= end wend))    ; with in range
            (and (<= wstart end) (<= start wstart))  ; just one above
            (and (<= wend end) (<= start wend)))))   ; just one below
    node))

;;;###autoload
(defun treesit-fold-indicators-refresh (&rest _)
  "Refresh indicators for all folding range."
  (when (and (ignore-errors (treesit-buffer-root-node)) treesit-fold-indicators-mode)
    (treesit-fold--ensure-ts
      (when-let*
          ((node (ignore-errors (treesit-buffer-root-node)))
           (patterns (seq-mapcat (lambda (fold-range) `((,(car fold-range)) @name))
                                 (alist-get major-mode treesit-fold-range-alist)))
           (query (ignore-errors
                    (treesit-query-compile (treesit-node-language node) patterns)))
           (nodes-to-fold (treesit-query-capture node query))
           (wend (window-end nil t))
           (wstart (window-start))
           (nodes-to-fold
            (cl-remove-if-not (lambda (node)
                                (ignore-errors
                                  (treesit-fold-indicators--within-window (cdr node) wend wstart)))
                              nodes-to-fold))
           (mode-ranges (alist-get major-mode treesit-fold-range-alist))
           (nodes-to-fold
            (cl-remove-if (lambda (node)
                            (treesit-fold--non-foldable-node-p (cdr node) mode-ranges))
                          nodes-to-fold)))
        (treesit-fold-indicators--remove-ovs)
        (thread-last nodes-to-fold
                     (mapcar #'cdr)
                     (mapc #'treesit-fold-indicators--create))
        (run-hooks 'treesit-fold-indicators-refresh-hook)))))

(defun treesit-fold-indicators--remove-ovs (&optional window)
  "Remove all indicators overlays in this WINDOW."
  (remove-overlays (point-min) (point-max) 'treesit-fold-indicators-window
                   (or window (selected-window))))

(defun treesit-fold-indicators--remove-ovs-buffer ()
  "Remove all indicators overlays for this buffer."
  (dolist (window (get-buffer-window-list nil nil t))
    (treesit-fold-indicators--remove-ovs window)))

(provide 'treesit-fold-indicators)
;;; treesit-fold-indicators.el ends here
