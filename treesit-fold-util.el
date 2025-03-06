;;; treesit-fold-util.el --- Utility module  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025  emacs-tree-sitter maintainers

;; Created date 2021-10-04 20:19:42

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
;; Utility module.
;;

;;; Code:

;;
;; (@* "Redisplay" )
;;

(defmacro treesit-fold--with-no-redisplay (&rest body)
  "Execute BODY without any redisplay execution."
  (declare (indent 0) (debug t))
  `(let ((inhibit-redisplay t)
         (inhibit-modification-hooks t)
         after-focus-change-function
         buffer-list-update-hook
         display-buffer-alist
         window-configuration-change-hook
         window-scroll-functions
         window-size-change-functions
         window-state-change-hook
         jit-lock-mode)
     ,@body))

;;
;; (@* "String" )
;;

(defun treesit-fold-2str (obj)
  "Convert OBJ to string."
  (format "%s" obj))

(defun treesit-fold--count-matches (pattern str)
  "Count occurrences of PATTERN in STR.

Like function `s-count-matches' but faster."
  (max 0 (1- (length (split-string str pattern)))))

;;
;; (@* "Cons" )
;;

(defun treesit-fold--cons-add (&rest args)
  "Addition for list of cons ARGS."
  (let ((v1 0) (v2 0))
    (dolist (c args)
      (setq v1 (+ v1 (car c))
            v2 (+ v2 (cdr c))))
    (cons v1 v2)))

;;
;; (@* "Overlay" )
;;

(defun treesit-fold--overlays-in (prop name &optional beg end)
  "Return overlays with PROP of NAME, from region BEG to END."
  (unless beg (setq beg (point-min))) (unless end (setq end (point-max)))
  (let ((lst '()) (ovs (overlays-in beg end)))
    (dolist (ov ovs)
      (when (eq name (overlay-get ov prop))
        (push ov lst)))
    lst))

;;
;; (@* "Face" )
;;

(defvar treesit-fold--doc-faces
  '( font-lock-doc-face
     font-lock-comment-face
     font-lock-comment-delimiter-face
     tree-sitter-hl-face:comment
     tree-sitter-hl-face:doc
     hl-todo
     rst-comment)
  "List of face that apply for document string.")

(defun treesit-fold--get-face (obj trim)
  "Return face name from OBJ.
If argument TRIM is non-nil, trim the OBJ."
  (let* ((obj (if trim (string-trim obj) obj))
         (len (length obj)))
    (or (get-text-property 0 'face obj)
        (and (<= 1 len)
             (get-text-property 1 'face obj)))))

(defun treesit-fold--is-face (obj lst-face &optional trim)
  "Return non-nil if OBJ's face is define inside list LST-FACE.
Optional argument TRIM, see function `treesit-fold--get-face'."
  (unless (listp lst-face) (setq lst-face (list lst-face)))
  (let ((faces (treesit-fold--get-face obj trim)))
    (cond ((listp faces)
           (cl-some (lambda (face) (memq face lst-face)) faces))
          (t (memq faces lst-face)))))

(defun treesit-fold--doc-faces-p (obj &optional trim)
  "Return non-nil if face at OBJ is within `treesit-fold--doc-faces' list.
Optional argument TRIM, see function `treesit-fold--get-face'."
  (treesit-fold--is-face obj treesit-fold--doc-faces trim))

;;
;; (@* "Positions" )
;;

(defun treesit-fold--last-eol (pos)
  "Go to POS then find previous line break, and return its position."
  (save-excursion
    (goto-char pos)
    (max 1 (1- (line-beginning-position)))))

(defun treesit-fold--bol (point)
  "Return line beginning position at POINT."
  (save-excursion (goto-char point) (line-beginning-position)))

(defun treesit-fold--eol (point)
  "Return line end position at POINT."
  (save-excursion (goto-char point) (line-end-position)))

(defun treesit-fold--indentation (pos)
  "Return current indentation by POS."
  (goto-char pos)
  (current-indentation))

(defun treesit-fold--node-start-point (node)
  "Return NODE's start point."
  (save-excursion
    (goto-char (treesit-node-start node))
    (cons (line-number-at-pos) (current-column))))

(defun treesit-fold--node-end-point (node)
  "Return NODE's end point."
  (save-excursion
    (goto-char (treesit-node-end node))
    (cons (line-number-at-pos) (current-column))))

;;
;; (@* "Math" )
;;

(defun treesit-fold--in-range-p (in-val in-min in-max)
  "Check to see if IN-VAL is between IN-MIN and IN-MAX."
  (and (<= in-min in-val) (<= in-val in-max)))

;;
;; (@* "List" )
;;

(defun treesit-fold-listify (obj)
  "Ensure OBJ is a list."
  (if (listp obj) obj (list obj)))

;;
;; (@* "Window" )
;;

(defmacro treesit-fold--with-selected-window (window &rest body)
  "Same with `with-selected-window' but safe.

See macro `with-selected-window' description for arguments WINDOW and BODY."
  (declare (indent 1) (debug t))
  `(when (window-live-p ,window) (with-selected-window ,window ,@body)))

;;
;; (@* "TS node" )
;;

(defun treesit-fold--compare-type (node type)
  "Compare NODE's type to TYPE."
  (string= (treesit-node-type node) type))

(defun treesit-fold-find-children (node type)
  "Search through the children of NODE to find all with type equal to TYPE;
then return that list."
  (cl-remove-if-not (lambda (child) (treesit-fold--compare-type child type))
                    (treesit-node-children node)))

(defun treesit-fold-find-parent (node type)
  "Find the TYPE of parent from NODE."
  (let ((parent (treesit-node-parent node))
        (break))
    (while (and parent (not break))
      (setq break (treesit-fold--compare-type parent type))
      (unless break
        (setq parent (treesit-node-parent parent))))
    parent))

(defun treesit-fold-last-child (node)
  "Return last child node from parent NODE."
  (when-let* ((count (treesit-node-child-count node))
              ((not (= count 0))))
    (treesit-node-child node (1- count))))

(provide 'treesit-fold-util)
;;; treesit-fold-util.el ends here
