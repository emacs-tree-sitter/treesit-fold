;;; treesit-fold-summary.el --- Extract summary from fold region  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026  emacs-tree-sitter maintainers

;; Created date 2021-10-04 16:59:22

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
;; Extract summary from fold region.
;;

;;; Code:

(require 'mule-util)

(require 'treesit-fold-util)

(defcustom treesit-fold-summary-show t
  "Flag to show summary if available."
  :type 'boolean
  :group 'treesit-fold)

(defcustom treesit-fold-summary-max-length 60
  "Maximum length for summary to display."
  :type '(choice (const :tag "nil" nil)
                 (integer :tag "positive integer number"))
  :group 'treesit-fold)

(defcustom treesit-fold-summary-format " <S> %s "
  "Prefix string added before summary overlay."
  :type 'string
  :group 'treesit-fold)

;;
;; (@* "Externals" )
;;

(defvar treesit-fold-replacement-face)

;;
;; (@* "Parsers" )
;;

(defun treesit-fold-summary--valid-content-p (content)
  "Return non-nil if CONTENT is a valid document string for extraction.
Some programmers use some type of characters for splitting the code module
into sections.  For instance, ===, ---, ///, =-=, etc.  Try to omit these
type of content by checking the word boundary's existence."
  (string-match-p "\\w" content))

(defun treesit-fold-summary--apply-sym (line sym)
  "Remove SYM from LINE."
  (when (string-prefix-p sym line)
    (setq line (substring line (length sym) (length line))
          line (string-trim line)))
  line)

(defun treesit-fold-summary--extract-summary (doc-str sym)
  "Extract only document summary from DOC-STR using SYM."
  (let ((lines (split-string doc-str "\n")) new-lines)
    (dolist (line lines)
      (setq line (string-trim line))
      (cond ((listp sym)
             (dolist (c sym) (setq line (treesit-fold-summary--apply-sym line c))))
            (t (setq line (treesit-fold-summary--apply-sym line sym))))
      (when (treesit-fold-summary--valid-content-p line) (push line new-lines)))
    (reverse new-lines)))

(defun treesit-fold-summary--doc-extract (doc-str sym)
  "Default way to extract the doc summary from DOC-STR using SYM."
  (let* ((lines (treesit-fold-summary--extract-summary doc-str sym)) (summary (nth 0 lines)))
    (when summary (setq summary (string-trim summary)))
    (if (string-empty-p summary) nil summary)))

(defun treesit-fold-summary--generic (doc-str sym)
  "Generic DOC-STR extraction using SYM."
  (when (treesit-fold--doc-faces-p doc-str)
    (treesit-fold-summary--doc-extract doc-str sym)))

(defun treesit-fold-summary-batch (doc-str)
  "Extract summary from DOC-STR in Batch."
  (treesit-fold-summary--generic doc-str '("::" "rem" "REM")))

(defun treesit-fold-summary-csharp-vsdoc (doc-str)
  "Extract summary from DOC-STR in C# vsdoc."
  (let ((type-triple (string-match-p "///" doc-str)))
    (setq doc-str (replace-regexp-in-string "<[/]*[^>]+." "" doc-str))
    (treesit-fold-summary--generic doc-str (if type-triple "///" "//"))))

(defun treesit-fold-summary-csharp (doc-str)
  "Extract summary from DOC-STR in C#."
  (cond ((string-match-p "///" doc-str)
         (treesit-fold-summary-csharp-vsdoc doc-str))
        (t (treesit-fold-summary-javadoc doc-str))))

(defun treesit-fold-summary-elisp (doc-str)
  "Extract summary from DOC-STR in Elisp."
  (treesit-fold-summary--generic doc-str ";;"))

(defun treesit-fold-summary-javadoc (doc-str)
  "Extract summary from DOC-STR in Javadoc."
  (treesit-fold-summary--generic doc-str "*"))

(defun treesit-fold-summary-go (doc-str)
  "Extract summary from DOC-STR in Go."
  (treesit-fold-summary--generic doc-str "//"))

(defun treesit-fold-summary-lua-doc (doc-str)
  "Extract summary from DOC-STR in Lua."
  (treesit-fold-summary--generic doc-str "--"))

(defun treesit-fold-summary-pascal-doc (doc-str)
  "Extract summary from DOC-STR in Pascal."
  (cond ((string-prefix-p "{" doc-str)
         (treesit-fold-summary--generic doc-str '("{" "}")))
        (t (treesit-fold-summary-go doc-str))))

(defun treesit-fold-summary-python-doc (doc-str)
  "Extract summary from DOC-STR in Python."
  (treesit-fold-summary--generic doc-str "\"\"\""))

(defun treesit-fold-summary-rst-doc (doc-str)
  "Extract summary from DOC-STR in reStructuredText."
  (treesit-fold-summary--generic doc-str ".."))

(defun treesit-fold-summary-ruby-doc (doc-str)
  "Extract summary from DOC-STR in Ruby."
  (treesit-fold-summary--generic doc-str "#"))

(defun treesit-fold-summary-rust-doc (doc-str)
  "Extract summary from DOC-STR in Rust."
  (treesit-fold-summary--generic doc-str "///"))

(defun treesit-fold-summary-tex-doc (doc-str)
  "Extract summary from DOC-STR in Tex family."
  (treesit-fold-summary--generic doc-str "%"))

(defun treesit-fold-summary-c-macro (doc-str)
  "Parse C macro summary from DOC-STR."
  (when (treesit-fold--is-face doc-str
                               '( font-lock-preprocessor-face
                                  preproc-font-lock-preprocessor-background))
    (treesit-fold-summary--doc-extract doc-str "")))

(defun treesit-fold-summary-c (doc-str)
  "Extract summary from DOC-STR in C comment."
  (or (treesit-fold-summary-javadoc doc-str)
      (treesit-fold-summary-c-macro doc-str)))

(defun treesit-fold-summary-markdown (doc-str)
  "Extract summary from DOC-STR in Markdown block."
  (treesit-fold-summary--doc-extract doc-str '("-" "```")))

(defun treesit-fold-summary-matlab-doc (doc-str)
  "Extract summary from MATLAB DOC-STR."
  (treesit-fold-summary--generic doc-str "%"))

(defun treesit-fold-summary-mermaid (doc-str)
  "Extract summary from DOC-STR in Mermaid comment."
  (treesit-fold-summary--generic doc-str '("%%")))

(defun treesit-fold-summary-org (doc-str)
  "Extract summary from DOC-STR in Org block."
  (treesit-fold-summary--doc-extract doc-str '()))

(defun treesit-fold-summary-vim (doc-str)
  "Extract summary from DOC-STR in Vim."
  (treesit-fold-summary--generic doc-str '("\"")))

(defun treesit-fold-summary-xml (doc-str)
  "Extract summary from DOC-STR in XML."
  (treesit-fold-summary--generic doc-str "-"))

(defun treesit-fold-summary-julia-doc (doc-str)
  "Extract summary from DOC-STR in Julia."
  (treesit-fold-summary--generic doc-str '("#" "\"\"\"")))

(defun treesit-fold-summary-ocaml (doc-str)
  "Extract summary from DOC-STR in OCaml."
  (treesit-fold-summary--generic doc-str '("\"\"")))

;;
;; (@* "Core" )
;;

;; TODO(everyone): keep this alist alphabetically sorted
(defcustom treesit-fold-summary-parsers-alist
  `((actionscript-mode      . treesit-fold-summary-javadoc)
    (arduino-mode           . treesit-fold-summary-c)
    (asm-mode               . treesit-fold-summary-elisp)
    (fasm-mode              . treesit-fold-summary-elisp)
    (masm-mode              . treesit-fold-summary-elisp)
    (nasm-mode              . treesit-fold-summary-elisp)
    (gas-mode               . treesit-fold-summary-elisp)
    (bat-mode               . treesit-fold-summary-batch)
    (beancount-mode         . treesit-fold-summary-elisp)
    (c-mode                 . treesit-fold-summary-c)
    (c++-mode               . treesit-fold-summary-c)
    (caml-mode              . treesit-fold-summary-ocaml)
    (cmake-mode             . treesit-fold-summary-ruby-doc)
    (clojure-mode           . treesit-fold-summary-elisp)
    (clojure-ts-mode        . treesit-fold-summary-elisp)
    (csharp-mode            . treesit-fold-summary-csharp)
    (csharp-ts-mode         . treesit-fold-summary-csharp)
    (css-mode               . treesit-fold-summary-javadoc)
    (css-ts-mode            . treesit-fold-summary-javadoc)
    (dart-mode              . treesit-fold-summary-javadoc)
    (dart-ts-mode           . treesit-fold-summary-javadoc)
    (editorconfig-conf-mode . treesit-fold-summary-ruby-doc)
    (emacs-lisp-mode        . treesit-fold-summary-elisp)
    (elixir-mode            . treesit-fold-summary-ruby-doc)
    (erlang-mode            . treesit-fold-summary-tex-doc)
    (fennel-mode            . treesit-fold-summary-elisp)
    (fennel-ts-mode         . treesit-fold-summary-elisp)
    (fish-mode              . treesit-fold-summary-javadoc)
    (fsharp-mode            . treesit-fold-summary-ocaml)
    (gdscript-mode          . treesit-fold-summary-ruby-doc)
    (gitconfig-mode         . treesit-fold-summary-ruby-doc)
    (gdscript-ts-mode       . treesit-fold-summary-ruby-doc)
    (glsl-mode              . treesit-fold-summary-c)
    (go-mode                . treesit-fold-summary-go)
    (go-ts-mode             . treesit-fold-summary-go)
    (graphql-mode           . treesit-fold-summary-ruby-doc)
    (groovy-mode            . treesit-fold-summary-javadoc)
    (jenkinsfile-mode       . treesit-fold-summary-javadoc)
    (haskell-mode           . treesit-fold-summary-lua-doc)
    (haskell-ts-mode        . treesit-fold-summary-lua-doc)
    (haxe-mode              . treesit-fold-summary-javadoc)
    (heex-mode              . treesit-fold-summary-xml)
    (hlsl-mode              . treesit-fold-summary-c)
    (html-mode              . treesit-fold-summary-xml)
    (jai-mode               . treesit-fold-summary-c)
    (jai-ts-mode            . treesit-fold-summary-c)
    (janet-mode             . treesit-fold-summary-ruby-doc)
    (janet-ts-mode          . treesit-fold-summary-ruby-doc)
    (java-mode              . treesit-fold-summary-javadoc)
    (java-ts-mode           . treesit-fold-summary-javadoc)
    (javascript-mode        . treesit-fold-summary-javadoc)
    (js-ts-mode             . treesit-fold-summary-javadoc)
    (js-mode                . treesit-fold-summary-javadoc)
    (js2-mode               . treesit-fold-summary-javadoc)
    (js3-mode               . treesit-fold-summary-javadoc)
    (json-mode              . treesit-fold-summary-javadoc)
    (json-ts-mode           . treesit-fold-summary-javadoc)
    (jsonnet-mode           . treesit-fold-summary-javadoc)
    (jsonnet-ts-mode        . treesit-fold-summary-javadoc)
    (julia-mode             . treesit-fold-summary-julia-doc)
    (julia-ts-mode          . treesit-fold-summary-julia-doc)
    (kotlin-mode            . treesit-fold-summary-javadoc)
    (kotlin-ts-mode         . treesit-fold-summary-javadoc)
    (latex-mode             . treesit-fold-summary-tex-doc)
    (LaTeX-mode             . treesit-fold-summary-tex-doc)
    (lisp-mode              . treesit-fold-summary-elisp)
    (lisp-interaction-mode  . treesit-fold-summary-elisp)
    (llvm-mode              . treesit-fold-summary-elisp)
    (llvm-mir-mode          . treesit-fold-summary-elisp)
    (lua-mode               . treesit-fold-summary-lua-doc)
    (lua-ts-mode            . treesit-fold-summary-lua-doc)
    (magik-mode             . treesit-fold-summary-ruby-doc)
    (makefile-mode          . treesit-fold-summary-ruby-doc)
    (makefile-automake-mode . treesit-fold-summary-ruby-doc)
    (makefile-gmake-mode    . treesit-fold-summary-ruby-doc)
    (makefile-makepp-mode   . treesit-fold-summary-ruby-doc)
    (makefile-bsdmake-mode  . treesit-fold-summary-ruby-doc)
    (makefile-imake-mode    . treesit-fold-summary-ruby-doc)
    (markdown-mode          . treesit-fold-summary-markdown)
    (matlab-mode            . treesit-fold-summary-matlab-doc)
    (mermaid-mode           . treesit-fold-summary-mermaid)
    (ninja-mode             . treesit-fold-summary-ruby-doc)
    (nim-mode               . treesit-fold-summary-ruby-doc)
    (nim-ts-mode            . treesit-fold-summary-ruby-doc)
    (nix-mode               . treesit-fold-summary-ruby-doc)
    (nix-ts-mode            . treesit-fold-summary-ruby-doc)
    (noir-mode              . treesit-fold-summary-rust-doc)
    (noir-ts-mode              . treesit-fold-summary-rust-doc)
    (objc-mode              . treesit-fold-summary-c)
    (org-mode               . treesit-fold-summary-org)
    (perl-mode              . treesit-fold-summary-ruby-doc)
    (php-mode               . treesit-fold-summary-javadoc)
    (pascal-mode            . treesit-fold-summary-pascal-doc)
    (python-mode            . treesit-fold-summary-python-doc)
    (qss-mode               . treesit-fold-summary-css)
    (rjsx-mode              . treesit-fold-summary-javadoc)
    (ron-mode               . treesit-fold-summary-rust-doc)
    (rst-mode               . treesit-fold-summary-rst-doc)
    (ruby-mode              . treesit-fold-summary-ruby-doc)
    (rust-mode              . treesit-fold-summary-rust-doc)
    (scala-mode             . treesit-fold-summary-javadoc)
    (scheme-mode            . treesit-fold-summary-elisp)
    (sh-mode                . treesit-fold-summary-javadoc)
    (sh-ts-mode             . treesit-fold-summary-javadoc)
    (sql-mode               . treesit-fold-summary-c)
    (sql-ts-mode            . treesit-fold-summary-c)
    (svelte-mode            . treesit-fold-summary-xml)
    (svelte-ts-mode         . treesit-fold-summary-xml)
    (swift-mode             . treesit-fold-summary-c)
    (swift-ts-mode          . treesit-fold-summary-c)
    (tablegen-mode          . treesit-fold-summary-javadoc)
    (toml-mode              . treesit-fold-summary-javadoc)
    (toml-ts-mode           . treesit-fold-summary-javadoc)
    (conf-toml-mode         . treesit-fold-summary-javadoc)
    (typescript-mode        . treesit-fold-summary-javadoc)
    (typescript-ts-mode     . treesit-fold-summary-javadoc)
    (verilog-mode           . treesit-fold-summary-javadoc)
    (verilog-ts-mode        . treesit-fold-summary-javadoc)
    (vhdl-mode              . treesit-fold-summary-lua-doc)
    (vhdl-ts-mode           . treesit-fold-summary-lua-doc)
    (vimrc-mode             . treesit-fold-summary-vim)
    (vimscript-ts-mode      . treesit-fold-summary-vim)
    (nxml-mode              . treesit-fold-summary-xml)
    (yaml-mode              . treesit-fold-summary-ruby-doc)
    (yaml-ts-mode           . treesit-fold-summary-ruby-doc)
    (k8s-mode               . treesit-fold-summary-ruby-doc)
    (zig-mode               . treesit-fold-summary-go)
    (zig-ts-mode            . treesit-fold-summary-go))
  "Alist mapping `major-mode' to doc parser function."
  :type '(alist :key-type symbol :value-type function)
  :group 'treesit-fold)

(defun treesit-fold-summary--keep-length (summary)
  "Keep the SUMMARY length to `treesit-fold-summary-max-length'."
  (let ((len-sum (length summary)))
    (when (< treesit-fold-summary-max-length len-sum)
      (setq summary (truncate-string-to-width summary
                                              treesit-fold-summary-max-length
                                              0 nil
                                              t))))
  summary)

(defun treesit-fold-summary--apply-format (summary)
  "Return the SUMMARY that has added the summary prefix."
  (format treesit-fold-summary-format summary))

(defun treesit-fold-summary--parser ()
  "Return the summary parser from `treesit-fold-summary-parsers-alist'."
  (assoc (buffer-local-value 'major-mode (current-buffer)) treesit-fold-summary-parsers-alist))

(defun treesit-fold-summary--get (doc-str)
  "Extract summary from DOC-STR in order to display ontop of the overlay."
  (let ((parser (cdr (treesit-fold-summary--parser))) summary
        (map (make-sparse-keymap)))
    (keymap-set map "<mouse-1>" #'treesit-fold-open)
    (when parser
      (setq summary (funcall parser doc-str))
      (when (integerp treesit-fold-summary-max-length)
        (setq summary (treesit-fold-summary--keep-length summary)))
      (when summary
        (setq summary (treesit-fold-summary--apply-format summary)
              summary (propertize summary 'face 'treesit-fold-replacement-face
                                  'mouse-face 'treesit-fold-replacement-mouse-face
                                  'keymap map))))
    summary))

(provide 'treesit-fold-summary)
;;; treesit-fold-summary.el ends here
