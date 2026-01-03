;;; treesit-fold-parsers.el --- Adapter layer to Tree-Sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2026  emacs-tree-sitter maintainers

;; Created date 2021-10-04 17:45:48

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
;; Adapter layer to Tree-Sitter
;;
;; This isn't a real parser implementation, but records down the rule
;; in order to let the Tree-Sitter to parse things correctly.  Think of
;; the rule sets!
;;

;;; Code:

;;
;; (@* "Externals" )
;;

;; TODO(everyone): keep the forward declared alphabetically sorted

(declare-function treesit-fold-range-seq "treesit-fold.el")
(declare-function treesit-fold-range-line-comment "treesit-fold.el")
(declare-function treesit-fold-range-block-comment "treesit-fold.el")
(declare-function treesit-fold-range-c-like-comment "treesit-fold.el")

(declare-function treesit-fold-range-asm-label "treesit-fold.el")
(declare-function treesit-fold-range-beancount-transaction "treesit-fold.el")
(declare-function treesit-fold-range-c-preproc-ifdef "treesit-fold.el")
(declare-function treesit-fold-range-c-preproc-if "treesit-fold.el")
(declare-function treesit-fold-range-c-preproc-elif "treesit-fold.el")
(declare-function treesit-fold-range-c-preproc-else "treesit-fold.el")
(declare-function treesit-fold-range-elisp-function "treesit-fold.el")
(declare-function treesit-fold-range-elixir "treesit-fold.el")
(declare-function treesit-fold-range-erlang-clause-body "treesit-fold.el")
(declare-function treesit-fold-range-erlang-type-guards "treesit-fold.el")
(declare-function treesit-fold-range-fish-function "treesit-fold.el")
(declare-function treesit-fold-range-fish-if "treesit-fold.el")
(declare-function treesit-fold-range-fish-case "treesit-fold.el")
(declare-function treesit-fold-range-fsharp-module-defn "treesit-fold.el")
(declare-function treesit-fold-range-git-config-section "treesit-fold.el")
(declare-function treesit-fold-range-fsharp-record-type-defn "treesit-fold.el")
(declare-function treesit-fold-range-haskell-function "treesit-fold.el")
(declare-function treesit-fold-range-html "treesit-fold.el")
(declare-function treesit-fold-range-julia-function "treesit-fold.el")
(declare-function treesit-fold-range-julia-if "treesit-fold.el")
(declare-function treesit-fold-range-julia-let "treesit-fold.el")
(declare-function treesit-fold-range-kotlin-when "treesit-fold.el")
(declare-function treesit-fold-range-latex-environment "treesit-fold.el")
(declare-function treesit-fold-range-latex-section "treesit-fold.el")
(declare-function treesit-fold-range-lisp-function "treesit-fold.el")
(declare-function treesit-fold-range-llvm-label "treesit-fold.el")
(declare-function treesit-fold-range-llvm-mir-label "treesit-fold.el")
(declare-function treesit-fold-range-lua-comment "treesit-fold.el")
(declare-function treesit-fold-range-lua-function "treesit-fold.el")
(declare-function treesit-fold-range-lua-if "treesit-fold.el")
(declare-function treesit-fold-range-lua-elseif "treesit-fold.el")
(declare-function treesit-fold-range-lua-else "treesit-fold.el")
(declare-function treesit-fold-range-lua-do-loop "treesit-fold.el")
(declare-function treesit-fold-range-lua-repeat "treesit-fold.el")
(declare-function treesit-fold-range-make-recipe "treesit-fold.el")
(declare-function treesit-fold-range-markdown-heading "treesit-fold.el")
(declare-function treesit-fold-range-markdown-code-block "treesit-fold.el")
(declare-function treesit-fold-range-markdown-html-block "treesit-fold.el")
(declare-function treesit-fold-range-matlab-function "treesit-fold.el")
(declare-function treesit-fold-range-matlab-statements "treesit-fold.el")
(declare-function treesit-fold-range-matlab-blocks "treesit-fold.el")
(declare-function treesit-fold-range-mermaid-diagram "treesit-fold.el")
(declare-function treesit-fold-range-mermaid-block "treesit-fold.el")
(declare-function treesit-fold-range-ocaml-comment "treesit-fold.el")
(declare-function treesit-fold-range-ocaml-module-definition "treesit-fold.el")
(declare-function treesit-fold-range-ocaml-type-definition "treesit-fold.el")
(declare-function treesit-fold-range-ocaml-value-definition "treesit-fold.el")
(declare-function treesit-fold-range-org-body "treesit-fold.el")
(declare-function treesit-fold-range-clojure-function "treesit-fold.el")
(declare-function treesit-fold-range-cmake-body "treesit-fold.el")
(declare-function treesit-fold-range-editorconfig-section "treesit-fold.el")
(declare-function treesit-fold-range-pascal-comment "treesit-fold.el")
(declare-function treesit-fold-range-python-block "treesit-fold.el")
(declare-function treesit-fold-range-python-def "treesit-fold.el")
(declare-function treesit-fold-range-python-expression-statement "treesit-fold.el")
(declare-function treesit-fold-range-ron-struct "treesit-fold.el")
(declare-function treesit-fold-range-rst-body "treesit-fold.el")
(declare-function treesit-fold-range-ruby-class-def "treesit-fold.el")
(declare-function treesit-fold-range-ruby-if "treesit-fold.el")
(declare-function treesit-fold-range-rust-macro "treesit-fold.el")
(declare-function treesit-fold-range-sql-block "treesit-fold.el")
(declare-function treesit-fold-range-toml-table "treesit-fold.el")
(declare-function treesit-fold-range-verilog-list "treesit-fold.el")
(declare-function treesit-fold-range-verilog-initial-construct "treesit-fold.el")
(declare-function treesit-fold-range-verilog-module "treesit-fold.el")
(declare-function treesit-fold-range-vhdl-package "treesit-fold.el")
(declare-function treesit-fold-range-vim-for-loop "treesit-fold.el")
(declare-function treesit-fold-range-vhdl-type "treesit-fold.el")

;;
;; (@* "Parsers" )
;;

;; Should these functions be variables?
;; TODO(everyone): keep the function alphabetically sorted

(defun treesit-fold-parsers-actionscript ()
  "Rule set for ActionScript."
  '((statement_block . treesit-fold-range-seq)
    (line_comment    . treesit-fold-range-c-like-comment)
    (block_comment   . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-agda ()
  "Rule set for Agda."
  '(()))

(defun treesit-fold-parsers-arduino ()
  "Rule set for Arduino."
  (append (treesit-fold-parsers-c++)))

(defun treesit-fold-parsers-asm ()
  "Rule set for Assembly."
  '((label         . treesit-fold-range-asm-label)
    (block_comment . treesit-fold-range-c-like-comment)
    (line_comment
     . (lambda (node offset)
         (let ((text (treesit-node-text node)))
           (cond ((string-prefix-p ";;" text)
                  (treesit-fold-range-line-comment node offset ";;"))
                 ((string-prefix-p "#" text)
                  (treesit-fold-range-line-comment node offset "#"))
                 (t
                  (treesit-fold-range-c-like-comment node offset))))))))

(defun treesit-fold-parsers-awk ()
  "Rule set for Awk."
  ;; TODO: Complete this function
  (append (treesit-fold-parsers-c)))

(defun treesit-fold-parsers-bash ()
  "Rule set for Bash."
  '((compound_statement . treesit-fold-range-seq)
    (do_group           . (treesit-fold-range-seq 1 -3))
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-beancount ()
  "Rule set for Beancount."
  '((transaction . treesit-fold-range-beancount-transaction)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset ";;")))))

(defun treesit-fold-parsers-c ()
  "Rule set for C."
  '((compound_statement     . treesit-fold-range-seq)
    (declaration_list       . treesit-fold-range-seq)
    (enumerator_list        . treesit-fold-range-seq)
    (field_declaration_list . treesit-fold-range-seq)
    (preproc_if             . treesit-fold-range-c-preproc-if)
    (preproc_ifdef          . treesit-fold-range-c-preproc-ifdef)
    (preproc_elif           . treesit-fold-range-c-preproc-elif)
    (preproc_else           . treesit-fold-range-c-preproc-else)
    (comment                . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-c++ ()
  "Rule set for C++."
  (append (treesit-fold-parsers-c)))

(defun treesit-fold-parsers-clojure ()
  "Rule set for Clojure."
  '((list_lit . treesit-fold-range-clojure-function)
    (map_lit  . treesit-fold-range-seq)
    (str_lit  . treesit-fold-range-seq)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node
                                          (treesit-fold--cons-add offset '(0 . -1))
                                          ";;")))))

(defun treesit-fold-parsers-cmake ()
  "Rule set for CMake."
  '((body . treesit-fold-range-cmake-body)
    (line_comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-csharp ()
  "Rule set for C#."
  '((block                                . treesit-fold-range-seq)
    (accessor_list                        . treesit-fold-range-seq)
    (enum_member_declaration_list         . treesit-fold-range-seq)
    (declaration_list                     . treesit-fold-range-seq)
    (switch_body                          . treesit-fold-range-seq)
    (anonymous_object_creation_expression . treesit-fold-range-seq)
    (initializer_expression               . treesit-fold-range-seq)
    ;;(if_directive                         . treesit-fold-range-seq)
    ;;(else_directive                       . treesit-fold-range-seq)
    ;;(elif_directive                       . treesit-fold-range-seq)
    ;;(endif_directive                      . treesit-fold-range-seq)
    ;;(region_directive                     . treesit-fold-range-seq)
    ;;(endregion_directive                  . treesit-fold-range-seq)
    (comment                              . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-css ()
  "Rule set for CSS."
  '((keyframe_block_list . treesit-fold-range-seq)
    (block               . treesit-fold-range-seq)
    (comment             . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-dart ()
  "Rule set for Dart."
  '((block                 . treesit-fold-range-seq)
    (class_body            . treesit-fold-range-seq)
    (arguments             . treesit-fold-range-seq)
    (comment               . treesit-fold-range-c-like-comment)
    (documentation_comment . treesit-fold-range-c-like-comment)
    (list_literal          . treesit-fold-range-seq)))  ; array

(defun treesit-fold-parsers-editorconfig ()
  "Rule set for EditorConfig."
  '((section . treesit-fold-range-editorconfig-section)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node
                                          (treesit-fold--cons-add offset '(0 . -1))
                                          "#")))))

(defun treesit-fold-parsers-elisp ()
  "Rule set for Elisp."
  '((macro_definition    . treesit-fold-range-elisp-function)
    (function_definition . treesit-fold-range-elisp-function)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset ";;")))))

(defun treesit-fold-parsers-elixir ()
  "Rules set for Elixir."
  '((list     . treesit-fold-range-seq)
    (map      . treesit-fold-range-seq)
    (tuple    . treesit-fold-range-seq)
    (do_block . treesit-fold-range-elixir)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-erlang ()
  "Rules set for Erlang."
  '((list        . treesit-fold-range-seq)
    (clause_body . treesit-fold-range-erlang-clause-body)
    (type_guards . treesit-fold-range-erlang-type-guards)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "%")))))

(defun treesit-fold-parsers-fennel ()
  "Rules set for Fennel."
  '((macro_form . treesit-fold-range-elisp-function)
    (fn_form    . treesit-fold-range-elisp-function)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset ";;")))))

(defun treesit-fold-parsers-fish ()
  "Rules set for Fish."
  '((function_definition . treesit-fold-range-fish-function)
    (if_statement        . treesit-fold-range-fish-if)
    (switch_statement    . treesit-fold-range-fish-if)
    (for_statement       . treesit-fold-range-fish-if)
    (while_statement     . treesit-fold-range-fish-if)
    (case_clause         . treesit-fold-range-fish-case)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-fsharp ()
  "Rules set for F#."
  '((module_defn      . treesit-fold-range-fsharp-module-defn)
    (list_expression  . treesit-fold-range-seq)
    (record_type_defn . treesit-fold-range-fsharp-record-type-defn)
    (line_comment     . treesit-fold-range-c-like-comment)
    (block_comment    . (treesit-fold-range-seq 1 -1))))

(defun treesit-fold-parsers-gdscript ()
  "Rule set for GGScript."
  '((body . (treesit-fold-range-seq -1 1))
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-git-config ()
  "Rule set for Git config."
  '((section . treesit-fold-range-git-config-section)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-gleam ()
  "Rules set for Gleam."
  '((function           . treesit-fold-range-gleam)
    (type_definition    . treesit-fold-range-gleam)
    (anonymous_function . treesit-fold-range-gleam)
    (block              . treesit-fold-range-gleam)
    (list               . treesit-fold-range-seq)
    (module_comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "////")))
    (statement_comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "///")))))

(defun treesit-fold-parsers-glsl ()
  "Rule set for GLSL."
  '((field_declaration_list . treesit-fold-range-seq)
    (compound_statement     . treesit-fold-range-seq)
    (comment                . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-go ()
  "Rule set for Go."
  '((block                  . treesit-fold-range-seq)
    (comment                . treesit-fold-range-c-like-comment)
    (const_declaration      . (lambda (node offset)
                                (treesit-fold-range-markers node offset "(" ")")))
    (field_declaration_list . treesit-fold-range-seq)
    (import_spec_list       . treesit-fold-range-seq)
    (interface_type         . (lambda (node offset)
                                (treesit-fold-range-markers node offset "{" "}")))))

(defun treesit-fold-parsers-graphql ()
  "Rule set for GraphQL."
  '((fields_definition . treesit-fold-range-seq)
    (list_type         . treesit-fold-range-seq)))

(defun treesit-fold-parsers-groovy ()
  "Rule set for Groovy."
  '((block         . treesit-fold-range-groovy-block)
    (line_comment  . treesit-fold-range-c-like-comment)
    (block_comment . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-haskell ()
  "Rule set for Haskell."
  '((function . treesit-fold-range-haskell-function)
    (comment  . treesit-fold-range-lua-comment)))

(defun treesit-fold-parsers-haxe ()
  "Rule set for Haxe."
  '((block    . treesit-fold-range-seq)
    (comment  . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-heex ()
  "Rule set for Heex."
  '((tag       . treesit-fold-range-html)
    (component . treesit-fold-range-html)
    (comment   . (treesit-fold-range-seq 1 -1))))

(defun treesit-fold-parsers-hlsl ()
  "Rule set for HLSL."
  '((field_declaration_list . treesit-fold-range-seq)
    (compound_statement     . treesit-fold-range-seq)
    (comment                . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-html ()
  "Rule set for HTML."
  '((element . treesit-fold-range-html)
    (comment . (treesit-fold-range-seq 1 -1))))

(defun treesit-fold-parsers-jai ()
  "Rule set for Jai."
  '((imperative_scope . treesit-fold-range-seq)
    (data_scope       . treesit-fold-range-seq)
    (block_comment    . treesit-fold-range-block-comment)
    (inline_comment   . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-janet ()
  "Rule set for Janet."
  '((par_tup_lit . treesit-fold-range-seq)
    (sqr_tup_lit . treesit-fold-range-seq)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-java ()
  "Rule set for Java."
  '((switch_block                    . treesit-fold-range-seq)
    (block                           . treesit-fold-range-seq)
    (element_value_array_initializer . treesit-fold-range-seq)
    (module_body                     . treesit-fold-range-seq)
    (enum_body                       . treesit-fold-range-seq)
    (class_body                      . treesit-fold-range-seq)
    (constructor_body                . treesit-fold-range-seq)
    (annotation_type_body            . treesit-fold-range-seq)
    (interface_body                  . treesit-fold-range-seq)
    (array_initializer               . treesit-fold-range-seq)
    (block_comment                   . treesit-fold-range-block-comment)
    (line_comment                    . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-javascript ()
  "Rule set for JavaScript."
  '((export_clause   . treesit-fold-range-seq)
    (statement_block . treesit-fold-range-seq)
    (object          . treesit-fold-range-seq)
    (array           . treesit-fold-range-seq)
    (comment         . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-json ()
  "Rule set for JSON."
  '((object  . treesit-fold-range-seq)
    (array   . treesit-fold-range-seq)
    (comment . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-jsonnet ()
  "Rule set for Jsonnet."
  '((object  . treesit-fold-range-seq)
    (array   . treesit-fold-range-seq)
    (comment . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-julia ()
  "Rule set for Julia."
  '((block_comment       . (treesit-fold-range-seq 1 -1))
    (for_statement       . treesit-fold-range-julia-if)
    (function_definition . treesit-fold-range-julia-function)
    (if_statement        . treesit-fold-range-julia-if)
    (let_statement       . treesit-fold-range-julia-let)
    (macro_definition    . treesit-fold-range-julia-function)
    (module_definition   . treesit-fold-range-julia-function)
    (quote_statement     . treesit-fold-range-julia-function)
    (struct_definition   . treesit-fold-range-julia-function)
    (triple_string       . (treesit-fold-range-seq 2 -2))
    (try_statement       . (treesit-fold-range-seq 2 -2))
    (while_statement     . treesit-fold-range-julia-function)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-kotlin ()
  "Rule set for Kotlin."
  '((function_body          . treesit-fold-range-seq)
    (control_structure_body . treesit-fold-range-seq)
    (lambda_literal         . treesit-fold-range-seq)
    (enum_class_body        . treesit-fold-range-seq)
    (class_body             . treesit-fold-range-seq)
    (when_expression        . treesit-fold-range-kotlin-when)
    (multiline_comment      . treesit-fold-range-c-like-comment)
    (line_comment           . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-latex ()
  "Rule set for LaTex."
  '((generic_environment . treesit-fold-range-latex-environment)
    (math_environment . treesit-fold-range-latex-environment)
    (section             . treesit-fold-range-latex-section)
    (subsection          . treesit-fold-range-latex-section)
    (subsubsection       . treesit-fold-range-latex-section)
    (curly_group         . treesit-fold-range-seq)
    (line_comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "%")))))

(defun treesit-fold-parsers-lisp ()
  "Rule set for Lisp."
  '((defun . treesit-fold-range-lisp-function)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node
                                          (treesit-fold--cons-add offset '(0 . -1))
                                          ";;")))))

(defun treesit-fold-parsers-llvm ()
  "Rule set for LLVM."
  '((function_body . treesit-fold-range-seq)
    (label         . treesit-fold-range-llvm-label)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset ";;")))))

(defun treesit-fold-parsers-llvm-mir ()
  "Rule set for LLVM MIR."
  '((basic_block . treesit-fold-range-llvm-mir-label)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset ";;")))))

(defun treesit-fold-parsers-lua ()
  "Rule set for Lua."
  '((expression_list      . treesit-fold-range-seq)
    (function_declaration . treesit-fold-range-lua-function)
    (if_statement         . treesit-fold-range-lua-if)
    (elseif_statement     . treesit-fold-range-lua-elseif)
    (else_statement       . treesit-fold-range-lua-else)
    (while_statement      . treesit-fold-range-lua-do-loop)
    (for_statement        . treesit-fold-range-lua-do-loop)
    (repeat_statement     . treesit-fold-range-lua-repeat)
    (comment              . treesit-fold-range-lua-comment)))

(defun treesit-fold-parsers-magik ()
  "Rule set for Magik."
  '((method . treesit-fold-range-seq)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-make ()
  "Rule set for Make."
  '((recipe . treesit-fold-range-make-recipe)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-markdown ()
  "Rule set for Markdown."
  '((fenced_code_block . treesit-fold-range-markdown-code-block)
    (section           . treesit-fold-range-markdown-heading)
    (html_block        . treesit-fold-range-markdown-html-block)))

(defun treesit-fold-parsers-matlab ()
  "Rule set for MATLAB."
  '((expression_list     . treesit-fold-range-seq)
    (function_definition . treesit-fold-range-matlab-function)
    (properties          . treesit-fold-range-matlab-function)
    (methods             . treesit-fold-range-matlab-function)
    (class_definition    . treesit-fold-range-matlab-function)
    (if_statement        . treesit-fold-range-matlab-statements)
    (elseif_clause       . treesit-fold-range-matlab-statements)
    (else_clause         . treesit-fold-range-matlab-statements)
    (for_statement       . treesit-fold-range-matlab-statements)
    (while_statement     . treesit-fold-range-matlab-statements)
    (switch_statement    . treesit-fold-range-matlab-statements)
    (case_clause         . treesit-fold-range-matlab-statements)
    (otherwise_clause    . treesit-fold-range-matlab-statements)
    (try_statement       . treesit-fold-range-matlab-statements)
    (catch_clause        . treesit-fold-range-matlab-statements)
    (comment             . treesit-fold-range-matlab-blocks)))

(defun treesit-fold-parsers-mermaid ()
  "Rule set for Mermaid."
  '((diagram_flow         . treesit-fold-range-mermaid-diagram)
    (diagram_sequence     . treesit-fold-range-mermaid-diagram)
    (diagram_class        . treesit-fold-range-mermaid-diagram)
    (diagram_er           . treesit-fold-range-mermaid-diagram)
    (class_stmt_class     . treesit-fold-range-mermaid-block)
    (er_stmt_entity_block . treesit-fold-range-mermaid-block)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node
                                          (treesit-fold--cons-add offset '(0 . -1))
                                          "%%")))))

(defun treesit-fold-parsers-ninja ()
  "Rule set for Ninja."
  '((build . (treesit-fold-range-seq 4 0))
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node
                                          (treesit-fold--cons-add offset '(0 . -1))
                                          "#")))))

(defun treesit-fold-parsers-noir ()
  "Rule set for Noir."
  '((body    . treesit-fold-range-seq)
    (comment . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-nim ()
  "Rule set for Nim."
  '((array_construction . treesit-fold-range-seq)
    (for                . treesit-fold-range-seq)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-nix ()
  "Rule set for Nix."
  '((attrset_expression . treesit-fold-range-seq)
    (interpolation      . treesit-fold-range-seq)
    (list_expression    . treesit-fold-range-seq)
    (comment
     . (lambda (node offset)
         (if (string-prefix-p "#" (treesit-node-text node))
             (treesit-fold-range-line-comment node offset "#")
           (treesit-fold-range-c-like-comment node offset))))))

(defun treesit-fold-parsers-ocaml ()
  "Rule set for OCaml."
  '((comment             . treesit-fold-range-ocaml-comment)
    (module_definition   . treesit-fold-range-ocaml-module-definition)
    (type_definition     . treesit-fold-range-ocaml-type-definition)
    (value_definition    . treesit-fold-range-ocaml-value-definition)))

(defun treesit-fold-parsers-org ()
  "Rule set for Org."
  '((body    . treesit-fold-range-org-body)
    (block   . treesit-fold-range-seq)
    (comment . treesit-fold-range-seq)))

(defun treesit-fold-parsers-pascal ()
  "Rule set for Pascal."
  '((comment . treesit-fold-range-pascal-comment)))

(defun treesit-fold-parsers-perl ()
  "Rule set for Perl."
  '((block           . treesit-fold-range-seq)
    (list_expression . treesit-fold-range-seq)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-php ()
  "Rule set for PHP."
  '((namespace_use_group . treesit-fold-range-seq)
    (declaration_list    . treesit-fold-range-seq)
    (use_list            . treesit-fold-range-seq)
    (switch_block        . treesit-fold-range-seq)
    (compound_statement  . treesit-fold-range-seq)
    (comment
     . (lambda (node offset)
         (if (string-prefix-p "#" (treesit-node-text node))
             (treesit-fold-range-line-comment node offset "#")
           (treesit-fold-range-c-like-comment node offset))))))

(defun treesit-fold-parsers-python ()
  "Rule set for Python."
  '((import_statement         . treesit-fold-range-seq)
    (import_from_statement    . treesit-fold-range-seq)
    (future_import_statement  . treesit-fold-range-seq)
    (function_definition      . treesit-fold-range-python-def)
    (class_definition         . treesit-fold-range-python-def)
    (while_statement          . treesit-fold-range-python-block)
    (for_statement            . treesit-fold-range-python-block)
    (if_statement             . treesit-fold-range-python-block)
    (elif_clause              . treesit-fold-range-python-block)
    (else_clause              . treesit-fold-range-python-block)
    (match_statement          . treesit-fold-range-python-block)
    (case_clause              . treesit-fold-range-python-block)
    (try_statement            . treesit-fold-range-python-block)
    (except_clause            . treesit-fold-range-python-block)
    (with_statement           . treesit-fold-range-python-block)
    (list                     . treesit-fold-range-seq)
    (dictionary               . treesit-fold-range-seq)
    (parenthesized_expression . treesit-fold-range-seq)
    (expression_statement     . treesit-fold-range-python-expression-statement)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-qss ()
  "Rule set for QSS."
  (append (treesit-fold-parsers-css)))

(defun treesit-fold-parsers-r ()
  "Rule set for R."
  '((brace_list . treesit-fold-range-seq)))

(defun treesit-fold-parsers-ron ()
  "Rule set for RON."
  '((array  . treesit-fold-range-seq)
    (map    . treesit-fold-range-seq)
    (struct . treesit-fold-range-ron-struct)
    (line_comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node
                                          (treesit-fold--cons-add offset '(0 . -1))
                                          "///")))
    (block_comment . treesit-fold-range-block-comment)))

(defun treesit-fold-parsers-rst ()
  "Rule set for reStructuredText."
  '((body    . treesit-fold-range-rst-body)
    (comment . (treesit-fold-range-seq 1 0))))

(defun treesit-fold-parsers-ruby ()
  "Rule set for Ruby."
  '((class    . treesit-fold-range-ruby-class-def)
    (method   . treesit-fold-range-ruby-class-def)
    (array    . treesit-fold-range-seq)
    (do       . (treesit-fold-range-seq 1 -2))     ; match with `end`
    (do_block . (treesit-fold-range-seq 1 -2))     ; match with `end`, in spec file
    (then     . treesit-fold-range-ruby-if)        ; `if` and `elsif` block
    (else     . (treesit-fold-range-ruby-if 4 0))  ; `else` block
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-rust ()
  "Rule set for Rust."
  '((declaration_list       . treesit-fold-range-seq)
    (enum_variant_list      . treesit-fold-range-seq)
    (field_declaration_list . treesit-fold-range-seq)
    (use_list               . treesit-fold-range-seq)
    (field_initializer_list . treesit-fold-range-seq)
    (match_block            . treesit-fold-range-seq)
    (macro_definition       . (treesit-fold-range-rust-macro 1 -1))
    (block                  . treesit-fold-range-seq)
    (token_tree             . treesit-fold-range-seq)
    (line_comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node
                                          (treesit-fold--cons-add offset '(0 . -1))
                                          "///")))
    (block_comment          . treesit-fold-range-block-comment)))

(defun treesit-fold-parsers-scala ()
  "Rule set for Scala."
  '((import_selectors . treesit-fold-range-seq)
    (template_body    . treesit-fold-range-seq)
    (block            . treesit-fold-range-seq)
    (comment          . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-scheme ()
  "Rule set for Scheme."
  '((list . treesit-fold-range-seq)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset ";;")))))

(defun treesit-fold-parsers-sql ()
  "Rule set for SQL."
  '((block              . treesit-fold-range-sql-block)
    (subquery           . treesit-fold-range-seq)
    (list               . treesit-fold-range-seq)
    (column_definitions . treesit-fold-range-seq)
    (marginalia         . treesit-fold-range-c-like-comment)  ; This is the comment!
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "--")))))

(defun treesit-fold-parsers-svelte ()
  "Rule set for Svelte."
  (append
   (treesit-fold-parsers-html)
   '((script_element . treesit-fold-range-html)
     (style_element  . treesit-fold-range-html))))

(defun treesit-fold-parsers-swift ()
  "Rule set for Swift."
  '((function_body     . treesit-fold-range-seq)
    (class_body        . treesit-fold-range-seq)
    (enum_class_body   . treesit-fold-range-seq)
    (protocol_body     . treesit-fold-range-seq)
    (multiline_comment . treesit-fold-range-c-like-comment)
    (comment           . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-tablegen ()
  "Rule set for Tablegen."
  '((record_body       . treesit-fold-range-seq)
    (multiline_comment . treesit-fold-range-c-like-comment)
    (comment           . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-toml ()
  "Rule set for TOML."
  '((table        . treesit-fold-range-toml-table)
    (array        . treesit-fold-range-seq)
    (inline_table . treesit-fold-range-seq)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))))

(defun treesit-fold-parsers-typescript ()
  "Rule set for TypeScript."
  (append
   (treesit-fold-parsers-javascript)
   '((class_body    . treesit-fold-range-seq)
     (enum_body     . treesit-fold-range-seq)
     (named_imports . treesit-fold-range-seq)
     (object_type   . treesit-fold-range-seq))))

(defun treesit-fold-parsers-tsx ()
  "Rule set for TSX files (TypeScript with JSX)."
  (append
   (treesit-fold-parsers-typescript)
   '((jsx_element   . treesit-fold-range-html))))

(defun treesit-fold-parsers-verilog ()
  "Rule set for Verilog."
  '((module_declaration       . treesit-fold-range-verilog-module)
    (list_of_port_connections . treesit-fold-range-verilog-list)
    (initial_construct        . treesit-fold-range-verilog-initial-construct)
    (comment                  . treesit-fold-range-c-like-comment)))

(defun treesit-fold-parsers-vhdl ()
  "Rule set for VHDL."
  '((package_declaration         . treesit-fold-range-vhdl-package)
    (full_type_declaration       . treesit-fold-range-vhdl-type)
    (enumeration_type_definition . treesit-fold-range-seq)
    (comment                     . treesit-fold-range-lua-comment)))

(defun treesit-fold-parsers-vim ()
  "Rule set for Vim."
  '((function_definition . treesit-fold-range-vim-for-loop)
    (for_loop            . treesit-fold-range-vim-for-loop)
    (while_loop          . treesit-fold-range-vim-for-loop)
    (if_statement        . treesit-fold-range-vim-for-loop)
    (elseif_statement    . treesit-fold-range-vim-for-loop)
    (else_statement      . treesit-fold-range-vim-for-loop)
    (list                . treesit-fold-range-seq)
    (comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "\"")))))

(defun treesit-fold-parsers-xml ()
  "Rule set for XML."
  '((element . treesit-fold-range-html)
    (Comment . (treesit-fold-range-seq 3 -2))))

(defun treesit-fold-parsers-yaml ()
  "Rule set for YAML."
  '((comment
     . (lambda (node offset)
         (treesit-fold-range-line-comment node offset "#")))
    (block_sequence_item
     . (lambda (node offset)
         (let* ((key (treesit-search-subtree
                      node
                      (lambda (node)
                        (string= "key" (treesit-node-field-name node)))))
                (value (treesit-search-subtree
                        node
                        (lambda (node)
                          (string= "value" (treesit-node-field-name node)))))
                (beg (treesit-node-end
                      (if (string= "block_node" (treesit-node-type value))
                          key
                        value)))
                (end (treesit-node-end node)))
           (treesit-fold--cons-add (cons beg end) offset))))
    (block_mapping_pair
     . ((lambda (node offset)
          (treesit-fold-range-markers node offset ":"))
        0 1))))

(defun treesit-fold-parsers-zig ()
  "Rule set for Zig."
  '((ErrorSetDecl  . (lambda (node offset)
                       (treesit-fold-range-markers node offset "{")))
    (ContainerDecl . (lambda (node offset)
                       (treesit-fold-range-markers node offset "{")))
    (SwitchExpr    . (lambda (node offset)
                       (treesit-fold-range-markers node offset "{")))
    (Block         . treesit-fold-range-seq)
    (InitList      . treesit-fold-range-seq)
    (line_comment  . treesit-fold-range-c-like-comment)))

(provide 'treesit-fold-parsers)
;;; treesit-fold-parsers.el ends here
