;; -*- mode: eask; lexical-binding: t -*-

(package "treesit-fold"
         "0.2.1"
         "Code folding using treesit")

(website-url "https://github.com/emacs-tree-sitter/treesit-fold")
(keywords "convenience" "folding" "tree-sitter")

(package-file "treesit-fold.el")
(files "treesit-*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "29.1")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t)))
