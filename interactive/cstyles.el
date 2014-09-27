;;; default line width
(setq-default fill-column 100)
;;; indentation
(setq-default standard-indent 3)
(setq c-basic-offset standard-indent)
(setq-default tab-width standard-indent)
(setq-default indent-tabs-mode t) ;; insert tabs to indent

;; smart-tabs-mode: http://www.emacswiki.org/emacs/SmartTabs
(require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'c++ 'java 'javascript 'cperl 'python 'ruby 'nxml)

;; guess c-basic-offset and indent-tabs-mode: https://github.com/jscheid/dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; set some styles based on the mode
(setq c-default-style
		'((java-mode . "java")
		  (awk-mode . "awk")
		  (other . "cc-mode")))

(c-add-style "cc-mode" 
  `("linux"
	 (c-basic-offset  . 3)
	 (tab-width       . ,c-basic-offset)
	 (c-offsets-alist 
	  (case-label   . 0)
	  (innamespace  . 0)
	  (inline-open  . 0))))

(c-add-style "rsc"
  '((c-basic-offset             . 4)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist
     (statement-block-intro . +)
     (substatement-open     . 0)
     (substatement-label    . 0)
     (label                 . 0)
     (statement-cont        . +)

     (namespace-close       . 0)
     (namespace-open        . 0)
     (innamespace           . 0))))


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
