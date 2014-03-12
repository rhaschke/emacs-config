(setq c-default-style
		'((java-mode . "java")
		  (awk-mode . "awk")
		  (other . "linux")))

(setq c-basic-offset 3)
(dolist (name '(case-label 
					 innamespace 
					 inline-open)) 
  (c-set-offset name 0))

(defvar rsc-style
  '("rsc"
    (c-basic-offset             . 4)
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

(unless (find "rsc" c-style-alist
	      :key  #'car
	      :test #'string=)
  (push rsc-style c-style-alist))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
