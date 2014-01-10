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

(defvar celbrech-style
  '("celbrech"
    (c-basic-offset             . 2)
    (c-comment-only-line-offset . 0)
	 (indent-tabs-mode           . nil)
	 (standard-indent            . 2)

    (c-offsets-alist
     (statement-block-intro . +)
     (substatement-open     . 0)
     (substatement-label    . 0)
     (label                 . 0)
     (statement-cont        . +)

     (namespace-close       . 0)
     (namespace-open        . 0)
     (innamespace           . 2))))

(unless (find "celbrech" c-style-alist
	      :key  #'car
	      :test #'string=)
  (push celbrech-style c-style-alist))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;;; Projects
;;

(require 'ede/cpp-root)

(ede-cpp-root-project
 "rsc"
 :name                "RSC"
 :file                "~/src/rsc/CMakeLists.txt"
 :include-path        '("/src")
 :system-include-path '()
 :spp-table           '(("RSC_EXPORT" . ""))
 :local-variables     `((indent-tabs-mode    . nil)
                        (c-indentation-style . "rsc")))

(ede-cpp-root-project
 "rsb"
 :name                "RSB"
 :file                "~/src/rsb/rsb-cpp/CMakeLists.txt"
 :include-path        '("/src/rsb")
 :system-include-path '()
 :spp-table           '(("RSB_EXPORT" . ""))
 :local-variables     `((indent-tabs-mode    . nil)
                        (c-indentation-style . "rsc")))

(ede-cpp-root-project
 "rst"
 :name                "RST"
 :file                "~/src/rst/rst-proto/CMakeLists.txt"
 :system-include-path '()
 :spp-table           '(("RST_EXPORT" . "")))

;; (ede-cpp-root-project
;;  "rsx"
;;  :name                "RSX"
;;  :file                "/vol/rsb/include/sp.h"
;;  :include-path        '("")
;;  :system-include-path '()
;;  :spp-table           '(("RSC_EXPORT" . "") ("RSB_EXPORT" . "") ("RST_EXPORT" . ""))
;;  :local-variables     `((indent-tabs-mode    . nil)
;;                         (c-indentation-style . "rsc")))

(ede-cpp-root-project
 "rsb-xml"
 :name                "RSB-XML"
 :file                "~/src/rsb-xml/rsb-xml-cpp/CMakeLists.txt"
 :include-path        '("/cpp/src")
 :system-include-path '("/vol/rsb/include")
 :spp-table           '(("RSC_EXPORT" . "") ("RSB_EXPORT" . "") ("RST_EXPORT" . ""))
 :local-variables     `((indent-tabs-mode    . nil)
                        (c-indentation-style . "rsc")
								(compile-command . "make -C ~/src/rsb-xml/rsb-xml-cpp/build")))

(ede-cpp-root-project
 "mynst"
 :name                "mynst"
 :file                "~/mynst/Makefile"
 :include-path        '()
 :system-include-path '("/local/vol/nst/include")
 :spp-table           '()
 :local-variables     `((indent-tabs-mode    . t)
                        (c-indentation-style . "linux")
								(compile-command . "make -C ~/mynst all")))

(ede-cpp-root-project
 "nst"
 :name                "nst"
 :file                "~/nst7/Makefile"
 :include-path        '()
 :system-include-path '()
 :spp-table           '()
 :local-variables     `((indent-tabs-mode    . t)
                        (c-indentation-style . "linux")
								(compile-command . "make -C ~/nst7 all")))

