;; enable EDE
(global-ede-mode 1)
;(ede-enable-generic-projects)

(ede-cpp-root-project
 "mynst"
 :name                "mynst"
 :file                "~/mynst/Makefile"
 :include-path        '()
 :system-include-path '("/vol/nst/include")
 :spp-table           '()
 :local-variables     '((indent-tabs-mode    . t)
                        (c-indentation-style . "linux")
								(compile-command . "make -C ~/mynst all")))

(ede-cpp-root-project
 "nst"
 :name                "nst"
 :file                "~/nst7/Makefile"
 :include-path        '()
 :system-include-path '()
 :spp-table           '()
 :local-variables     '((indent-tabs-mode    . t)
                        (c-indentation-style . "linux")
								(compile-command . "make -C ~/nst7 all")))

(ede-cpp-root-project
 "sfbVision"
 :name                "sfbVision"
 :file                "~/src/sfbVision/Makefile.custom"
 :include-path        '("src")
 :system-include-path '()
 :spp-table           '()
 :local-variables     '((indent-tabs-mode    . nil)
								(c-basic-offset      . 2)
								(compile-command . "make -j4 -C ~/src/sfbVision")
								(eval . (progn (c-set-offset 'innamespace  '+)))))

