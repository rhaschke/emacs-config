;;; Projects

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

