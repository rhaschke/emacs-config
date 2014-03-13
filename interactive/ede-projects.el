;; enable EDE
(global-ede-mode 1)
(ede-enable-generic-projects)

(defun rhaschke/ede-current-project (&optional buffer-or-filename)
  "fetch ede project from argument"
  (let* ((filename (cond 
						 ; arg is buffer
						 ((bufferp buffer-or-filename) 
						  (or (buffer-file-name buffer-or-filename) default-directory))
						 ; arg is string: interprete as filename
						 ((and (stringp buffer-or-filename)
								 (file-exists-p buffer-or-filename))
						  buffer-or-filename)
						 ; arg is something else: use filename of current-buffer
						 (t (or (buffer-file-name (current-buffer)) default-directory))))
			(dir (file-name-directory filename)))
	 (ede-current-project dir)))

(defun rhaschke/ede-get-local-var (prj var)
  "fetch given variable var from :local-variables of project"
  (when prj
	 (let* ((ov (oref prj local-variables))
			  (lst (assoc var ov)))
		(when lst (cdr lst)))))

(defun rhaschke/std-compile-cmd (&optional subdir target)
  "Generates compile string for compiling CMake project in debug mode"
  (let* ((prj (rhaschke/ede-current-project))
         (root-dir (concat (ede-project-root-directory prj) subdir)))
    (concat "make -C " root-dir " -j4 " target)))

(defun rhaschke/get-spp-symbol-definition (item)
  "convert item=(name . value) pair to -Dname=value"
  (let ((name (car item))
		  (value (cdr item)))
	 (setq value (cond
					  ((numberp value) (number-to-string value))
					  ((and (stringp value) (string= value "")) nil)
					  (t nil)))
	 (when value (setq value (concat "=" value)))
	 (concat "-D" name value)))
	 
(defun rhaschke/gen-clang-flags (&optional defines includes)
  (append (mapcar 'rhaschke/get-spp-symbol-definition defines)
			 (mapcar (lambda (item) (concat "-I" item)) includes)))

(defun rhaschke/process-ede-settings ()
  "process ede project settings
 - define compile-command from function
 - set auto-complete include dirs"
  (interactive)
  (let ((prj (rhaschke/ede-current-project)))
	 (when prj
		;; set compile-command from function
		(let* ((cmd (rhaschke/ede-get-local-var prj 'compile-command))
				 (cmd (cond
						 ((functionp cmd) (funcall cmd))
						 ((stringp cmd) cmd)
						 (t (rhaschke/std-compile-cmd))))
				 (ov (oref prj local-variables))
				 (lst (assoc 'compile-command ov)))
		  (if (null lst) (push '(compile-command . cmd) ov)
			 (setcdr lst cmd)))
		;; set includes for clang auto-completion
		(when (boundp 'ac-clang-flags)
		  (set (make-local-variable 'ac-clang-flags) 
				 (rhaschke/gen-clang-flags (oref prj spp-table) 
													(append (oref prj system-include-path)
															  (oref prj include-path)))))
		)))

(add-hook 'find-file-hook 'rhaschke/process-ede-settings)

;;; definitions of projects

(ede-cpp-root-project
 "mynst"
 :name                "mynst"
 :file                "~/mynst/Makefile"
 :include-path        '()
 :system-include-path '("/vol/nst/include")
 :spp-table           '()
 :local-variables     '((indent-tabs-mode    . t)
                        (c-indentation-style . "linux")
								(compile-command . (lambda() (rhaschke/std-compile-cmd nil "all")))))

(ede-cpp-root-project
 "nst"
 :name                "nst"
 :file                "~/nst7/Makefile"
 :include-path        '("nstsrc" "neosrc" "foldersrc")
 :system-include-path '()
 :spp-table           '()
 :local-variables     '((indent-tabs-mode    . t)
                        (c-indentation-style . "linux")
								(compile-command . (lambda() (rhaschke/std-compile-cmd nil "all")))))

(ede-cpp-root-project
 "sfbVision"
 :name                "sfbVision"
 :file                "~/src/sfbVision/Makefile.custom"
 :include-path        '("src")
 :system-include-path '("/vol/nivision/include/icl-8.4")
 :spp-table           '(("HAVE_QT" . 1))
 :local-variables     '((indent-tabs-mode    . nil)
								(c-basic-offset      . 2)
								(eval . (progn (c-set-offset 'innamespace  '+)))))

(ede-cpp-root-project
 "hsm"
 :name                "hsm"
 :file                "~/src/hsm/configure.ac"
 :include-path        '("src")
 :system-include-path '()
 :spp-table           '(("HAVE_DBUS" . 1))
 ;; backquote ` allows to selectively evaluate parts of a quoted list (marked with ,)
 :local-variables     '((compile-command . (lambda() (rhaschke/std-compile-cmd (concat "o." (getenv "ARCH")))))))

(ede-cpp-root-project
 "cbf"
 :name                "cbf"
 :file                "~/src/cbf/CMakeLists.txt"
 :include-path        '("libcbf")
 :system-include-path '()
 :spp-table           '(("CBF_HAVE_XSD" . 1))
 ;; backquote ` allows to selectively evaluate parts of a quoted list (marked with ,)
 :spp-files           `(,(concat "o." (getenv "ARCH") "/libcbf/cbf/config.h"))
 :local-variables     '((compile-command . (lambda() (rhaschke/std-compile-cmd (concat "o." (getenv "ARCH")))))))
