(add-hook 'c-mode-common-hook 'hs-minor-mode)

;(load-file "/vol/ni/share/lib/emacs/cedet-1.1/common/cedet.el")

(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode) ; allow jumping to previous code blocks
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode) ; maintain tag database
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode) ; reparse buffer when idle
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode) ; show completions when idle
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode) ; highlight current tag
;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode) ; summarize tag at point
;;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode) ; show current function in header line

;; Activate semantic
(semantic-mode 1)

;; load some language support
(require 'semantic/bovine/c)
(require 'semantic/bovine/gcc)  ;; set gcc include files
(require 'semantic/bovine/make)
(require 'semantic/analyze)
(require 'semantic/analyze/refs)

;; add system-wide include paths
(semantic-add-system-include "/vol/xcf/include" 'c++-mode)
(semantic-add-system-include "/vol/rsb/include" 'c++-mode)

;; display information for tags & classes
(require 'semantic/ia)

(require 'semantic/decorate/include)
(require 'semantic/lex-spp)

;; customisation of modes
(defun install-common-cedet-keys ()
  (local-set-key [C-return] 'semantic-ia-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
  (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block))

;; load cedet key map
(add-hook 'c-mode-common-hook 'install-common-cedet-keys)
(add-hook 'lisp-mode-hook 'install-common-cedet-keys)
(add-hook 'scheme-mode-hook 'install-common-cedet-keys)
(add-hook 'emacs-lisp-mode-hook 'install-common-cedet-keys)

(defun install-c-mode-cedet-keys ()
  ; allow auto-completion for variable. or variable->
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref))

(add-hook 'c-mode-common-hook 'install-c-mode-cedet-keys)

;; have nice decorations by default
(global-semantic-decoration-mode 1)

;; EDE
(global-ede-mode 1)
(ede-enable-generic-projects)

;; functions for EDE
(defun ede-get-local-var (fname var)
  "fetch given variable var from :local-variables of project of file fname"
  (let* ((current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (when prj
      (let* ((ov (oref prj local-variables))
            (lst (assoc var ov)))
        (when lst
          (cdr lst))))))

(defun rhaschke/compile ()
  "Saves all unsaved buffers, and runs 'compile'."
  (interactive)
  (save-some-buffers t)
  (let* ((r (ede-get-local-var
             (or (buffer-file-name (current-buffer)) default-directory)
             'compile-command))
         (cmd (if (functionp r) (funcall r) r)))
	 (message "var: %s" r)
    (set (make-local-variable 'compile-command) (or cmd compile-command))
    (compile compile-command)))

(global-set-key [f9] 'rhaschke/compile)

;;
(defun rhaschke/gen-std-compile-string ()
  "Generates compile string for compiling CMake project in debug mode"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj)))
    (concat "cd " root-dir "; make -j2")))

;;
(defun rhaschke/gen-cmake-debug-compile-string ()
  "Generates compile string for compiling CMake project in debug mode"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project current-dir))
         (root-dir (ede-project-root-directory prj))
         (subdir "")
         )
    (when (string-match root-dir current-dir)
      (setf subdir (substring current-dir (match-end 0))))
    (concat "cd " root-dir "Debug/" "; make -j3")))

(defun rhaschke/gen-cmake-debug/release-compile-string ()
  "Generates compile string for compiling CMake project in debug & release modes"
  (let* ((current-dir (file-name-directory
                       (or (buffer-file-name (current-buffer)) default-directory)))
         (prj (ede-current-project "~/src/rsb/cpp/core/src/rsb/"))
         (root-dir (ede-project-root-directory prj))
         (subdir "")
         )
    (when (string-match root-dir current-dir)
      (setf subdir (substring current-dir (match-end 0))))
    (concat "cd " root-dir "Debug/ && make -j3 && cd " root-dir "Release/ && make -j3" )))

;; Projects

;; cpp-tests project definition
(when (file-exists-p "~/projects/lang-exp/cpp/CMakeLists.txt")
  (setq cpp-tests-project
	(ede-cpp-root-project "cpp-tests"
			      :file "~/projects/lang-exp/cpp/CMakeLists.txt"
			      :system-include-path '("/home/ott/exp/include"
						     boost-base-directory)
			      :local-variables (list
						(cons 'compile-command 'alexott/gen-cmake-debug-compile-string)
						)
			      )))

;;; emacs-rc-cedet.el ends here

