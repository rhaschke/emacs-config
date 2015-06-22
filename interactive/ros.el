(defun invoke-rosemacs ()
  (interactive)
  (add-hook 'shell-mode-hook 'set-rosemacs-shell-hooks)
  (setq rosemacs/invoked t)
  ;(rosemacs/track-topics ros-topic-update-interval)
  ;(rosemacs/track-nodes ros-node-update-interval)

  ;; nxml mode
  (let ((path (locate-file "rng-schemas.xml" load-path)))
    (when path
      (require 'rng-loc)
      (pushnew path rng-schema-locating-files)))
  (add-to-list 'auto-mode-alist '("\\.launch$" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.urdf" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.xacro" . xml-mode))
  (add-to-list 'auto-mode-alist '("\\.cmake\\.em" . cmake-mode))

  ;; rosbag view mode
  (add-to-list 'auto-mode-alist '("\.bag$" . rosbag-view-mode))  

  ;; msg and srv files: for now use gdb-script-mode
  (add-to-list 'auto-mode-alist '("\\.msg\\'" . gdb-script-mode))
  (add-to-list 'auto-mode-alist '("\\.srv\\'" . gdb-script-mode))
  (add-to-list 'auto-mode-alist '("\\.action\\'" . gdb-script-mode))
  (font-lock-add-keywords 'gdb-script-mode
                          '(("\\<\\(bool\\|byte\\|int8\\|uint8\\|int16\\|uint16\\|int32\\|uint32\\|int64\\|uint64\\|float32\\|float64\\|string\\|time\\|duration\\)\\>" . font-lock-builtin-face)) 'set))

;; post-pone loading of rosemacs until it is used first time
(autoload 'ros-keymap "rosemacs" "ros keymap" nil 'keymap)
(autoload 'rosbag-view-mode "rosemacs")
(autoload 'set-rosemacs-shell-hooks "rosemacs")

;; load rosemacs environment
(invoke-rosemacs)

;; Optional but highly recommended: add a prefix for quick access to the rosemacs commands
(global-set-key "\C-x\C-r" 'ros-keymap)
