(add-to-list 'auto-mode-alist '("\\.hsm$" . nxml-mode))

(require 'nxml-speedbar)
(speedbar-add-supported-extension ".hsm")

(require 'nxml-script)
(require 'nxml-where)
(nxml-where-global-mode 1)

;; formatting of breadcrumbs path in hsm files: only looking for state names
(defun nxml-where-format-hsm-element (element)
  ""
  (let ((element-name   (plist-get element :local-name))
		  (attribute      (plist-get element :attribute)))
    (concat 
	  (when (and attribute (string-match-p "STATE$\\|^REGION$" element-name))
		 (let ((attribute-name   (plist-get attribute :local-name))
				 (attribute-value  (plist-get attribute :value)))
			(concat
			 (propertize ":" 'face 'nxml-tag-delimiter)
			 (propertize attribute-value 'face 'nxml-element-local-name)))))))
(defun nxml-where-format-hsm-path (path)
  ""
  (mapconcat #'nxml-where-format-hsm-element path ""))
(setq nxml-where-format-path-function #'nxml-where-format-hsm-path)

; regular expression to generate outline in hsm mode
(setq-mode-local nxml-mode 
	nxml-section-element-name-regexp "STATE\\|REGION\\|EVENT\\|ONEVENT")

;; hsm error display
(defun display-hsm-errors (message)
  (with-current-buffer (get-buffer-create "*hsm errors*")
    (erase-buffer)
    (insert message)
    (pop-to-buffer (current-buffer))))

;; jump to buffer for python errors
(defun hsm-pythonError (errors file row col &rest ignored)
  "callback method for dbus messages from hsm (for python errors)"
  (message "HSM python error in %s:%d" file row)
;  (display-hsm-errors errors)
  (when (and file (file-exists-p file))
	 (let ((buf (find-buffer-visiting file)))
		(if buf 
		  (let ((w (get-buffer-window buf)))
			 (if w 
				(select-window w) ; make window w active
				(switch-to-buffer buf))) ; show buf in currently active window
		  (find-file file))
		(when (> row 0) (goto-line row))
		(when (> col 0) (move-to-column col))
;		(raise-frame (window-frame (selected-window)))
)))

(require 'dbus)
(when (and (eq window-system 'x) (fboundp 'dbus-register-signal))
  (condition-case nil 
		(dbus-register-signal
		 :session nil "/de/unibi/agni/hsm"
		 "de.unibi.agni.hsm" "pythonError"
		 'hsm-pythonError)
	 (dbus-error (message "registering dbus hsm messages failed"))))

(defun rhaschke/hsm-mode-hook ()
  (set (make-local-variable 'compile-command)
		 (concat "PYTHONPATH=/vol/nirobots/demo/vdemo-cfg/hsm/python "
					"dynamicHSM -m xcf:ShortTerm "
					buffer-file-name))
)
(add-hook 'nxml-mode-hook 'rhaschke/hsm-mode-hook)
