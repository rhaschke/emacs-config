(defun other-window-backwards ()
  "Like `other-window' but visit windows in reverse order."
  (interactive)
  (other-window -1))

(defun fullscreen-p (&optional frame)
  ""
  (frame-parameter frame 'fullscreen))

(defun toggle-fullscreen (&optional frame)
  "Toggle fullscreen state of FRAME.
If FRAME is nil, apply to current frame."
  (interactive)
  (set-frame-parameter frame 'fullscreen
             (if (fullscreen-p frame) nil 'fullboth)))

(defun count-words (start end)
    "Print number of words in the region."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (message "words: %d" (count-matches "\\sw+")))))

(defun normalize-whitespace (start end)
  "remove trailing whitespace, remove empty lines at end of document"
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list nil nil))))
  (save-excursion
   (let ((have-region (and start end))
        (end (or end (point-max)))
        (start (or start (point-min))))
     (indent-region start end)
     (if have-region (delete-trailing-whitespace start end)
      (delete-trailing-whitespace)))))

(defun dired-do-normalize-whitespace ()
  "apply normalize-whitespace function to all marked files in dired"
  (interactive)
  (dired-map-over-marks 
   (progn 
    (dired-find-file)
    (setq-local indent-tabs-mode 1)
    (smart-tabs-mode 1)
    (normalize-whitespace nil nil)
    (save-buffer)
    (kill-buffer)
    ) nil))

(defun toggle-indent-tabs-mode ()
  "toggle indent-tabs-mode for current buffer"
  (interactive)
  (set (make-local-variable 'indent-tabs-mode)
       (not indent-tabs-mode))
  (when (featurep 'smart-tabs-mode)
    (if indent-tabs-mode (smart-tabs-mode 1) (smart-tabs-mode -1)))
  (message "%s indent-tabs-mode" 
           (cond (indent-tabs-mode "enabled")
                 (t "disabled"))))

(defun dired-do-recode ()
  "recode all marked files in dired to utf8"
  (interactive)
  (dired-map-over-marks 
   (progn
    (dired-find-file)
    (set-buffer-file-coding-system 'utf-8)
    (save-buffer)
    (kill-buffer)
    ) nil))
