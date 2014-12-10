(when (require 'magit-autoloads nil 'noerror)

;; http://magit.github.io/magit.html#Activating-extensions
;; e.g. to activate magit-svn, exec the following git cmd in your repository:
;; git config --add magit.extension svn
(add-hook 'magit-mode-hook 'magit-load-config-extensions)
)
