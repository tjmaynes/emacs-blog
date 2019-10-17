(provide 'package-manager)

(defvar package-manager/package-manager-refreshed nil)

(defun package-manager/package-manager-refresh-once ()
  (when (not package-manager/package-manager-refreshed)
    (setq package-manager/package-manager-refreshed t)
    (package-refresh-contents)))

(defun package-manager/ensure-packages-installed (&rest packages)
  (dolist (package packages)
    (when (not (package-installed-p package))
      (package-manager/package-manager-refresh-once)
      (package-install package))))

(defun package-manager/setup ()
  (setq	package-enable-at-startup nil)
  (package-initialize)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)  
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
