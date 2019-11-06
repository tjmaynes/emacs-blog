;; --- build-blog.el ---
;; Author: TJ Maynes <tjmaynes at gmail dot com>
;; Website: https://tjmaynes.com/

(add-to-list 'load-path (expand-file-name "." "elisp"))

(require 'utilities)
(require 'package-manager)
(require 'org-blog)

(defun setup-global-variables (config blog-publishing-directory)
  (let* ((settings-config (gethash "settings" config))
	 (author-config (gethash "author" config))
	 (files-config (gethash "files" config)))
    (setq blog-title (gethash "title" settings-config)
	  blog-description (gethash "description" settings-config)
	  blog-url (gethash "url" settings-config)
	  blog-icon (gethash "icon" settings-config)
	  blog-author-name  (gethash "name" author-config)
	  blog-author-email (gethash "email" author-config)
	  blog-author-github (gethash "github" author-config)
	  blog-author-linkedin (gethash "linkedin" author-config)
	  blog-author-twitter (gethash "twitter" author-config)	  
	  blog-author-description (gethash "description" author-config)
	  blog-author-cv (gethash "cv" author-config)	  
	  blog-author-avatar (gethash "avatar" author-config)
	  blog-publishing-directory blog-publishing-directory
	  blog-latex-files (gethash "latex" files-config)
	  blog-copy-files (gethash "copy" files-config)
	  blog-css-url (gethash "css" settings-config)
	  blog-timestamps-directory (concat blog-directory "timestamps"))))

(defun initialize (config-location blog-directory build-directory)
  (let* ((config (utilities/read-json-file config-location))
	 (blog-publishing-directory (expand-file-name build-directory blog-directory)))
    (package-manager/setup)
    (package-manager/ensure-packages-installed 'seq)
    (setup-global-variables config blog-publishing-directory)
    (org-blog/publish)
    (utilities/compile-latex-files blog-latex-files blog-publishing-directory)
    (utilities/copy-files-to-publishing-directory blog-copy-files blog-publishing-directory)))

(initialize
 (utilities/get-environment-variable "BLOG_CONFIG")
 (utilities/get-environment-variable "BLOG_DIRECTORY")
 (utilities/get-environment-variable "BLOG_BUILD_DIRECTORY"))
