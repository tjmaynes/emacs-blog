;; --- build_blog.el ---
;; Author: TJ Maynes <tjmaynes at gmail dot com>
;; Website: https://tjmaynes.com/

;; Utilities

(defun utilities/get-environment-variable (env-name)
  (let ((value (getenv env-name)))
    (if (not value) (error (format "Missing environment variable: %s." env-name)))
    value))

(defun utilities/ensure-directory-exists (directory)
  (unless (file-directory-p directory)
    (make-directory directory :parents))
  directory)

(defun utilities/read-json-file (json-file)
  (require 'json)
  (let* ((json-object-type 'hash-table)
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (data (json-read-file json-file)))
    data))

(defun utilities/org-get-keywords ()
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
		       (org-element-property :value keyword)))))

(defun utilities/org-get-file-keyword (keyword)
  (cdr (assoc keyword (utilities/org-get-keywords))))

(defun utilities/org-parse-and-format-date (str format)
  (let ((time-string (org-time-string-to-time str)))
    (format-time-string format time-string)))

(defun utilities/get-relative-parent-directory (file)
  (let* ((directory (file-name-directory (directory-file-name file)))
	 (directory (substring directory 0 (1- (length directory)))))
    (file-name-nondirectory directory)))

;; Package Manager

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

;; Blog

(defun blog/get-head (title description)
  (let ((blog-author-avatar-url (format "%s/%s" blog-url blog-author-avatar)))
    (concat
     "<head>\n"
     (concat
      "<meta charset=\"utf-8\">
<title>" title "</title>
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">
<meta name=\"description\" content=\"" description "\" />
<meta property=\"og:title\" content=\"" title "\" />
<meta property=\"og:url\" content=\"https://tjmaynes.com/index.html\" />
<meta property=\"og:description\" content=\"" description "\"/>
<meta property=\"og:image\" content=\"" blog-author-avatar-url "\" />
<meta property=\"og:type\" content=" "hello" " />
<meta property=\"twitter:title\" content=\"" title "\" />
<meta property=\"twitter:url\" content=\"https://tjmaynes.com/index.html\" />
<meta property=\"twitter:image\" content=\"" blog-author-avatar-url "\" />
<meta property=\"twitter:description\" content=\"" description "\" />
<meta property=\"twitter:card\" content=\"" description "\" />
<link rel=\"stylesheet\" type=\"text/css\" href=\"" blog-css-url "\">\n")
     "</head>\n")))

(defun blog/get-header ()
  (concat
   "<section class=\"content-header\">\n"
   (concat
    "<nav>
     <ul>
       <li><a href=\"/\"><img src=" blog-icon " alt=\"icon\" /></a></li>
     </ul>
     <ul>
       <li><a href=\"/\">Home</a></li>
       <li><a href=\"" blog-author-cv "\">CV</a></li>
       <li><a href=\"/rss.xml\">Feed</a></li>
     </ul>
   </nav>\n")
   "</section>\n"))

(defun blog/get-footer ()
  (concat
   "<section class=\"content-footer\">\n"
   (concat
    "<article class=\"about\">
     <img src=" blog-author-avatar ">
     <p>" blog-author-description "</p>
    </article>
    <nav class=\"footer-menu\">
     <ul>
      <li><p><a href=\"https://github.com/" blog-author-github "\" target=\"_blank\">GitHub</a></p></li>
      <li><p><a href=\"https://linkedin.com/in/" blog-author-linkedin "\" target=\"_blank\">LinkedIn</a></p></li>
      <li><p><a href=\"https://twitter.com/" blog-author-twitter "\" target=\"_blank\">Twitter</a></p></li>
      <li><p><a href=\"mailto:" blog-author-email "\">Contact</a></p></li>
     </ul>
     <ul>
      <li>
        <p><a href=\"https://github.com/tjmaynes/blog\">Built using Org-Mode ❤</a></p>
      </li>
     </ul>
    </nav>")
   "</section>\n"))

(defun blog/get-body (content)
  (concat
   "<body>\n"
   (concat
    "<div class=\"content-wrapper\">\n"
    (blog/get-header)
    content
    (blog/get-footer)
    "</div>\n")
   "</body>\n"))

(defun blog/get-post-header (post-title post-date)
  (let* ((xml-date-time (utilities/org-parse-and-format-date post-date "%F"))
	 (display-date-time (utilities/org-parse-and-format-date post-date "%Y-%m-%d")))
    (concat
     "<header>\n"
     (concat
      "<h1 itemprop=\"name headline\">" post-title "</h1>\n"
      "<p>Posted on <time datetime=\"" xml-date-time "\" itemprop=\"datePublished\">" display-date-time "</time> • " blog-author-name "</p>\n")
   "</header>\n")))

(defun blog/get-post-body (title date content)
  (concat
   "<div class=\"post\">\n"
   (blog/get-post-header title date)
   content
   "<p>" blog-author-footnote-message "</p>\n"
   "</div>\n"))

(defun blog/get-html (head body language)
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" language)
   head
   body
   "</html>\n"))

(defun blog/base-html-template (title description language content)
  (blog/get-html
   (blog/get-head title description)
   (blog/get-body content)
   language))

(defun blog/blog-index-template (content info)
  (let* ((language (plist-get info :language)))
    (blog/base-html-template blog-title
			     blog-description
			     language
			     (concat
			      "<div class=\"archive\">\n"
			      content
			      "</div>\n"))))

(defun blog/blog-post-template (content info)
  (let* ((title (utilities/org-get-file-keyword "TITLE"))
	 (date (utilities/org-get-file-keyword "DATE"))
	 (description (utilities/org-get-file-keyword "DESCRIPTION"))
	 (language (plist-get info :language)))
    (blog/base-html-template title
			     description
			     language
			     (blog/get-post-body title date content))))

(defun blog/get-page-body (title date content)
  (concat
   "<div class=\"post\">\n"
   content
   "</div>\n"))

(defun blog/blog-page-template (content info)
  (let* ((title (utilities/org-get-file-keyword "TITLE"))
	 (date (utilities/org-get-file-keyword "DATE"))
	 (description (utilities/org-get-file-keyword "DESCRIPTION"))
	 (language (plist-get info :language)))
    (blog/base-html-template title
			     description
			     language
			     (blog/get-page-body title date content))))

(defun blog/org-publish-to-html (plist filename pub-dir)
  (let ((parent-directory (utilities/get-relative-parent-directory filename))
	(posts-dir (expand-file-name "posts" pub-dir)))
    (cond ((equal parent-directory "posts")
	   (if (equal (file-name-base filename) "index")
	       (org-publish-org-to 'custom-blog-index-backend filename ".html" plist pub-dir)
	     (org-publish-org-to 'custom-blog-post-backend filename ".html" plist posts-dir)))
	  ((org-publish-org-to 'custom-blog-page-backend filename ".html" plist pub-dir)))))

(defun blog/org-publish-sitemap (_title list)
  (package-manager/ensure-packages-installed 'seq)
  (require 'seq)
  (mapconcat (lambda (li)
	       (format "@@html:<li class=\"archive-item\">@@%s@@html:</li>@@" (car li)))
	     (seq-filter #'car (cdr list))
	     "\n"))

(defun blog/org-publish-sitemap-format (entry style project)
  (let ((datetime (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
	(title (org-publish-find-title entry project))
	(post-entry (format "posts/%s" entry)))
    (format "@@html:<span class=\"archive-item\"><span class=\"archive-date\">@@ %s @@html:</span>@@ | [[file:%s][%s]] @@html:</span>@@"
	    datetime post-entry title)))

(defun blog/org-rss-publish-to-rss (plist filename pub-dir)
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

(defun blog/get-publish-project-alist ()
  `(("blog-home"
     :base-directory ,(expand-file-name "posts" blog-directory)
     :base-extension "org"
     :exclude ,(regexp-opt '("rss.org" "index.org"))
     :publishing-function blog/org-publish-to-html
     :publishing-directory ,blog-publishing-directory
     :html-home/up-format nil
     :auto-sitemap t
     :sitemap-filename "index.org"
     :sitemap-title ,blog-title
     :sitemap-style list
     :sitemap-sort-files anti-chronologically
     :sitemap-function blog/org-publish-sitemap
     :sitemap-format-entry blog/org-publish-sitemap-format)
    ("blog-post-images"
     :base-directory ,(expand-file-name "posts/images" blog-directory)
     :exclude nil
     :base-extension ,(regexp-opt '("jpg" "png"))
     :publishing-directory ,(expand-file-name (format "%s/posts/images" build-directory) blog-directory)
     :publishing-function org-publish-attachment
     :recursive nil)    
    ("blog-rss"
     :base-directory ,(expand-file-name "posts" blog-directory)
     :base-extension "org"
     :recursive nil
     :exclude ,(regexp-opt '("index.org"))
     :publishing-function blog/org-rss-publish-to-rss
     :publishing-directory ,blog-publishing-directory
     :rss-extension "xml"
     :table-of-contents nil)
    ("blog-public"
     :base-directory ,(expand-file-name "static" blog-directory)
     :exclude ,(regexp-opt '("public"))
     :base-extension ,(regexp-opt '("jpg" "png" "css" "eot" "woff" "woff2" "ttf"))
     :publishing-directory ,(expand-file-name (format "%s/public" build-directory) blog-directory)
     :publishing-function org-publish-attachment
     :recursive t)
    ("blog" :components ("blog-home" "blog-post-images" "blog-rss" "blog-public"))))

(defun blog/setup-custom-templates ()
  (require 'ox)
  (org-export-define-derived-backend 'custom-blog-index-backend 'html
				     :translate-alist '((template . blog/blog-index-template)))
  (org-export-define-derived-backend 'custom-blog-post-backend 'html
				     :translate-alist '((template . blog/blog-post-template)))
  (org-export-define-derived-backend 'custom-blog-page-backend 'html
				     :translate-alist '((template . blog/blog-page-template))))

(defun blog/publish-all ()
  (package-manager/ensure-packages-installed 'org 'org-plus-contrib 'htmlize)
  (require 'org)
  (require 'htmlize)
  (let* ((org-publish-project-alist          (blog/get-publish-project-alist))
	 (org-export-with-section-numbers    nil)
	 (org-export-with-smart-quotes       t)
	 (org-export-with-toc                nil)
	 (org-export-with-sub-superscripts   '{})
	 (org-html-container-element         "section")
	 (org-html-metadata-timestamp-format "%Y-%m-%d")
	 (org-html-checkbox-type             'html)
	 (org-html-html5-fancy               t)
	 (org-html-validation-link           nil)
	 (org-html-doctype                   "html5")
	 (org-html-htmlize-output-type       'css)
	 (make-backup-files nil))
    (blog/setup-custom-templates)
    (org-publish-project "blog" t)))

(defun blog/add-file-to-publishing-directory (file-location)
  (let* ((file (file-name-nondirectory file-location))
	 (destination-file (expand-file-name file blog-publishing-directory)))
    (message (format "Publishing file %s to %s" file blog-publishing-directory))
    (copy-file file-location destination-file t t)))

(defun blog/add-files-to-publishing-directory (files)
  (require 'seq)
  (seq-map 'blog/add-file-to-publishing-directory files))

(defun blog/compile-latex-file (file-location)
  (let* ((file (file-name-nondirectory file-location))
	 (filename (file-name-base file))
	 (file-directory (utilities/get-relative-parent-directory file-location))
	 (output-directory (expand-file-name file-directory blog-publishing-directory))
	 (output-directory (utilities/ensure-directory-exists output-directory))
	 (change-directory-command (concat "cd " file-directory))
	 (latex-program (executable-find "xelatex"))
	 (compile-latex-command (concat latex-program " -output-directory=" output-directory " " file))
	 (cleanup-command (concat "rm -rf " (expand-file-name (format "%s.log" filename) output-directory)))
	 (compile-command (concat
			   change-directory-command " && "
			   compile-latex-command " && "
			   cleanup-command)))
    (message (format "Compiling LaTeX file %s to %s" file-location output-directory))
    (shell-command compile-command)))

(defun blog/compile-latex-files (files)
  (require 'seq)
  (seq-map 'blog/compile-latex-file files))

(defun setup-global-variables (blog-directory build-directory config)
  (let* ((settings-config (gethash "settings" config))
	 (author-config (gethash "author" config)))
    (setq blog-title (gethash "title" settings-config)
	  blog-description (gethash "description" settings-config)
	  blog-url (gethash "url" settings-config)
	  blog-icon (gethash "icon" settings-config)
	  blog-needed-files (gethash "needed-files" settings-config)
	  blog-latex-files (gethash "latex-files" settings-config)
	  blog-author-name  (gethash "name" author-config)
	  blog-author-email (gethash "email" author-config)
	  blog-author-github (gethash "github" author-config)
	  blog-author-linkedin (gethash "linkedin" author-config)
	  blog-author-twitter (gethash "twitter" author-config)	  
	  blog-author-description (gethash "description" author-config)
	  blog-author-cv (gethash "cv" author-config)	  
	  blog-author-avatar (gethash "avatar" author-config)
	  blog-author-footnote-message (gethash "footnote-message" author-config)
	  blog-directory blog-directory
	  blog-publishing-directory (expand-file-name build-directory blog-directory)
	  blog-css-url (gethash "css" settings-config)
	  blog-timestamps-directory (concat blog-directory "timestamps"))))

(defun initialize (config-location blog-directory build-directory)
  (let* ((config (utilities/read-json-file config-location))
	 (publish-config (gethash "publish" config)))
    (setup-global-variables blog-directory build-directory config)
    (package-manager/setup)
    (blog/publish-all)
    (blog/compile-latex-files blog-latex-files)
    (blog/add-files-to-publishing-directory blog-needed-files)))

(initialize
 (utilities/get-environment-variable "BLOG_CONFIG")
 (utilities/get-environment-variable "BLOG_DIRECTORY")
 (utilities/get-environment-variable "BLOG_BUILD_DIRECTORY"))
