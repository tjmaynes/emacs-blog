;; --- build-blog.el ---
;; Author: TJ Maynes <tjmaynes at gmail dot com>
;; Website: https://tjmaynes.com/

(add-to-list 'load-path (expand-file-name "." "elisp"))

(require 'utilities)
(require 'package-manager)
(require 'seq)
(require 'html)

;; Blog

(defvar blog/yt-iframe-format
  (concat
   "<div class=\"video-wrapper\">"
   (concat "<iframe"
	   " src=\"https://www.youtube.com/embed/%s\""
	   " frameborder=\"0\""
	   " allowfullscreen>%s</iframe>")
   "</div>"))

(defun blog/org-add-link-types ()
  (org-add-link-type
   "yt"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
	      handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format blog/yt-iframe-format
		     path (or desc "")))
       (latex (format "\href{%s}{%s}"
		      path (or desc "video")))))))

(defun blog/org-publish-to-html (plist filename pub-dir)
  (let ((parent-directory (utilities/get-relative-parent-directory filename))
	(posts-dir (expand-file-name "posts" pub-dir)))
    (cond ((equal parent-directory "posts")
	   (if (equal (file-name-base filename) "index")
	       (org-publish-org-to 'custom-blog-index-backend filename ".html" plist pub-dir)
	     (org-publish-org-to 'custom-blog-post-backend filename ".html" plist posts-dir)))
	  ((org-publish-org-to 'custom-blog-page-backend filename ".html" plist pub-dir)))))

(defun blog/org-publish-sitemap (_title list)
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

(defun blog/org-publish-format-rss-feed (title list)
  (concat "#+TITLE: " title "\n"
	  "#+DESCRIPTION: " blog-description "\n\n"
          (org-list-to-subtree list '(:icount "" :istart ""))))

(defun blog/org-publish-format-rss-feed-entry (entry style project)
  (cond ((not (directory-name-p entry))
         (let* ((file (org-publish--expand-file-name entry project))
                (title (org-publish-find-title entry project))
                (date (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
                (link (concat (file-name-sans-extension entry) ".html")))
           (with-temp-buffer
             (insert (format "* [[file:%s][%s]]\n" file title))
             (org-set-property "RSS_PERMALINK" link)
             (org-set-property "PUBDATE" date)
             (org-id-get-create)
             (insert-file-contents file)
             (buffer-string))))
        ((eq style 'tree)
         (file-name-nondirectory (directory-file-name entry)))
        (t entry)))

(defun blog/org-rss-publish-to-rss (plist filename pub-dir)
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

(defun blog/org-reveal-get-title-page (title author date)
  (concat
   "<section id='sec-title-slide'>"
   (concat
    "<h1 class='title>" title "</h1>")
   (concat
    "<h2 class='author'>" author "</h2>")
   (concat
    "<h3 class='date'>" date "</h3>")
   "</section>"))

(defun blog/org-reveal-publish-to-html (plist filename pub-dir)
  (require 'org-re-reveal)
  (org-publish-org-to 're-reveal filename ".html" plist pub-dir))

(defun blog/get-publish-project-alist ()
  `(("blog-home"
     :base-directory ,(expand-file-name "posts" blog-directory)
     :base-extension "org"
     :exclude ,(regexp-opt '("index.org" "rss.org"))
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
     :exclude ,(regexp-opt '("index.org" "rss.org"))
     :publishing-function blog/org-rss-publish-to-rss
     :publishing-directory ,blog-publishing-directory
     :rss-extension "xml"
     :auto-sitemap t
     :sitemap-filename "rss.org"
     :sitemap-title ,blog-title
     :sitemap-style list
     :sitemap-sort-files anti-chronologically
     :sitemap-function blog/org-publish-format-rss-feed
     :sitemap-format-entry blog/org-publish-format-rss-feed-entry
     :table-of-contents nil)
    ("blog-public"
     :base-directory ,(expand-file-name "static" blog-directory)
     :exclude ,(regexp-opt '("public"))
     :base-extension ,(regexp-opt '("jpg" "png" "css" "eot" "woff" "woff2" "ttf"))
     :publishing-directory ,(expand-file-name (format "%s/public" build-directory) blog-directory)
     :publishing-function org-publish-attachment
     :recursive t)
    ("blog-talks"
     :base-directory ,(expand-file-name "talks" blog-directory)
     :base-extension "org"
     :publishing-directory ,(expand-file-name (format "%s/talks" build-directory) blog-directory)
     :publishing-function blog/org-reveal-publish-to-html
     :section-numbers nil
     :recursive t)
    ("blog-talks-images"
     :base-directory ,(expand-file-name "talks/images" blog-directory)
     :exclude nil
     :base-extension ,(regexp-opt '("jpg" "png"))
     :publishing-directory ,(expand-file-name (format "%s/talks/images" build-directory) blog-directory)
     :publishing-function org-publish-attachment
     :recursive nil)
    ("blog" :components ("blog-home" "blog-post-images" "blog-rss" "blog-public" "blog-talks" "blog-talks-images"))))

(defun blog/setup-custom-templates ()
  (require 'ox)
  (blog/org-add-link-types)
  (org-export-define-derived-backend 'custom-blog-index-backend 'html
				     :translate-alist '((template . html/index-page-template)))
  (org-export-define-derived-backend 'custom-blog-post-backend 'html
				     :translate-alist '((template . html/post-page-template)))
  (org-export-define-derived-backend 'custom-blog-page-backend 'html
				     :translate-alist '((template . html/static-page-template))))

(defun blog/publish-all ()
  (package-manager/ensure-packages-installed 'org 'org-plus-contrib 'htmlize 'org-re-reveal)
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
    (org-publish-project "blog" t)
    (utilities/compile-latex-files blog-latex-files blog-publishing-directory)
    (utilities/copy-files-to-publishing-directory blog-copy-files blog-publishing-directory)))

(defun blog/setup-global-variables (blog-directory build-directory config)
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
	  blog-publishing-directory (expand-file-name build-directory blog-directory)
	  blog-copy-files (gethash "copy" files-config)
	  blog-latex-files (gethash "latex" files-config)
	  blog-css-url (gethash "css" settings-config)
	  blog-timestamps-directory (concat blog-directory "timestamps"))))

(defun initialize (config-location blog-directory build-directory)
  (let* ((config (utilities/read-json-file config-location))
	 (publish-config (gethash "publish" config)))
    (package-manager/setup)
    (package-manager/ensure-packages-installed 'seq)
    (blog/setup-global-variables blog-directory build-directory config)
    (blog/publish-all)))

(initialize
 (utilities/get-environment-variable "BLOG_CONFIG")
 (utilities/get-environment-variable "BLOG_DIRECTORY")
 (utilities/get-environment-variable "BLOG_BUILD_DIRECTORY"))
