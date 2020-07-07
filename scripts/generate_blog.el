;; --- generate_blog.el ---
;; Author: TJ Maynes <tjmaynes at gmail dot com>
;; Website: https://tjmaynes.com/

(require 'json)

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

(defun utilities/get-environment-variable (env-name)
  (let ((value (getenv env-name)))
    (if (not value) (error (format "Missing environment variable: %s." env-name)))
    value))

(defun utilities/ensure-program-exists (program)
  (let ((value (executable-find program)))
    (if (not value) (error (format "Program not available: %s." program)))
    value))

(defun utilities/ensure-directory-exists (directory)
  (unless (file-directory-p directory)
    (make-directory directory :parents))
  directory)

(defun utilities/read-json-file (json-file)
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

(defvar org-blog/video-wrapper
  (concat
   "<div class=\"video-wrapper\">"
   (concat "<iframe"
	   " src=\"https://www.youtube.com/embed/%s\""
	   " frameborder=\"0\""
	   " allowfullscreen>%s</iframe>")
   "</div>"))

(defun org-blog/get-head (title description)
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
<meta property=\"og:image\" content=\"" (format "%s/%s" blog-url blog-author-avatar) "\" />
<meta property=\"twitter:title\" content=\"" title "\" />
<meta property=\"twitter:url\" content=\"https://tjmaynes.com/index.html\" />
<meta property=\"twitter:image\" content=\"" (format "%s/%s" blog-url blog-author-avatar) "\" />
<meta property=\"twitter:description\" content=\"" description "\" />
<meta property=\"twitter:card\" content=\"" description "\" />
<link rel=\"stylesheet\" type=\"text/css\" href=\"" blog-css-url "\">
<link rel=\"stylesheet\" type=\"text/css\" href=\"" blog-syntax-css-url "\">")
   "</head>\n"))

(defun org-blog/get-header ()
  (concat
   "<div class=\"content-header\">\n"
   (concat
    "<nav>
     <ul>
       <li><a href=\"/\"><img src=" blog-icon " alt=\"icon\" /></a></li>
     </ul>
     <ul>
       <li><a href=\"/\">Home</a></li>
       <li><a href=\"https://github.com/" blog-author-github "\" target=\"_blank\">Code</a></li>
       <li><a href=\"/rss.xml\">Feed</a></li>
     </ul>
   </nav>\n")
   "</div>\n"))

(defun org-blog/get-footer ()
  (concat
   "<div class=\"content-footer\">
     <p>" blog-author-description "</p>
     <p><a href=" blog-source-code-url ">Built using Org-Publish</a> ❤️</p>
    </div>"))

(defun org-blog/get-body (content)
  (concat
   "<body>\n"
   (concat
    "<section class=\"content-wrapper\">\n"
    (org-blog/get-header)
    content
    (org-blog/get-footer)
    "</div>\n")
   "</body>\n"))

(defun org-blog/get-post-header (post-title post-date)
  (let* ((xml-date-time (utilities/org-parse-and-format-date post-date "%F"))
	 (display-date-time (utilities/org-parse-and-format-date post-date "%Y-%m-%d")))
    (concat
     "<header>\n"
     (concat
      "<h1 itemprop=\"name headline\">" post-title "</h1>\n"
      "<p>Posted on <time datetime=\"" xml-date-time "\" itemprop=\"datePublished\">" display-date-time "</time> • " blog-author-name "</p>\n")
     "</header>\n")))

(defun org-blog/get-post-page-body (title date content)
  (concat
   "<article class=\"post\">\n"
   (org-blog/get-post-header title date)
   content
   "</article>\n"))

(defun org-blog/get-html (head body language)
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" language)
   head
   body
   "</html>\n"))

(defun org-blog/get-static-page-body (title date content)
  (concat
   "<article class=\"post\">\n"
   content
   "</article>\n"))

(defun org-blog/base-html-template (title description language content)
  (org-blog/get-html
   (org-blog/get-head title description)
   (org-blog/get-body content)
   language))

(defun org-blog/index-page-template (content info)
  (let* ((language (plist-get info :language)))
    (org-blog/base-html-template
     blog-title
     blog-description
     language
     (concat
      "<div class=\"archive\">\n"
      content
      "</div>\n"))))

(defun org-blog/post-src-block (code content info)
  (let ((src-block (org-element-property :value code))
	(language (or (org-element-property :language code) ""))
	(pygments (utilities/ensure-program-exists "pygmentize"))
	(temp-file (format "/tmp/pygmentize-%s.txt" (md5 (current-time-string)))))
    (with-temp-file temp-file (insert src-block))
    (shell-command-to-string (format "%s -l \"%s\" -f html %s" pygments language temp-file))))

(defun org-blog/post-page-template (content info)
  (let* ((title (utilities/org-get-file-keyword "TITLE"))
	 (date (utilities/org-get-file-keyword "DATE"))
	 (description (utilities/org-get-file-keyword "DESCRIPTION"))
	 (language (plist-get info :language)))
    (org-blog/base-html-template
     title
     description
     language
     (org-blog/get-post-page-body title date content))))

(defun org-blog/static-page-template (content info)
  (let* ((title (utilities/org-get-file-keyword "TITLE"))
	 (date (utilities/org-get-file-keyword "DATE"))
	 (description (utilities/org-get-file-keyword "DESCRIPTION"))
	 (language (plist-get info :language)))
    (org-blog/base-html-template
     title
     description
     language
     (org-blog/get-static-page-body title date content))))

(defun org-blog/org-add-link-types ()
  (org-add-link-type
   "youtube-video"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
	      handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format org-blog/video-wrapper
		     path (or desc "")))
       (latex (format "\href{%s}{%s}"
		      path (or desc "video")))))))

(defun org-blog/org-publish-to-html (plist filename pub-dir)
  (let* ((parent-directory (utilities/get-relative-parent-directory filename))
	 (posts-directory (if (equal parent-directory "posts")
			      (expand-file-name (format "%s/posts" blog-publishing-directory) blog-source-directory)
			    pub-dir)))
    (cond ((or (equal parent-directory "posts") (equal parent-directory "drafts"))
	   (if (equal (file-name-base filename) "index")
	       (org-publish-org-to 'custom-blog-index-backend filename ".html" plist pub-dir)
	     (org-publish-org-to 'custom-blog-post-backend filename ".html" plist posts-directory)))
	  ((org-publish-org-to 'custom-blog-page-backend filename ".html" plist pub-dir)))))

(defun org-blog/org-publish-sitemap (_title list)
  (mapconcat (lambda (li)
	       (format "@@html:<li class=\"archive-item\">@@%s@@html:</li>@@" (car li)))
	     (seq-filter #'car (cdr list))
	     "\n"))

(defun org-blog/org-publish-sitemap-format (entry style project)
  (let ((datetime (format-time-string "%Y-%m-%d" (org-publish-find-date entry project)))
	(title (org-publish-find-title entry project))
	(post-entry (format "posts/%s" entry)))
    (format "@@html:<span class=\"archive-content\"><span class=\"archive-date\">@@ %s @@html:</span>@@ | [[file:%s][%s]] @@html:</span>@@" datetime post-entry title)))

(defun org-blog/org-publish-format-rss-feed (title list)
  (concat "#+TITLE: " title "\n"
	  "#+DESCRIPTION: " blog-description "\n\n"
          (org-list-to-subtree list '(:icount "" :istart ""))))

(defun org-blog/org-publish-format-rss-feed-entry (entry style project)
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

(defun org-blog/org-rss-publish-to-rss (plist filename pub-dir)
  (if (equal "rss.org" (file-name-nondirectory filename))
      (org-rss-publish-to-rss plist filename pub-dir)))

(defun org-blog/org-reveal-get-title-page (title author date)
  (concat
   "<section id='sec-title-slide'>"
   (concat
    "<h1 class='title>" title "</h1>")
   (concat
    "<h2 class='author'>" author "</h2>")
   (concat
    "<h3 class='date'>" date "</h3>")
   "</section>"))

(defun org-blog/org-reveal-publish-to-html (plist filename pub-dir)
  (require 'org-re-reveal)
  (org-publish-org-to 're-reveal filename ".html" plist pub-dir))

(defun org-blog/get-publish-project-alist ()
  `(("blog-home"
     :base-directory ,(expand-file-name "posts" blog-source-directory)
     :base-extension "org"
     :exclude ,(regexp-opt '("index.org" "rss.org"))
     :publishing-function org-blog/org-publish-to-html
     :publishing-directory ,blog-publishing-directory
     :html-home/up-format nil
     :auto-sitemap t
     :sitemap-filename "index.org"
     :sitemap-title ,blog-title
     :sitemap-style list
     :sitemap-sort-files anti-chronologically
     :sitemap-function org-blog/org-publish-sitemap
     :sitemap-format-entry org-blog/org-publish-sitemap-format)
    ("blog-post-images"
     :base-directory ,(expand-file-name "posts/images" blog-source-directory)
     :exclude nil
     :base-extension ,(regexp-opt '("jpg" "png"))
     :publishing-directory ,(expand-file-name (format "%s/posts/images" blog-publishing-directory) blog-source-directory)
     :publishing-function org-publish-attachment
     :recursive nil)
    ("blog-rss"
     :base-directory ,(expand-file-name "posts" blog-source-directory)
     :base-extension "org"
     :recursive nil
     :exclude ,(regexp-opt '("index.org" "rss.org"))
     :publishing-function org-blog/org-rss-publish-to-rss
     :publishing-directory ,blog-publishing-directory
     :rss-extension "xml"
     :auto-sitemap t
     :sitemap-filename "rss.org"
     :sitemap-title ,blog-title
     :sitemap-style list
     :sitemap-sort-files anti-chronologically
     :sitemap-function org-blog/org-publish-format-rss-feed
     :sitemap-format-entry org-blog/org-publish-format-rss-feed-entry
     :table-of-contents nil)
    ("blog-drafts"
     :base-directory ,(expand-file-name "drafts" blog-source-directory)
     :base-extension "org"
     :exclude ,(regexp-opt '("index.org"))
     :publishing-function org-blog/org-publish-to-html
     :publishing-directory ,(expand-file-name (format "%s/drafts" blog-publishing-directory) blog-source-directory)
     :publishing-directory ,blog-publishing-directory
     :html-home/up-format nil)
    ("blog-talks"
     :base-directory ,(expand-file-name "talks" blog-source-directory)
     :base-extension "org"
     :publishing-directory ,(expand-file-name (format "%s/talks" blog-publishing-directory) blog-source-directory)
     :publishing-function org-blog/org-reveal-publish-to-html
     :section-numbers nil
     :recursive t)
    ("blog-talks-images"
     :base-directory ,(expand-file-name "talks/images" blog-source-directory)
     :exclude nil
     :base-extension ,(regexp-opt '("jpg" "png"))
     :publishing-directory ,(expand-file-name (format "%s/talks/images" blog-publishing-directory) blog-source-directory)
     :publishing-function org-publish-attachment
     :recursive nil)
    ("blog-public"
     :base-directory ,(expand-file-name "public" blog-directory)
     :base-extension ,(regexp-opt '("jpg" "png" "js" "css" "eot" "woff" "woff2" "ttf"))
     :publishing-directory ,(expand-file-name (format "%s/public" blog-publishing-directory) blog-directory)
     :publishing-function org-publish-attachment
     :recursive t)
    ("blog-required-files"
     :base-directory ,blog-directory
     :exclude ,(regexp-opt '("README.org"))
     :include ["favicon.ico" "CNAME"]
     :publishing-directory ,blog-publishing-directory
     :publishing-function org-publish-attachment
     :recursive nil)
    ("blog" :components ("blog-home" "blog-post-images" "blog-rss" "blog-drafts" "blog-talks" "blog-talks-images" "blog-public" "blog-required-files"))))

(defun org-blog/setup-custom-templates ()
  (org-export-define-derived-backend 'custom-blog-index-backend 'html
				     :translate-alist '((template . org-blog/index-page-template)))
  (org-export-define-derived-backend 'custom-blog-post-backend 'html
				     :translate-alist '((src-block . org-blog/post-src-block)
							(template . org-blog/post-page-template)))
  (org-export-define-derived-backend 'custom-blog-page-backend 'html
				     :translate-alist '((template . org-blog/static-page-template))))

(defun org-blog/publish ()
  (let* ((org-publish-project-alist          (org-blog/get-publish-project-alist))
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
    (org-blog/setup-custom-templates)
    (org-blog/org-add-link-types)
    (org-publish-project "blog" t)))

(defun setup-global-variables (config blog-directory blog-publishing-directory)
  (let* ((settings-config (gethash "settings" config))
	 (author-config (gethash "author" config))
	 (css-config (gethash "css" settings-config)))
    (setq blog-directory blog-directory
	  blog-publishing-directory blog-publishing-directory
	  blog-source-directory (expand-file-name "content" blog-directory)
	  blog-title (gethash "title" settings-config)
	  blog-description (gethash "description" settings-config)
	  blog-url (gethash "url" settings-config)
	  blog-source-code-url (gethash "source-code-url" settings-config)
	  blog-license-url (gethash "license-url" settings-config)
	  blog-icon (gethash "icon" settings-config)
	  blog-author-name  (gethash "name" author-config)
	  blog-author-email (gethash "email" author-config)
	  blog-author-github (gethash "github" author-config)
	  blog-author-linkedin (gethash "linkedin" author-config)
	  blog-author-twitter (gethash "twitter" author-config)	  
	  blog-author-description (gethash "description" author-config)
	  blog-author-cv (gethash "cv" author-config)	  
	  blog-author-avatar (gethash "avatar" author-config)
	  blog-css-url (gethash "main" css-config)
	  blog-syntax-css-url (gethash "syntax-highlighting" css-config)
	  blog-timestamps-directory (concat blog-source-directory "timestamps"))))

(defun install-required-packages ()
  (package-manager/setup)
  (package-manager/ensure-packages-installed 'el-get)
  (el-get-bundle blog-dependencies
    :url "https://gist.github.com/e0b13e010f4c34dd71c41a4f50e0e5cd.git")
  (require 'ox-rss)
  (require 'org-re-reveal)
  (require 'htmlize)
  (require 'seq))

(defun initialize (blog-directory blog-publishing-directory)
  (let* ((config (utilities/read-json-file (expand-file-name "config.json" blog-directory))))
    (install-required-packages)
    (setup-global-variables config blog-directory blog-publishing-directory)
    (org-blog/publish)))

(initialize
 (utilities/get-environment-variable "BLOG_DIRECTORY")
 (utilities/get-environment-variable "BLOG_PUBLISHING_DIRECTORY"))
