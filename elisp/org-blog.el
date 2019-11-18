(provide 'org-blog)

(require 'seq)
(require 'html-components)

(defun org-blog/org-add-link-types ()
  (org-add-link-type
   "youtube-video"
   (lambda (handle)
     (browse-url
      (concat "https://www.youtube.com/embed/"
	      handle)))
   (lambda (path desc backend)
     (cl-case backend
       (html (format html-components/video-wrapper
		     path (or desc "")))
       (latex (format "\href{%s}{%s}"
		      path (or desc "video")))))))

(defun org-blog/org-publish-to-html (plist filename pub-dir)
  (let ((parent-directory (utilities/get-relative-parent-directory filename))
	(posts-dir (expand-file-name "posts" pub-dir)))
    (cond ((equal parent-directory "posts")
	   (if (equal (file-name-base filename) "index")
	       (org-publish-org-to 'custom-blog-index-backend filename ".html" plist pub-dir)
	     (org-publish-org-to 'custom-blog-post-backend filename ".html" plist posts-dir)))
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
    (format "@@html:<span class=\"archive-item\"><span class=\"archive-date\">@@ %s @@html:</span>@@ | [[file:%s][%s]] @@html:</span>@@"
	    datetime post-entry title)))

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
     :base-directory ,(expand-file-name "posts" blog-directory)
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
    ("blog-public"
     :base-directory ,(expand-file-name "static" blog-directory)
     :exclude ,(regexp-opt '("public"))
     :base-extension ,(regexp-opt '("jpg" "png" "js" "css" "eot" "woff" "woff2" "ttf"))
     :publishing-directory ,(expand-file-name (format "%s/public" build-directory) blog-directory)
     :publishing-function org-publish-attachment
     :recursive t)
    ("blog-talks"
     :base-directory ,(expand-file-name "talks" blog-directory)
     :base-extension "org"
     :publishing-directory ,(expand-file-name (format "%s/talks" build-directory) blog-directory)
     :publishing-function org-blog/org-reveal-publish-to-html
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

(defun org-blog/setup-custom-templates ()
  (org-blog/org-add-link-types)
  (org-export-define-derived-backend 'custom-blog-index-backend 'html
				     :translate-alist '((template . html-components/index-page-template)))
  (org-export-define-derived-backend 'custom-blog-post-backend 'html
				     :translate-alist '((template . html-components/post-page-template)))
  (org-export-define-derived-backend 'custom-blog-page-backend 'html
				     :translate-alist '((template . html-components/static-page-template))))

(defun org-blog/publish ()
  (package-manager/ensure-packages-installed 'org 'org-plus-contrib 'htmlize 'org-re-reveal)
  (require 'ox)
  (require 'org)
  (require 'htmlize)
  (require 'seq)
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
    (org-publish-project "blog" t)))
