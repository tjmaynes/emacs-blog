(provide 'html-components)

(defvar html-components/video-wrapper
  (concat
   "<div class=\"video-wrapper\">"
   (concat "<iframe"
	   " src=\"https://www.youtube.com/embed/%s\""
	   " frameborder=\"0\""
	   " allowfullscreen>%s</iframe>")
   "</div>"))

(defun html-components/get-head (title description)
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

(defun html-components/get-header ()
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

(defun html-components/get-footer ()
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
        <p><a href=\"https://github.com/tjmaynes/blog\">Built using Org-Mode ❤️</a></p>
      </li>
     </ul>
    </nav>")
   "</section>\n"))

(defun html-components/get-body (content)
  (concat
   "<body>\n"
   (concat
    "<div class=\"content-wrapper\">\n"
    (html-components/get-header)
    content
    (html-components/get-footer)
    "</div>\n")
   "</body>\n"))

(defun html-components/get-post-header (post-title post-date)
  (let* ((xml-date-time (utilities/org-parse-and-format-date post-date "%F"))
	 (display-date-time (utilities/org-parse-and-format-date post-date "%Y-%m-%d")))
    (concat
     "<header>\n"
     (concat
      "<h1 itemprop=\"name headline\">" post-title "</h1>\n"
      "<p>Posted on <time datetime=\"" xml-date-time "\" itemprop=\"datePublished\">" display-date-time "</time> • " blog-author-name "</p>\n")
   "</header>\n")))

(defun html-components/get-post-page-body (title date content)
  (concat
   "<div class=\"post\">\n"
   (html-components/get-post-header title date)
   content
   "</div>\n"))

(defun html-components/get-html (head body language)
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" language)
   head
   body
   "</html>\n"))

(defun html-components/get-static-page-body (title date content)
  (concat
   "<div class=\"post\">\n"
   content
   "</div>\n"))

(defun html-components/base-html-template (title description language content)
  (html-components/get-html
   (html-components/get-head title description)
   (html-components/get-body content)
   language))

(defun html-components/index-page-template (content info)
  (let* ((language (plist-get info :language)))
    (html-components/base-html-template
     blog-title
     blog-description
     language
     (concat
      "<div class=\"archive\">\n"
      content
      "</div>\n"))))

(defun html-components/post-page-template (content info)
  (let* ((title (utilities/org-get-file-keyword "TITLE"))
	 (date (utilities/org-get-file-keyword "DATE"))
	 (description (utilities/org-get-file-keyword "DESCRIPTION"))
	 (language (plist-get info :language)))
    (html-components/base-html-template
     title
     description
     language
     (html-components/get-post-page-body title date content))))

(defun html-components/static-page-template (content info)
  (let* ((title (utilities/org-get-file-keyword "TITLE"))
	 (date (utilities/org-get-file-keyword "DATE"))
	 (description (utilities/org-get-file-keyword "DESCRIPTION"))
	 (language (plist-get info :language)))
    (html-components/base-html-template
     title
     description
     language
     (html-components/get-static-page-body title date content))))
