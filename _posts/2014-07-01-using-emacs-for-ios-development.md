---
layout: post
author: TJ Maynes
title: Using Emacs for iOS Development
date: 2014-07-01 00:00
published: true
---
<span style="font-style: italic;">This post is primarily for iOS developers who are already familiar with Emacs and Xcode, yet want to code their objective-c applications in a distraction-free environment via Emacs.</span>

In this post, I will be going over how to use Emacs as your primary editor for iOS Development. For reference, here is a <a href="https://raw.githubusercontent.com/TJMaynes/config/master/.emacs">link</a> to my .emacs file.

Before we begin, I am going to assume you already have the installed following:
<ol>
<li>Xcode (latest stable)</li>
<li>Emacs v24.3</li>
</ol>

## Inside Xcode
First, you will need to build your project in Xcode.
Open Xcode and create a new Xcode project. Next, choose template: iOS > Application > Empty Application.
Finally, name the product my_app.

<img src="/img/xcode-screencast.gif">

## Inside Emacs
Next, add the following to your .emacs file:
{% highlight common-lisp %}
;;
;; Objective-C
;;
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'magic-mode-alist
	     `(,(lambda ()
		  (and (string= (file-name-extension buffer-file-name) "h")
		       (re-search-forward "@\\<interface\\>"
					  magic-mode-regexp-match-limit t)))
	       . objc-mode))
(require 'find-file) ;; for the "cc-other-file-alist" variable
(nconc (cadr (assoc "\\.h\\'" cc-other-file-alist)) '(".m" ".mm"))

(defadvice ff-get-file-name (around ff-get-file-name-framework
				    (search-dirs
				     fname-stub
				     &optional suffix-list))
  "Search for Mac framework headers as well as POSIX headers."
  (or
   (if (string-match "\\(.*?\\)/\\(.*\\)" fname-stub)
       (let* ((framework (match-string 1 fname-stub))
	      (header (match-string 2 fname-stub))
	      (fname-stub (concat framework ".framework/Headers/" header)))
	 ad-do-it))
   ad-do-it))
(ad-enable-advice 'ff-get-file-name 'around 'ff-get-file-name-framework)
(ad-activate 'ff-get-file-name)

(setq cc-search-directories '("." "../include" "/usr/include" "/usr/local/include/*"
			      "/System/Library/Frameworks" "/Library/Frameworks"))
{% endhighlight %}

Next, type "M-x load-file ~/.emacs" and press enter. Alternatively, exit Emacs and log back in.

That concludes this post, if you are interested in utilizing some of Apple's commandline tools to build out your project using Eshell or terminal check out the sources below. Also, feel free to send me an email or comment below if I have made any mistakes in this post! Thanks for reading!

Sources:
<ul>
<li><a href="http://www.emacswiki.org/emacs/ObjectiveCMode">Emacs Wiki</a></li>
<li><a href="https://developer.apple.com/library/ios/technotes/tn2339/_index.html">Building from Commandline</a></li>
<li><a href="https://developer.apple.com/library/mac/documentation/Darwin/Reference/ManPages/man1/xcodebuild.1.html">Apple Documentation for Xcodebuild</a></li>
</ul>
