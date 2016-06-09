---
layout: post
author: TJ Maynes
title: Jekyll is Awesome
date: 2014-07-12 00:00
published: true
---
*This post is for readers/friends who want to get started on their own blog, but don't know where to start.*

Over the years, I've had a countless number of conversations with programmer friends about how awesome Jekyll is and how awesome Github is for letting you host your own website/blog for free. However, the usual responses from friends include:
<ul>
<li>"Jekyll sounds too difficult to setup and maintain."</li>
<li>"what is git?"</li>
<li>"Git-what??"</li>
<li>"why not wordpress?"</li>
</ul>

<a href="http://www.jekyllrb.com">Jekyll</a> is a static page generator written in Ruby. In this post, we'll be using Jekyll to generate our blog. Also, with a few commands in terminal, you'll have a working blog up in less than a few minutes (depending on your connection speeds). Here is a <a href="http://jekyllrb.com/">link</a> to the Jekyll website.

<a href="http://www.git-scm.org/">git</a> is a open source version control system designed for small and large projects. Many developers/companies are using and supporting git everyday. I myself use git everyday for various small and large projects that I am working on. In this post, I will not get into the specifics of how to use git, however you can learn more about that by youtubing "git tutorials" or checking out this <a href="http://training.github.com/resources/videos/">link</a>.

<a href="http://www.github.com/">Github</a> is a website that allows you to contribute, share, and show off your git repositories to the world. Also, Github offers a free web hosting service called Github Pages, which is what you are going to use to get your Jekyll-based blog running on Github. Bonus: here is a <a href="http://tom.preston-werner.com/2009/05/19/the-git-parable.html">link</a> by Tom Preston-Werner called the Git Parable.

I'll get to why I don't recommend going the wordpress way at the conclusion of my post.

*Don't be afraid of what comes next. I'm going to try and explain everything as clearly as I can in this post!*

## Getting started with Jekyll

## Requirements

<ol>
<li>Your machine is using a unix/linux operating system.</li>
<li>You are used to using a command line interface.</li>
<li>You have ruby installed on your machine.</li>
<li>Today, is not the first day you have heard the term HTML, CSS, or Javascript (or ruby).</li>
</ol>

## Part One
Check to see that you have ruby gems installed on your machine. You can check by finding the verion number typing using the following command and pressing enter in Terminal.
{% highlight bash %}
gem -v
{% endhighlight %}

If you have ruby gems installed on your computer, please skip to Part three. If you <span style="font-weight:800;">don't</span> have ruby gems (no version number appeared) then, please read Part two.

## Part Two
Install ruby gems by going to their <a href="http://rubygems.org/pages/download">website</a> and downloading the TGZ file. After downloading, change directory in terminal to designated download folder. Next, copy and paste these command into terminal and press enter:
{% highlight bash %}
tar xvzf rubygems-{ version_number }.tgz
cd rubygems-{ version_number }
sudo ruby setup.rb
{% endhighlight %}
Ruby gems should now be installed on your machine, check by entering the following command:
{% highlight bash %}
gem -v
{% endhighlight %}
You should now see a version number, which will prepare you for Part three.

## Part Three (Installing Jekyll)
Install Jekyll on your machine by entering this command in terminal (within a desired directory):
{% highlight bash %}
gem install jekyll
{% endhighlight %}
Next, change your directory to your documents (or where you please). Then type the following and press enter:
{% highlight bash %}
jekyll new my-site
{% endhighlight %}
This will create a jekyll folder called my-site. Next, change directory to my-site, then enter the following command:
{% highlight bash %}
jekyll serve -w
{% endhighlight %}
This will generate the static files from that directory into a working website folder called _site and will start a ruby webserver pointing to that folder.

Next, go to your web-browser and type "localhost:4000" and press enter.

<img src="/img/localhost-screenshot.png">
You should be staring at what the above example displays. Congratulations! This might just be your first jekyll-based website!

*If you have any problems setting up Jekyll, read the documentation page <a href="http://jekyllrb.com/docs/quickstart/">here</a>!*

## Getting started with git
You're going to need git on your machine. If you already have git on your machine then you should skip to the Important Git Commands section.

For OSX users, go to this <a href="http://git-scm.com/download/mac">link</a> to download git directly from the git website.

For Linux users, in terminal enter:
{% highlight bash %}
sudo apt-get install git
{% endhighlight %}
*First, make sure you update and upgrade your packages to get the latest version of git!*

## Git Commands

The three git commands you will primarily use for "publishing" your website/blog:
<ul>
<li><a href="https://www.kernel.org/pub/software/scm/git/docs/git-add.html">git add --all</a></li>
<li><a href="https://www.kernel.org/pub/software/scm/git/docs/git-commit.html">git commit -m "latest update"</a></li>
<li><a href="https://www.kernel.org/pub/software/scm/git/docs/git-push.html">git push origin master</a></li>
<li>Bonus: <a href="http://www.git-scm.com/book/en/Git-Branching-Basic-Branching-and-Merging">git checkout branch</a></li>
</ul>

Again, I will not be going over how git works because that is a blog post in itself. Take some time learning git <span style="font-weight:800;">before</span> getting started with Github (it won't take long to understand how git works)!

Check out this <a href="http://git-scm.com/book/en/Getting-Started-About-Version-Control">link</a> for more information on using git.

## Conclusion
My biggest reason against using a wordpress as a blog platform for a software developer is that its just not modern platform anymore. Whereas with Jekyll, you learn alot about modern (important) technologies like git and github along the way (as well as a little Ruby). I highly recommend that you pursue learning more about web design and development regardless of your current title.

Feel free to send me an email if I have made any mistakes in this post! Thanks for reading!

Also, check out this <a href="https://help.github.com/categories/20/articles">link</a> for more information on using the free website hosting service, Github Pages!

*Sources:*
<ul>
<li><a href="http://rubygems.org/">Ruby Gems</a></li>
<li><a href="http://jekyllrb.com/docs/">Jekyll Docs</a></li>
<li><a href="https://pages.github.com/">Github Pages</a></li>
<li><a href="http://training.github.com/resources/videos/">Github Training Videos</a></li>
<li><a href="http://tom.preston-werner.com/2009/05/19/the-git-parable.html">The Git Parable</a></li>
</ul>

# update!
Follow this <a href="https://github.com/barryclark/jekyll-now">link</a> to try out Jekyll without touching the commandline!
