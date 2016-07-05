---
layout: post
author: TJ Maynes
title: 	Setting Up F# In Emacs
categories: computing
date: 2014-04-01 00:00:00
tags: emacs, fsharp, opensource, osx, mono
published: true
---
*This is a quick guide to setting up F# in Emacs on OSX! The goal of this guide is to make setting up F# in Emacs on OSX an easy experience.*

Before we begin, I am assuming you have the following already installed:
<ol>
<li>Homebrew</li>
<li>Emacs v24.3</li>
</ol>

# Installation
<ul>
<li>brew install mono</li>
<li>brew install fsharp</li>
</ul>

# Inside Emacs
Add the following lines to your .emacs file:
{% highlight common-lisp %}
(add-to-list 'load-path "~/.emacs.d/fsharp-mode/")
(autoload 'fsharp-mode "fsharp-mode"     "Major mode for editing F# code." t)
(add-to-list 'auto-mode-alist '("\\.fs[iylx]?$" . fsharp-mode))
(setq inferior-fsharp-program "/usr/local/bin/fsharpi --readline-")
(setq fsharp-compiler "/usr/local/bin/fsharpc")
{% endhighlight %}

Restart emacs and use the following commmands:
<ul>
<li>C-C C-S, for REPL interactive program.</li>
<li>C-C C-C, fsharpc filename.fs, to compile a F#(.fs) file to an executable.</li>
</ul>

# Inside eshell
<ul>
<li>Type command, fsharpi, and enter for REPL interactive program.</li>
<li>Type command, fsharpc filename.fs, and enter to compile a F#(.fs) file to an executable.</li>
<li>Type command, mono filename.exe, and enter to execute F# program.</li>
</ul>

# Lets test our setup!
<ol>
<li>Open Emacs and create a new fsharp file called Program.</li>
<li>Copy this sample code:</li>
{% highlight fsharp %}
// Code found at http://msdn.microsoft.com/en-us/library/dd233160.aspx
// file: Program.fs

let number = 5
let rec factorial n =
    if n = 0
    then 1
    else n * factorial (n - 1)
System.Console.WriteLine(factorial number)
{% endhighlight %}
<li>Save the file with C-X C-S.</li>
<li>Compile the file with C-C C-C.</li>
<li>Go into eshell (M-x eshell). </li>
<li>Type 'mono Program.exe' and press enter.</li>
<li>Done!</li>
</ol>

*If you get lost, here is a link to my emacs config [file](https://raw.githubusercontent.com/TJMaynes/config/master/.emacs). Also, checkout the <a href="http://fsharp.org/use/mac/">F# Documentation</a> for setting up F# on OSX!*
