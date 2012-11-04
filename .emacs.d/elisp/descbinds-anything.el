<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml"><head><title>EmacsWiki: descbinds-anything.el</title><link rel="alternate" type="application/wiki" title="Edit this page" href="http://www.emacswiki.org/emacs?action=edit;id=descbinds-anything.el" /><link type="text/css" rel="stylesheet" href="/emacs/wiki.css" /><meta name="robots" content="INDEX,FOLLOW" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki" href="http://www.emacswiki.org/emacs?action=rss" /><link rel="alternate" type="application/rss+xml" title="EmacsWiki: descbinds-anything.el" href="http://www.emacswiki.org/emacs?action=rss;rcidonly=descbinds-anything.el" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content"
      href="http://www.emacswiki.org/emacs/full.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki with page content and diff"
      href="http://www.emacswiki.org/emacs/full-diff.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Emacs Wiki including minor differences"
      href="http://www.emacswiki.org/emacs/minor-edits.rss" />
<link rel="alternate" type="application/rss+xml"
      title="Changes for descbinds-anything.el only"
      href="http://www.emacswiki.org/emacs?action=rss;rcidonly=descbinds-anything.el" /><meta http-equiv="Content-Type" content="text/html; charset=utf-8" /></head><body class="http://www.emacswiki.org/emacs"><div class="header"><a class="logo" href="http://www.emacswiki.org/emacs/SiteMap"><img class="logo" src="/emacs_logo.png" alt="[Home]" /></a><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/Glossary">Glossary</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span>
<!-- Google CSE Search Box Begins  -->
<form class="tiny" action="http://www.google.com/cse" id="searchbox_004774160799092323420:6-ff2s0o6yi"><p>
<input type="hidden" name="cx" value="004774160799092323420:6-ff2s0o6yi" />
<input type="text" name="q" size="25" />
<input type="submit" name="sa" value="Search" />
</p></form>
<script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=searchbox_004774160799092323420%3A6-ff2s0o6yi"></script>
<!-- Google CSE Search Box Ends -->
<br /><span class="specialdays">Ukraine, National Day</span><h1><a title="Click to search for references to this page" rel="nofollow" href="http://www.google.com/cse?cx=004774160799092323420:6-ff2s0o6yi&amp;q=%22descbinds-anything.el%22">descbinds-anything.el</a></h1></div><div class="wrapper"><div class="wrapper"><div class="content browse"><p class="download"><a href="download/descbinds-anything.el">Download</a></p><pre class="code"><span class="linecomment">;;; descbinds-anything.el --- Yet Another `describe-bindings' with `anything'.</span>

<span class="linecomment">;; Copyright (C) 2008, 2009, 2010  Taiki SUGAWARA &lt;buzz.taiki@gmail.com&gt;</span>

<span class="linecomment">;; Author: Taiki SUGAWARA &lt;buzz.taiki@gmail.com&gt;</span>
<span class="linecomment">;; Keywords: anything, help</span>
<span class="linecomment">;; Version: 1.05</span>
<span class="linecomment">;; Time-stamp: &lt;2010-02-05 15:00:10  taiki&gt;</span>
<span class="linecomment">;; URL: http://www.emacswiki.org/cgi-bin/wiki/descbinds-anything.el</span>
<span class="linecomment">;; URL: http://bitbucket.org/buzztaiki/elisp/src/tip/descbinds-anything.el</span>

<span class="linecomment">;; This file is free software; you can redistribute it and/or modify</span>
<span class="linecomment">;; it under the terms of the GNU General Public License as published by</span>
<span class="linecomment">;; the Free Software Foundation; either version 3, or (at your option)</span>
<span class="linecomment">;; any later version.</span>

<span class="linecomment">;; This file is distributed in the hope that it will be useful,</span>
<span class="linecomment">;; but WITHOUT ANY WARRANTY; without even the implied warranty of</span>
<span class="linecomment">;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the</span>
<span class="linecomment">;; GNU General Public License for more details.</span>

<span class="linecomment">;; You should have received a copy of the GNU General Public License</span>
<span class="linecomment">;; along with GNU Emacs; see the file COPYING.  If not, write to</span>
<span class="linecomment">;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,</span>
<span class="linecomment">;; Boston, MA 02110-1301, USA.</span>

<span class="linecomment">;;; Commentary:</span>
<span class="linecomment">;; This package is a replacement of `describe-bindings'.</span>

<span class="linecomment">;;; Usage:</span>
<span class="linecomment">;; Add followings on your .emacs.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;   (require 'descbinds-anything)</span>
<span class="linecomment">;;   (descbinds-anything-install)</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; Now, `describe-bindings' is replaced to `descbinds-anything'. Type</span>
<span class="linecomment">;; `C-h b', `C-x C-h' these run `descbinds-anything'.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;; In the Anything buffer, you can select key-binds with anything interface.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;  - When type RET, selected candidate command is executed.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;  - When type ESC, You can "Execute", "Describe Function" or "Find</span>
<span class="linecomment">;;    Function" by the menu.</span>
<span class="linecomment">;;</span>
<span class="linecomment">;;  - When type C-z, selected command is described without quiting.</span>

<span class="linecomment">;;; History:</span>
<span class="linecomment">;; 2010-02-05   Taiki SUGAWARA  &lt;sugawara_t@ariel-networks.com&gt;</span>
<span class="linecomment">;; </span>
<span class="linecomment">;;   * descbinds-anything.el: Version 1.05</span>
<span class="linecomment">;;   bug fix.</span>
<span class="linecomment">;; </span>
<span class="linecomment">;; 2010-02-02 UTC  Taiki SUGAWARA  &lt;buzz.taiki@gmail.com&gt;</span>
<span class="linecomment">;; </span>
<span class="linecomment">;;   * descbinds-anything.el: Version 1.04</span>
<span class="linecomment">;;   add sorting feature.</span>
<span class="linecomment">;;   separete sorce creation function.</span>
<span class="linecomment">;;   add persistent action.</span>
<span class="linecomment">;; </span>
<span class="linecomment">;; 2009-03-29 UTC  Taiki SUGAWARA  &lt;buzz.taiki@gmail.com&gt;</span>
<span class="linecomment">;; </span>
<span class="linecomment">;;   * descbinds-anything.el: Version 1.03</span>
<span class="linecomment">;;   fix typo.</span>
<span class="linecomment">;; </span>
<span class="linecomment">;; 2008-11-16 UTC  Taiki SUGAWARA  &lt;buzz.taiki@gmail.com&gt;</span>
<span class="linecomment">;; </span>
<span class="linecomment">;;   * descbinds-anything.el: Version 1.02</span>
<span class="linecomment">;;   bound `indent-tabs-mode` to t for nil environment.</span>
<span class="linecomment">;; </span>
<span class="linecomment">;; 2008-11-16 UTC  Taiki SUGAWARA  &lt;buzz.taiki@gmail.com&gt;</span>
<span class="linecomment">;; </span>
<span class="linecomment">;;   * descbinds-anything.el: fix infinitive-loop when binding-line</span>
<span class="linecomment">;;   has not tab.</span>

<span class="linecomment">;;; Code:</span>

(require 'anything)

(defgroup descbinds-anything nil
  "<span class="quote">Yet Another `describe-bindings' with `anything'.</span>"
  :prefix "<span class="quote">descbinds-anything-</span>"
  :group 'anything)

(defcustom descbinds-anything-actions
  '(("<span class="quote">Execute</span>" . descbinds-anything-action:execute)
    ("<span class="quote">Describe Function</span>" . descbinds-anything-action:describe)
    ("<span class="quote">Find Function</span>" . descbinds-anything-action:find-func))
  "<span class="quote">Actions of selected candidate.</span>"
  :type '(repeat
	  (cons
	   :tag "<span class="quote">Action</span>"
	   (string :tag "<span class="quote">Name</span>")
	   (function :tag "<span class="quote">Function</span>")))
  :group 'descbinds-anything)

(defcustom descbinds-anything-candidate-formatter
  'descbinds-anything-default-candidate-formatter
  "<span class="quote">Candidate formatter function.
This function called two argument KEY and BINDING.</span>"
  :type 'function
  :group 'descbinds-anything)


(defcustom descbinds-anything-window-style 'one-window
  "<span class="quote">Window splitting style.</span>"
  :type '(choice
	  (const :tag "<span class="quote">One Window</span>" one-window)
	  (const :tag "<span class="quote">Same Window</span>" same-window)
	  (const :tag "<span class="quote">Split Window</span>" split-window))
  :group 'descbinds-anything)


(defcustom descbinds-anything-section-order
  '("<span class="quote">Major Mode Bindings</span>" "<span class="quote">Minor Mode Bindings</span>" "<span class="quote">Global Bindings</span>")
  "<span class="quote">A list of section order by name regexp.</span>"
  :type '(repeat (regexp :tag "<span class="quote">Regexp</span>"))
  :group 'descbinds-anything)

(defcustom descbinds-anything-source-template
  '((candidate-transformer . descbinds-anything-transform-candidates)
    (persistent-action . descbinds-anything-action:describe)
    (action-transformer . descbinds-anything-transform-actions))
  "<span class="quote">A template of `descbinds-anything' source.</span>"
  :type 'sexp
  :group 'descbinds-anything)


(defun descbinds-anything-all-sections (buffer &optional prefix menus)
  (with-temp-buffer
    (let ((indent-tabs-mode t))
      (describe-buffer-bindings buffer prefix menus))
    (goto-char (point-min))
    (let ((header-p (not (= (char-after) ?\f)))
	  sections header section)
      (while (not (eobp))
	(cond
	 (header-p
	  (setq header (buffer-substring-no-properties
			(point)
			(line-end-position)))
	  (setq header-p nil)
	  (forward-line 3))
	 ((= (char-after) ?\f)
	  (push (cons header (nreverse section)) sections)
	  (setq section nil)
	  (setq header-p t))
	 ((looking-at "<span class="quote">^[ \t]*$</span>")
	  <span class="linecomment">;; ignore</span>
	  )
	 (t
	  (let ((binding-start (save-excursion
				 (and (re-search-forward "<span class="quote">\t+</span>" nil t)
				      (match-end 0))))
		key binding)
	    (when binding-start
	      (setq key (buffer-substring-no-properties (point) binding-start)
		    key (replace-regexp-in-string"<span class="quote">^[ \t\n]+</span>" "<span class="quote"></span>" key)
		    key (replace-regexp-in-string"<span class="quote">[ \t\n]+$</span>" "<span class="quote"></span>" key))
	      (goto-char binding-start)
	      (setq binding (buffer-substring-no-properties
			     binding-start
			     (line-end-position)))
	      (unless (member binding '("<span class="quote">self-insert-command</span>"))
		(push (cons key binding) section))))))
	(forward-line))
      (push (cons header (nreverse section)) sections)
      (nreverse sections))))

(defun descbinds-anything-action:execute (candidate)
  "<span class="quote">An action that execute selected CANDIDATE command.</span>"
  (call-interactively (cdr candidate)))

(defun descbinds-anything-action:describe (candidate)
  "<span class="quote">An action that describe selected CANDIDATE function.</span>"
  (describe-function (cdr candidate)))

(defun descbinds-anything-action:find-func (candidate)
  "<span class="quote">An action that find selected CANDIDATE function.</span>"
  (find-function (cdr candidate)))

(defun descbinds-anything-default-candidate-formatter (key binding)
  "<span class="quote">Default candidate formatter.</span>"
  (format "<span class="quote">%-10s\t%s</span>" key binding))

(defun descbinds-anything-sort-sections (sections)
  (flet ((order (x)
		(loop for n = 0 then (1+ n)
		      for regexp in descbinds-anything-section-order
		      if (and (car x) (string-match regexp (car x))) return n
		      finally return n)))
    (sort sections (lambda (a b)
		     (&lt; (order a) (order b))))))

(defun descbinds-anything-transform-candidates (candidates)
  (mapcar
   (lambda (pair)
     (cons (funcall descbinds-anything-candidate-formatter
		    (car pair) (cdr pair))
	   (cons (car pair) (intern-soft (cdr pair)))))
   candidates))

(defun descbinds-anything-transform-actions (actions candidate)
  (and (commandp (cdr candidate)) (or actions descbinds-anything-actions)))

(defun descbinds-anything-sources (buffer &optional prefix menus)
  (mapcar
   (lambda (section)
     (descbinds-anything-source (car section) (cdr section)))
   (descbinds-anything-sort-sections
    (descbinds-anything-all-sections buffer prefix menus))))

(defun descbinds-anything-source (name candidates)
  `((name . ,name)
    (candidates . ,candidates)
    ,@descbinds-anything-source-template))

(defun descbinds-anything (&optional prefix buffer)
  "<span class="quote">Yet Another `describe-bindings' with `anything'.</span>"
  (interactive)
  (let ((anything-sources (descbinds-anything-sources
			   (or buffer (current-buffer))
			   prefix nil))
	(anything-samewindow (and (not (minibufferp))
				  (memq descbinds-anything-window-style
					'(same-window one-window))))
	(anything-before-initialize-hook
	 (if (and (not (minibufferp))
		  (eq descbinds-anything-window-style 'one-window))
	     (cons 'delete-other-windows anything-before-initialize-hook)
	   anything-before-initialize-hook)))
    (anything)))

(defvar descbinds-anything-Orig-describe-bindings
  (symbol-function 'describe-bindings))

(defun descbinds-anything-install ()
  "<span class="quote">Use `descbinds-anything' as a replacement of `describe-bindings'.</span>"
  (interactive)
  (fset 'describe-bindings 'descbinds-anything))

(defun descbinds-anything-uninstall ()
  "<span class="quote">Restore original `describe-bindings'.</span>"
  (interactive)
  (fset 'describe-bindings descbinds-anything-Orig-describe-bindings))

(provide 'descbinds-anything)

<span class="linecomment">;; How to save (DO NOT REMOVE!!)</span>
<span class="linecomment">;; (emacswiki-post "descbinds-anything.el")</span>

<span class="linecomment">;;; descbinds-anything.el ends here</span></span></pre></div><div class="wrapper close"></div></div><div class="wrapper close"></div></div><div class="footer"><hr /><span class="gotobar bar"><a class="local" href="http://www.emacswiki.org/emacs/SiteMap">SiteMap</a> <a class="local" href="http://www.emacswiki.org/emacs/Search">Search</a> <a class="local" href="http://www.emacswiki.org/emacs/ElispArea">ElispArea</a> <a class="local" href="http://www.emacswiki.org/emacs/HowTo">HowTo</a> <a class="local" href="http://www.emacswiki.org/emacs/Glossary">Glossary</a> <a class="local" href="http://www.emacswiki.org/emacs/RecentChanges">RecentChanges</a> <a class="local" href="http://www.emacswiki.org/emacs/News">News</a> <a class="local" href="http://www.emacswiki.org/emacs/Problems">Problems</a> <a class="local" href="http://www.emacswiki.org/emacs/Suggestions">Suggestions</a> </span><span class="translation bar"><br />  <a class="translation new" rel="nofollow" href="http://www.emacswiki.org/emacs?action=translate;id=descbinds-anything.el;missing=de_en_es_fr_it_ja_ko_pt_ru_se_zh">Add Translation</a></span><span class="edit bar"><br /> <a class="comment local" href="http://www.emacswiki.org/emacs/Comments_on_descbinds-anything.el">Talk</a> <a class="edit" accesskey="e" title="Click to edit this page" rel="nofollow" href="http://www.emacswiki.org/emacs?action=edit;id=descbinds-anything.el">Edit this page</a> <a class="history" rel="nofollow" href="http://www.emacswiki.org/emacs?action=history;id=descbinds-anything.el">View other revisions</a> <a class="admin" rel="nofollow" href="http://www.emacswiki.org/emacs?action=admin;id=descbinds-anything.el">Administration</a></span><!-- test --><span class="time"><br /> Last edited 2010-02-05 15:10 UTC by <a class="author" title="from ckp0.ariel-networks.com" href="http://www.emacswiki.org/emacs/Sugawara">Sugawara</a> <a class="diff" rel="nofollow" href="http://www.emacswiki.org/emacs?action=browse;diff=2;id=descbinds-anything.el">(diff)</a></span><div style="float:right; margin-left:1ex;">
<!-- Creative Commons License -->
<a href="http://creativecommons.org/licenses/GPL/2.0/"><img alt="CC-GNU GPL" style="border:none" src="/pics/cc-GPL-a.png" /></a>
<!-- /Creative Commons License -->
</div>

<!--
<rdf:RDF xmlns="http://web.resource.org/cc/"
 xmlns:dc="http://purl.org/dc/elements/1.1/"
 xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<Work rdf:about="">
   <license rdf:resource="http://creativecommons.org/licenses/GPL/2.0/" />
  <dc:type rdf:resource="http://purl.org/dc/dcmitype/Software" />
</Work>

<License rdf:about="http://creativecommons.org/licenses/GPL/2.0/">
   <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
   <permits rdf:resource="http://web.resource.org/cc/Distribution" />
   <requires rdf:resource="http://web.resource.org/cc/Notice" />
   <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
   <requires rdf:resource="http://web.resource.org/cc/ShareAlike" />
   <requires rdf:resource="http://web.resource.org/cc/SourceCode" />
</License>
</rdf:RDF>
-->

<p class="legal">
This work is licensed to you under version 2 of the
<a href="http://www.gnu.org/">GNU</a> <a href="/GPL">General Public License</a>.
Alternatively, you may choose to receive this work under any other
license that grants the right to use, copy, modify, and/or distribute
the work, as long as that license imposes the restriction that
derivative works have to grant the same rights and impose the same
restriction. For example, you may choose to receive this work under
the
<a href="http://www.gnu.org/">GNU</a>
<a href="/FDL">Free Documentation License</a>, the
<a href="http://creativecommons.org/">CreativeCommons</a>
<a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
License, the XEmacs manual license, or
<a href="/OLD">similar licenses</a>.
</p>
</div>
</body>
</html>
