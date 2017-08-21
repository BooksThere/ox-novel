;; ox-novel.el --- translate .org into .latex using utbook for writing novel

;; Copyright (C) 2016 BooksThere

;; Author: BooksThere <BooksThere@gmail.com>
;; Keywords: org, novel

;; This program is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This library implements a translator from the .org files into .pdf files for writing novel.
;; .tex file is simply the intermediate product.
;; LaTeX back-end for Org is simply the auxiliary tool.

;;; Code:
(require 'ox)


;; Define Back-End
(org-export-define-backend
    'novel
  '(
    (bold . org-novel-bold)
    (headline . org-novel-headline)
    (link . org-novel-link)
    (paragraph . org-novel-paragraph)
    (section . org-novel-section)
    (plain-text . org-novel-plain-text)
    (template . org-novel-template)
    (verbatim . org-novel-verbatim)
    )
  :menu-entry
  '(?n "Export to Novel"
       (
	(?n "As LaTeX file" org-novel-to-latex)
	(?N "As LaTeX Buffer" org-novel-to-buffer)
	(?p "As PDF file" org-novel-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (org-novel-export-to-pdf t s v b)
		(org-open-file (org-novel-export-to-pdf nil s v b)))))
	))
  :options-alist
  '(
    (:address "ADDRESS" nil org-novel-address)
    (:edition "EDITION" nil org-novel-edition)
    (:printer "PRINTER" nil org-novel-printer)
    (:published "PUBLISHED" nil org-novel-published)
    (:publisher "PUBLISHER" nil org-novel-publisher)
    (:subtitle "SUBTITLE" nil org-novel-subtitle)
    ;; options
    (:with-size nil "size" org-export-with-size)
    (:with-sec-prefix nil "sec-prefix" org-export-with-sec-prefix)
    ))


;; --------------------------------
;;  Custom Variables
;;
(defgroup oxn:export nil
  "Options for exporting Org mode files to LaTeX for Novel."
  :tag "Org Export Novel"
  :group 'oxn:export)

(defcustom oxn:coding-system 'utf-8
  "Coding system for Novel export.
Use utf-8 as the default value."
  :group 'oxn:export
  :version "24.5"
  :package-version '(Org . "8.0")
  :type 'coding-system)


(defcustom org-novel-address ""
  "Address of author."
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-novel-edition ""
  "Edition."
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-novel-subtitle ""
  "Subtitle of the book."
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-novel-printer ""
  "Printer."
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-novel-published ""
  "Published date."
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-novel-publisher ""
  "Publisher."
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-export-with-size 'normal
  "Font size."
  :group 'org-export-novel
  :type '(choice
	  (const :tag "small size" small)
	  (const :tag "normal size" normal)
	  (const :tag "large size" large)))

(defcustom org-export-with-sec-prefix ""
  "Prefix of section's text."
  :group 'org-export-novel
  :type '(string :tag "String"))


;; --------------------------------
;;  variables
;;


;; --------------------------------
;;  auxiliary function
;;

(defun unless-null-format-1 (format-string str)
  "Format with FORMAT-STRING when STR is not null."
  (if (string= str "") "" (format format-string str)))


;; --------------------------------
;;  Transcoders
;;

(defun org-novel-bold (bold contents info)
  "Transcode BOLD from Org to LaTeX.
CONTENTS is the text with bold markup.
INFO is a plist holding contextual information."
  (format "\\textbf{\\textgt{%s}}" contents))

(defun org-novel--colophon (info)
  "Generate LaTeX script, which designate colophon.
INFO is a plist holdin contextual information."
  (concat
   (format "
\\newpage
\\onecolumn
\%\%\%\% colophon
\\setlength{\\oddsidemargin}{-0.9cm}
\\setlength{\\evensidemargin}{-0.9cm}
\\setlength{\\topmargin}{1.5in}
\\setlength{\\textwidth}{50zw}

\\begin{landscape}
\\chapter*{}
\\thispagestyle{empty}
\\rotatepbox{{\\Large %s}}

%s

\\vspace{1zw}
\\rotatepbox{%s %s}
"
	   (org-export-data (plist-get info :title) info)
	   (unless-null-format-1 "\\vspace{1ex}
\\rotatepbox{{\\normalsize %s}}"
				 (plist-get info :subtitle))
	   (plist-get info :published)
	   (plist-get info :edition))
   "
\\begin{table}[htb]
\\begin{tabular}{rl}
"
   (let ((author (car (plist-get info :author))))
     (if author (format "\\raiserotatepbox{著者} & \\raiserotatepbox{%s}" author)
       ""))
   (unless-null-format-1 "\\\\\n\\raiserotatepbox{発行} & \\raiserotatepbox{%s}"
			 (plist-get info :publisher))
   (unless-null-format-1 "\\\\\n\\raiserotatepbox{連絡先} & \\raiserotatepbox{%s}"
			 (plist-get info :address))
   (unless-null-format-1 "\\\\\n\\raiserotatepbox{印刷} & \\raiserotatepbox{%s}"
			 (plist-get info :printer))
      "
\\end{tabular}
\\end{table}
\\end{landscape}
"
      ))

(defun org-novel--font-size (info)
  "Generate LaTeX script, which specify font-size and line-height from INFO."
  (let ((size (plist-get info :with-size)))
    (cond ((member size '(normalsize normal))
	   "\\setstretch{1.2}\n\\normalsize")
	  ((eq size 'large) "\\setstretch{1.2}\n\\large")
	  (t "\\setstretch{1.4}\n\\small"))))

(defun org-novel-headline (headline contents info)
  "Transcode a HEADLINE element from Org to LaTeX.
This script processes until the heading of level 2.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (let ((level (org-export-get-relative-level headline info))
	(text (org-export-data (org-element-property :title headline) info))
	(sec-prefix (plist-get info :with-sec-prefix)))
    (concat
     (cond ((= level 1)
	    (if (string= text "")
		"\n\\chapter*{}\n\\vspace{-12.5em}"
	      (format "
\\chapter*{%s}
\\addcontentsline{toc}{chapter}{%s}
"
		      text text)))
	   ((= level 2)
	    (format "
\\hspace{1.0em}\\textgt{%s%s}
\\addcontentsline{toc}{section}{%s}

~

"
		      sec-prefix text text))
	   (t ""))
     contents)))

(defun org-novel-link (link desc info)
  "Transcode a LINK object from Org to LaTeX.
Fuzzy type link is treated as a ruby.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
	 (raw-path (org-element-property :path link))
	 (path (if (not (file-name-absolute-p raw-path)) raw-path
		 (expand-file-name raw-path)))
	 (image-p (org-export-inline-image-p link)))
    (cond
     (image-p (format "\\rotatebox{90}{\\includegraphics{%s}}" path))
     ((string= type "fuzzy")
      (format "\\ruby[g]{%s}{%s}"
	      (if desc desc raw-path) raw-path))
     
     (t "nothing"))))

(defun org-novel-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to LaTeX.
CONTENTS is the contents of the paragraph, as a string.  INFO is
the plist used as a communication channel."
  (let* ((parent (org-export-get-parent-element paragraph))
	 (ptype (org-element-type parent)))
    (if (memq ptype '(special-block item)) contents
      (format "%s~" (replace-regexp-in-string "\n" "\n\n" contents)))))

(defun org-novel-plain-text (text info)
  "Transcode a TEXT string from Org to LaTeX.
Sentence that the first character is a square bracket will not be indented.

TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (replace-regexp-in-string "『"
			    "\\\\noindent\\\\inhibitglue『"
			    (replace-regexp-in-string "「"
						      "\\\\noindent\\\\inhibitglue「"
						      text)))

(defun org-novel-section (section contents info)
  "Transcode a SECTION element from Org to LaTeX.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  contents)

(defun org-novel-template (contents info)
  "Return complete document string after LaTeX conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   "\\documentclass[a5j,10pt,uplatex,openright,dvipdfmx]{utbook}
\\usepackage[uplatex,deluxe]{otf}
\\usepackage{pxrubrica}
\\usepackage{setspace}
\\usepackage{lscape}
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{plext}

\\newcommand{\\rotatepbox}[1]{\\rotatebox{-90}{\\pbox<y>{#1}}}
\\newcommand{\\raiserotatepbox}[1]{\\raise1ex\\hbox{\\rotatepbox{#1}}}

\\begin{document}
"
   (org-novel--title info)
   (org-novel--toc info)
   "
\%\% main
\\setlength{\\oddsidemargin}{-1.2cm}
\\setlength{\\evensidemargin}{-0.6cm}
\\setlength{\\topmargin}{-0.5in}
\\setlength{\\textwidth}{50zw}
"
   (org-novel--font-size info)
   "
\\twocolumn
"
   contents

   (org-novel--colophon info)
   
   "
\\end{document}
"
   ))

(defun org-novel--title (info)
  "LaTeX script which genarates title-page from INFO."
    (concat
     "
\%\%\%\% titlepage
\\setlength{\\oddsidemargin}{-0.9cm}
\\setlength{\\evensidemargin}{-0.9cm}
\\setlength{\\topmargin}{1.8in}
\\setlength{\\textwidth}{50zw}

\\thispagestyle{empty}
\\begin{landscape}
"
     (format "\\begin{center}
\\rotatepbox{\\huge %s}
\\end{center}
"
	     (org-export-data (plist-get info :title) info))
     (unless-null-format-1 "\\begin{center}
\\rotatepbox{\\large %s}
\\end{center}
"
			   (plist-get info :subtitle))
"\\hspace{1ex}"
     (format "\\begin{center}
\\rotatepbox{\\large %s}
\\end{center}
"
	     (car (plist-get info :author)))
     "
\\end{landscape}
\\newpage
"
     ))

(defun org-novel--toc (info)
  "LaTeX command to set the table of contents.

INFO is a plist holding contextual information."
  (let ((depth (plist-get info :with-toc)))
    (when depth
      (concat
       "
\%\% toc
"
       (when (wholenump depth)
	 (format "\\setcounter{tocdepth}{%d}\n" depth))
       "\\tableofcontents\n"
       "\\thispagestyle{empty}"))))

(defun org-novel-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to LaTeX.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "{\\tt %s}" (org-element-property :value verbatim)))


;; --------------------------------
;;  Exporter
;;

(defun org-novel-publish-to-latex (plist filename pub-dir)
  "Publish an Org file to LaTeX.

PLIST is the property list for the given project. 
FILENAME is the filename of the Org file to be published. 
PUB-DIR is the publishing directory.

Return output file name."
  (org-publish-org-to 'novel filename
		      (concat ".tex")
		      plist pub-dir))

(defun org-novel-to-buffer (&optional async subtreep visible-only body-only ext-plist)
    "Export current buffer as a LaTeX buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Otional argument BODY-ONLY has no function. 

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org LATEX Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'novel "*Org Novel Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (latex-mode))))

(defun org-novel-to-latex (&optional async subtreep visible-only body-only ext-plist)
  "Assume the current region has org-mode syntax, and convert it to LaTeX."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'novel file
      async subtreep visible-only body-only ext-plist)))

(defun org-novel-export-to-pdf
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to LaTeX then process through to PDF.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Optional argument BODY-ONLY has no function.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'novel outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-novel-compile file)))))

(defun org-novel-compile (texfile)
  "Compile a TeX file.

TEXFILE is the name of the file being compiled.
Processing is done by uplatex and dvipdfmx."
  (let* ((base-name (file-name-sans-extension (file-name-nondirectory texfile)))
	 (full-name (file-truename texfile))
	 (out-dir (file-name-directory texfile))
	 (out-dvi-name (concat out-dir base-name ".dvi"))
	 (out-pdf-name (concat out-dir base-name ".pdf"))
	 (default-directory (if (file-name-absolute-p texfile)
				(file-name-directory full-name)
			      default-directory)))
    (save-window-excursion
      (let ((command (format "uplatex %s; uplatex %s; dvipdfmx -p %s %s"
			     texfile texfile "a5" out-dvi-name)))
	(shell-command command))
      (let ((pdffile (concat out-dir base-name ".pdf")))
	(if (not (file-exists-p pdffile))
	    (error (format "PDF file %s wasn't produced." pdffile))
	  pdffile))
      )))


;; --------------------------------
;;  Provide
;; 

(provide 'ox-novel)
;;; ox-novel.el ends here
