;; ox-novel.el --- translate .org into .latex using utbook for writing novel

;;; Commentary:
;; 

;;; Code:
(require 'ox)


;; Define Backend
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
    (title . org-novel-title)
    (verbatim . org-novel-verbatim)
    )
  :export-block "NOVEL"
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
  "address"
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-novel-edition ""
  "edition"
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-novel-subtitle ""
  "subtitle"
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-novel-printer ""
  "printer"
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-novel-published ""
  "published date"
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-novel-publisher ""
  "publisher"
  :group 'org-export-novel
  :type '(string :tag "String"))

(defcustom org-export-with-size 'normal
  "font size"
  :group 'org-export-novel
  :type '(choice
	  (const :tag "small size" small)
	  (const :tag "normal size" normal)
	  (const :tag "large size" large)))

(defcustom org-export-with-sec-prefix ""
  "font size"
  :group 'org-export-novel
  :type '(string :tag "String"))


;; --------------------------------
;;  variables
;;


;; --------------------------------
;;  Transcoders
;;

(defun org-novel-bold (bold contents info)
  (format "\\textbf{\\textgt{%s}}" contents))

(defun org-novel--colophon (info)
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
	   (let ((subt (plist-get info :subtitle)))
	     (if (string= subt "") ""
	       (format "
\\vspace{1ex}

\\rotatepbox{{\\normalsize %s}}

"
		       subt)))
	   (plist-get info :published)
	   (plist-get info :edition))
   "
\\begin{table}[htb]
\\begin{tabular}{rl}
"
   (let ((author (car (plist-get info :author))))
     (if author (format "\\raiserotatepbox{著者} & \\raiserotatepbox{%s}" author)
       ""))
   (let ((publisher (plist-get info :publisher)))
     (if (string= publisher "") ""
       (format "\\\\\\raiserotatepbox{発行} & \\raiserotatepbox{%s}"
	       publisher)))
   (let ((address (plist-get info :address)))
     (if (string= address "") ""
       (format "\\\\\\raiserotatepbox{連絡先} & \\raiserotatepbox{%s}"
	       address)))
   (let ((printer (plist-get info :printer)))
     (if (string= printer "") ""
       (format "\\\\\\raiserotatepbox{印刷} & \\raiserotatepbox{%s}"
	       printer)))
   "
\\end{tabular}
\\end{table}
\\end{landscape}
"
      ))

(defun org-novel--font-size (info)
  (let ((size (plist-get info :with-size)))
    (cond ((member size '(normalsize normal))
	   "\\setstretch{1.2}\n\\normalsize")
	  ((eq size 'large) "\\setstretch{1.2}\n\\large")
	  (t "\\setstretch{1.4}\n\\small"))))

(defun org-novel-headline (headline contents info)
  (let ((level (org-export-get-relative-level headline info))
	(text (org-export-data (org-element-property :title headline) info))
	(sec-prefix (plist-get info :with-sec-prefix)))
    (concat
     (cond ((= level 1)
	    (format "
\\chapter*{%s}
\\addcontentsline{toc}{chapter}{%s}
"
		    text text))
	   ((= level 2)
	    (if (string= text "") ""
	      (format "
\\hspace{1.0em}\\textgt{%s%s}
\\addcontentsline{toc}{section}{%s}

~

"
		      sec-prefix text text)))
	   (t ""))
     contents)
    ))

(defun org-novel-link (link desc info)
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
     
     (t "nothing"))
    ))

(defun org-novel-paragraph (paragraph contents info)
  (let* ((parent (org-export-get-parent-element paragraph))
	 (ptype (org-element-type parent)))
    (if (memq ptype '(special-block item)) contents
      (format "%s~" (replace-regexp-in-string "\n" "\n\n" contents)))))

(defun org-novel-plain-text (text info)
  (replace-regexp-in-string "「"
			    "\\\\noindent\\\\inhibitglue「"
			    text))

(defun org-novel-section (section contents info)
  contents)

(defun org-novel-template (contents info)
  (concat
   "\\documentclass[a5j,10pt,uplatex,openleft,dvipdfmx]{utbook}
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
   (org-novel-title info)
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

(defun org-novel-title (info)
  (format
   
   "
\%\%\%\% titlepage
\\setlength{\\oddsidemargin}{-0.9cm}
\\setlength{\\evensidemargin}{-0.9cm}
\\setlength{\\topmargin}{1.8in}
\\setlength{\\textwidth}{50zw}

\\thispagestyle{empty}
\\begin{landscape}

%s

\\end{landscape}
\\newpage
"
   (org-novel--title-form info)
   ))

(defun org-novel--title-form (info)
    (concat
     (format
      "\\begin{center}
\\rotatepbox{\\huge %s}
\\end{center}
"
      (org-export-data (plist-get info :title) info))
     (let ((subt (plist-get info :subtitle)))
       (if (string= subt "") ""
	 (format 
	  "\\begin{center}
\\rotatepbox{\\large %s}
\\end{center}
"
	  subt)))
     (format 
      "\\begin{center}
\\rotatepbox{\\normalsize %s}
\\end{center}
"
      (car (plist-get info :author))
      )))

(defun org-novel--toc (info)
  (let ((depth (plist-get info :with-toc)))
    (when depth
      (concat
       "
\%\% toc
"
       (when (wholenump depth)
	 (format "\\setcounter{tocdepth}{%d}\n" depth))
       "\\tableofcontents"))))

(defun org-novel-verbatim (verbatim contents info)
  (format "{\\tt %s}" (org-element-property :value verbatim)))


;; --------------------------------
;;  Exporter
;;

(defun org-novel-publish-to-latex (plist filename pub-dir)
  (org-publish-org-to 'novel filename
		      (concat ".tex")
		      plist pub-dir))

(defun org-novel-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'novel "*Org Novel Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (latex-mode))))

(defun org-novel-to-latex (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((extension ".tex")
	 (file (org-export-output-file-name extension subtreep))
	 (org-export-coding-system oxn:coding-system))
    (org-export-to-file 'novel file
      async subtreep visible-only body-only ext-plist)))

(defun org-novel-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'novel outfile
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-novel-compile file)))))

(defun org-novel-compile (texfile)
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
