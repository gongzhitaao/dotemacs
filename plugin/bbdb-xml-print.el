;;; bbdb-xml-print.el -- Convert bbdb records to XML.

;; Original Author	: Toby Speight
;; Last Modified	: Tue May 24 19:15:26 IST 2005

;;; L I C E N C E

;; Distributed under The GNU GPL version 2.

;;; Commentary:

;; This code creates an XML-ified file of BBDB records.  The generated
;; XML conforms to the bbdb.dtd file distributed along with this
;; program.

;; To use this program, place it in some directory that is accessible
;; from your load-path.  Then place the following in your ~/.emacs
;;
;; (require 'bbdb-xml-print)
;;
;; Now, inside the *BBDB* buffer, you can press "X" to convert the
;; displayed records into XML!!

;;; History

;; bbdb-xml-print was first written by Toby Speight
;; <Toby.Speight@streapadair.freeserve.co.uk> and was under wraps for an
;; unspecified amout of time.  It was fist posted on the bbdb-info
;; mailing list in March 2001.  Shengou Zhu <zsh@cs.rochester.edu>
;; posted a patch that made it work with bbdb version 2.2.  Sriram Karra
;; <karra@cs.utah.edu> fixed some assorted bugs and put together this
;; package.  Norman Walsh <ndw@nwalsh.com> published the dtd file he
;; uses with his own home-brewn xml-converter written in perl.  This
;; file was lifted with thanks and included here.

;; When first written, this was heavily inspired by bbdb-print written
;; by Boris Goldowsky <boris@psych.rochester.edu> and Dirk Grunwald
;; <grunwald@cs.colorado.edu>.  Many of the function names etc. still
;; bear obvious similarity to that package stuff.

;;; TODO

;; What else? Some XSLT converters ? :)

;;; Code

(require 'bbdb)
(require 'bbdb-com)

(define-key bbdb-mode-map "X" 'bbdb-xml-print)

;; Variables

(defvar bbdb-xml-print-file-name "~/bbdb.xml"
  "*Default file name for printouts of BBDB database.")

(defvar bbdb-xml-print-elide '(tex-name mail-alias nic nic-updated)
  "*List of fields NOT to print when printing an address list.
See also bbdb-xml-print-no-bare-names.")

(defvar bbdb-xml-print-no-bare-names t
  "*If nonnil, `bare names' will not be printed.
A name is bare if the record contains no non-elided fields other than
name and company \(see bbdb-xml-print-elide).")

(defvar bbdb-xml-print-prolog
  (eval-when-compile
    (concat "<?xml version=\"1.0\" ?>\n"
;	    "<!DOCTYPE BBDB SYSTEM \"bbdb.dtd\">\n"
            "<BBDB>\n\n"))
  "*XML prolog to include at the beginning of the bbdb-xml-print file.")

(defvar bbdb-xml-print-epilog "\n</section>\n\n</BBDB>\n"
  "*XML to include at the end of the bbdb-xml-print file.")

(defvar bbdb-xml-print-n-nets 'all
  "How many email addresses should be printed out.  'all means all, If
it is a digit then that is the max. number printed out.")

(defvar bbdb-xml-print-spam-ctrl-enabled t
  "If t, the email addresses will be transformed.
a@b.c will be printed as a/@/b.c")

(defun bbdb-xml-print-field-shown-p (field)
  (require 'bbdb-print)
  (not (memq field bbdb-print-omit-fields)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun bbdb-xml-print (to-file)
  "Print the selected BBDB entries"
  (interactive (list (read-file-name "Print To File: " bbdb-xml-print-file-name)))
  (setq bbdb-xml-print-file-name (expand-file-name to-file))
  (let ((current-letter t)
	(records (progn (set-buffer bbdb-buffer-name)
			bbdb-records)))
    (find-file bbdb-xml-print-file-name)
    (delete-region (point-min) (point-max))
    (while records
      (setq current-letter
	    (bbdb-xml-print-format-record (car (car records)) current-letter))
      (setq records (cdr records)))
    (goto-char (point-min)) (insert bbdb-xml-print-prolog)
    (goto-char (point-max)) (insert bbdb-xml-print-epilog)
    (goto-char (point-min))))

(defun bbdb-xml-print-format-record (record &optional current-letter brief)
  "Insert the bbdb RECORD in TeX format.
Optional CURRENT-LETTER is the section we're in -- if this is non-nil and
the first letter of the sortkey of the record differs from it, a new section
heading will be output \(an arg of t will always produce a heading).
The new current-letter is the return value of this function.
Someday, optional third arg BRIEF will produce one-line format."
  (bbdb-debug (if (bbdb-record-deleted-p record)
		  (error "plus ungood: xml formatting deleted record")))

  (let* ((bbdb-elided-display bbdb-xml-print-elide)
	 (first-letter
	  (substring (concat (bbdb-record-sortkey record) "?") 0 1))
	 (name   (and (bbdb-xml-print-field-shown-p 'name)
		      (or (bbdb-record-getprop record 'tex-name)
			  (bbdb-xml-print-tex-quote
			   (bbdb-record-name record)))))
	 (comp   (and (bbdb-xml-print-field-shown-p 'company)
		      (bbdb-record-company record)))
	 (net    (and (bbdb-xml-print-field-shown-p 'net)
		      (bbdb-record-net record)))
	 (phones (and (bbdb-xml-print-field-shown-p 'phone)
		      (bbdb-record-phones record)))
	 (addrs  (and (bbdb-xml-print-field-shown-p 'address)
		      (bbdb-record-addresses record)))
	 (notes  (bbdb-record-raw-notes record))
	 (begin (point))
	 (bare t))

    (message "printing for record.... %s " name)

    ;; Section header, if neccessary.

    (cond ((and current-letter
		(not (string-equal first-letter current-letter)))
	   (if (stringp current-letter)
	       (insert "</section>\n"))
	   (insert (format "\n<section initial=\"%s\">\n"
			   (bbdb-xml-print-attr-quote (upcase first-letter)))))
	  (t (insert "\n")))

    ;; Name and Company

    (insert (format "<record>\n<name>%s</name>\n"
		    (or name comp "")))

    (if (and name comp)
	(insert (format "<company>%s</company>\n"
			(bbdb-xml-print-tex-quote comp))))

    ;; Phone numbers

    (cond (phones
	   (setq bare nil)
	   (insert "<phones>\n")
	   (while phones
	     (let ((place (aref (car phones) 0))
		   (number (bbdb-phone-string (car phones))))
	       (insert (format "<phone location=\"%s\">%s</phone>\n"
			       (bbdb-xml-print-tex-quote
				(bbdb-xml-print-if-not-blank place))
			       (bbdb-xml-print-tex-quote number)))
	       (setq phones (cdr phones))))
	   (insert "</phones>\n")))

    ;; Email address -- just list their first address.

    (if net
	(let ((nett net)
	      (start 0))
	  (insert "<emails>\n")
	  (while (and nett
		      (or (eq bbdb-xml-print-n-nets 'all)
			  (< start bbdb-xml-print-n-nets)))
	    (setq net-addr (bbdb-xml-print-tex-quote (car nett)))
	    (setq bare nil)
	    (when (and bbdb-xml-print-spam-ctrl-enabled
		       (string-match "@" net-addr))
	      (setq net-addr (replace-match "/@/" nil nil net-addr)))
	    (insert (format "<email>%s</email>\n" net-addr))
	    (setq start (+ start 1))
	    (setq nett (cdr nett)))
	  (insert "</emails>\n")))

    ;; Addresses

    (setq kn 0)
    (while addrs
      (setq kn (+ kn 1))
      (message "Printing address... %d" kn)
      (let ((addr (car addrs)))
	(setq bare nil)
	(insert
	 (format
	  "<address location=\"%s\">%s\n</address>"
	  (bbdb-address-location addr)
	  (concat
	   (mapconcat
	    (lambda (x)
	      (bbdb-xml-print-if-not-blank-2
	       "\n <line>" (bbdb-xml-print-tex-quote x) "</line>"))
	    (bbdb-address-streets addr)
	    "")
	   (bbdb-xml-print-if-not-blank-2
	    "\n <city>" (bbdb-xml-print-tex-quote (bbdb-address-city addr)) "</city>")
	   (bbdb-xml-print-if-not-blank-2
	    "\n <state>" (bbdb-xml-print-tex-quote (bbdb-address-state addr)) "</state>")
	   (bbdb-xml-print-if-not-blank-2
	    " <zip>" (bbdb-xml-print-tex-quote (bbdb-address-zip-string
						addr)) "</zip>")
	   (bbdb-xml-print-if-not-blank-2
	    " <country>" (bbdb-xml-print-tex-quote
			  (bbdb-address-country addr)) "</country>")))))

      (setq addrs (cdr addrs)))

    ;; Notes
    ;;
    ;; BBDB calls all non-standard fields as "notes".  We sometimes have
    ;; a real need to treat the real "notes" fields differently from
    ;; others.  For e.g. I keep track of what each person is upto using
    ;; the multiple notes fields (notes, notes1 etc.)  We would like to
    ;; put them at a separate hierarchy for easier processing later.
    ;;
    ;; We will generate the xml output for fields beginning with
    ;; "^notes" and keep the xml in a separate string which we shall put
    ;; in later.

    (if (stringp notes)
	(setq notes (list (cons 'notes notes))))
    (let ((sp-notes))
      (while notes
	(let ((thisnote (car notes)))
	  (if (bbdb-xml-print-field-shown-p (car thisnote))
	      (progn
		(setq bare nil)
		(if (string-match "^pnotes" (symbol-name (car thisnote)))
		    (setq sp-notes
			  (concat sp-notes
				  (format "<not>%s\n</not>\n"
					  (bbdb-xml-print-tex-quote
					   (cdr thisnote)))))
		  (insert (format "<note type=\"%s\">%s</note>\n"
				  (bbdb-xml-print-attr-quote
				   (symbol-name (car thisnote)))
				  (bbdb-xml-print-tex-quote
				   (cdr thisnote)))))))
	  (setq notes (cdr notes))))
      (when sp-notes
	(insert (format "<nots>\n%s</nots>\n" sp-notes))))

    ;; If record is bare, delete anything we may have inserted.
    ;; otherwise, mark the end of this record.

    (if bare
	(delete-region begin (point))
      (insert "</record>\n")

      (setq current-letter first-letter))
    (message "Done for record.... %s " name)
    current-letter))


(defun bbdb-xml-print-attr-quote (str)
  "Quote any XML-significant characters that appear in STRING."
  (when str
    (let (n)
      (setq n (string-match "&" str))
      (while n
	(setq str (replace-match "&amp;" t t str))
	(setq n (string-match "&" str (+ n 4))))

      (setq n (string-match "\"" str))
      (while n
	(setq str (replace-match "&quot;" t t str))
	(setq n (string-match "\"" str (+ n 5))))))
  str)

(defun bbdb-xml-print-tex-quote (str)
  "Quote any XML-significant characters that appear in STRING."
  (let (n)
    (when str
      (setq n (string-match "&" str))
      (while n
	(setq str (replace-match "&amp;" t t str))
	(setq n (string-match "&" str (+ n 4))))

      (setq n (string-match ">" str))
      (while n
	(setq str (replace-match "&gr;" t t str))
	(setq n (string-match ">" str (+ n 4))))

      (setq n (string-match ">" str))
      (while n
	(setq str (replace-match "&gr;" t t str))
	(setq n (string-match ">" str (+ n 4))))))
  str)

(defun bbdb-xml-print-if-not-blank (string &rest more)
  "If STRING is not null, then return it concatenated
with rest of arguments.  If it is null, then all arguments are
ignored and the null string is returned."
  (if (or (null string) (equal "" string))
      ""
    (apply 'concat string more)))

(defun bbdb-xml-print-if-not-blank-2 (string0 string &rest more)
  "If the second argument is empty, the empty string; otherwise the cat of all the args."
  (if (or (null string) (equal "" string))
      ""
    (apply 'concat string0 string more)))

(provide 'bbdb-xml-print)
