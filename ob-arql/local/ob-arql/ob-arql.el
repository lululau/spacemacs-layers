;;; ob-arql.el --- Babel Functions for Arql          -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating arql source code.

;;; Code:
(require 'inf-ruby)
(require 'ob)

(declare-function org-trim "org" (s &optional keep-lead))
(declare-function run-ruby "ext:inf-ruby" (&optional command name))
(declare-function xmp "ext:rcodetools" (&optional option))

(defvar inf-ruby-default-implementation)
(defvar inf-ruby-implementations)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("ruby" . "rb"))

(defvar org-babel-default-header-args:arql '())

(defvar org-babel-arql-command "arql"
  "Name of command to use for executing ruby code.")

(defcustom org-babel-arql-hline-to "nil"
  "Replace hlines in incoming tables with this when translating to ruby."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'string)

(defcustom org-babel-arql-nil-to 'hline
  "Replace nil in ruby tables with this before returning."
  :group 'org-babel
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'symbol)

(defun org-babel-execute:arql (body params)
  "Execute a block of Ruby code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (org-babel-arql-initiate-session
		   (cdr (assq :session params)) params))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:arql params)))
         (result (if (member "xmp" result-params)
		     (with-temp-buffer
		       (require 'rcodetools)
		       (insert full-body)
		       (xmp (cdr (assq :xmp-option params)))
		       (buffer-string))
		   (org-babel-arql-evaluate
		    session full-body result-type result-params params))))
    (org-babel-reassemble-table
     (org-babel-result-cond result-params
       result
       (org-babel-arql-table-or-string result))
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rownames params))))))

(defun org-babel-prep-session:arql (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  ;; (message "params=%S" params) ;; debugging
  (let* ((session (org-babel-arql-initiate-session session params))
         (var-lines (org-babel-variable-assignments:arql params)))
    (org-babel-comint-in-buffer session
      (sit-for .5) (goto-char (point-max))
      (mapc (lambda (var)
              (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session)
              (sit-for .1) (goto-char (point-max))) var-lines))
    session))

(defun org-babel-load-session:arql (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:arql session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions

(defun org-babel-variable-assignments:arql (params)
  "Return list of ruby statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "%s=%s"
	     (car pair)
	     (org-babel-arql-var-to-arql (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-arql-var-to-arql (var)
  "Convert VAR into a ruby variable.
Convert an elisp value into a string of ruby source code
specifying a variable of the same value."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-arql-var-to-arql var ", ") "]")
    (if (eq var 'hline)
	org-babel-arql-hline-to
      (format "%S" var))))

(defun org-babel-arql-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If RESULTS look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (let ((res (org-babel-script-escape results)))
    (if (listp res)
        (mapcar (lambda (el) (if (not el)
				 org-babel-arql-nil-to el))
                res)
      res)))

(defun org-babel-arql-initiate-session (&optional session params)
  "Initiate a ruby session.
If there is not a current inferior-process-buffer in SESSION
then create one.  Return the initialized session."
  (unless (string= session "none")
    (require 'inf-ruby)
    (let* ((options (cdr (assoc :options params)))
           (cmd (format "%s -b %s"(cdr (assoc "arql" inf-ruby-implementations)) options))
           (buffer (get-buffer (format "*%s*" session)))
	   (session-buffer (or buffer (save-window-excursion
					(run-ruby cmd session)
					(current-buffer)))))
      (if (org-babel-comint-buffer-livep session-buffer)
	  (progn (sit-for .25) session-buffer)
	(sit-for .5)
	(org-babel-arql-initiate-session session params)))))

(defvar org-babel-arql-eoe-indicator ":org_babel_arql_eoe"
  "String to indicate that evaluation has completed.")
(defvar org-babel-arql-f-write
  "File.open('%s','w'){|f| f.write((_.class == String) ? _ : _.inspect)}")
(defvar org-babel-arql-pp-f-write
  "File.open('%s','w'){|f| $stdout = f; pp(results); $stdout = orig_out}")
(defvar org-babel-arql-wrapper-method
  "
def main()
%s
end
results = main()
File.open('%s', 'w'){ |f| f.write((results.class == String) ? results : results.inspect) }
")
(defvar org-babel-arql-pp-wrapper-method
  "
require 'pp'
def main()
%s
end
results = main()
File.open('%s', 'w') do |f|
  $stdout = f
  pp results
end
")

(defun org-babel-arql-build-command (params)
  (let* ((options (cdr (assoc :options params))))
    (format "%s -b %s" org-babel-arql-command options)))

(defun org-babel-arql-evaluate
    (buffer body &optional result-type result-params params)
  "Pass BODY to the Ruby process in BUFFER.
If RESULT-TYPE equals `output' then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals `value' then
return the value of the last statement in BODY, as elisp."
  (if (not buffer)
      ;; external process evaluation
      (pcase result-type
	(`output (org-babel-eval (org-babel-arql-build-command params) body))
	(`value (let ((tmp-file (org-babel-temp-file "arql-")))
		  (org-babel-eval
		   (org-babel-arql-build-command params)
		   (format (if (member "pp" result-params)
			       org-babel-arql-pp-wrapper-method
			     org-babel-arql-wrapper-method)
			   body (org-babel-process-file-name tmp-file 'noquote)))
		  (org-babel-eval-read-file tmp-file))))
    ;; comint session evaluation
    (pcase result-type
      (`output
       (let ((eoe-string (format "puts \"%s\"" org-babel-arql-eoe-indicator)))
	 ;; Force the session to be ready before the actual session
	 ;; code is run.  There is some problem in comint that will
	 ;; sometimes show the prompt after the the input has already
	 ;; been inserted and that throws off the extraction of the
	 ;; result for Babel.
	 (org-babel-comint-with-output
	     (buffer org-babel-arql-eoe-indicator t eoe-string)
	   (insert eoe-string) (comint-send-input nil t))
	 ;; Now we can start the evaluation.
	 (mapconcat
	  #'identity
	  (butlast
	   (split-string
	    (mapconcat
	     #'org-trim
	     (org-babel-comint-with-output
		 (buffer org-babel-arql-eoe-indicator t body)
	       (mapc
		(lambda (line)
		  (insert (org-babel-chomp line)) (comint-send-input nil t))
		(list "conf.echo=false;_org_prompt_mode=conf.prompt_mode;conf.prompt_mode=:NULL"
		      body
		      "conf.prompt_mode=_org_prompt_mode;conf.echo=true"
		      eoe-string)))
	     "\n") "[\r\n]") 4) "\n")))
      (`value
       (let* ((tmp-file (org-babel-temp-file "ruby-"))
	      (ppp (or (member "code" result-params)
		       (member "pp" result-params))))
	 (org-babel-comint-with-output
	     (buffer org-babel-arql-eoe-indicator t body)
	   (when ppp (insert "require 'pp';") (comint-send-input nil t))
	   (mapc
	    (lambda (line)
	      (insert (org-babel-chomp line)) (comint-send-input nil t))
	    (append
	     (list body)
	     (if (not ppp)
		 (list (format org-babel-arql-f-write
			       (org-babel-process-file-name tmp-file 'noquote)))
	       (list
		"results=_" "require 'pp'" "orig_out = $stdout"
		(format org-babel-arql-pp-f-write
			(org-babel-process-file-name tmp-file 'noquote))))
	     (list org-babel-arql-eoe-indicator)))
	   (comint-send-input nil t))
	 (org-babel-eval-read-file tmp-file))))))

(provide 'ob-arql)

;;; ob-arql.el ends here
