(provide 'utilities)

(require 'json)

(defun utilities/get-environment-variable (env-name)
  (let ((value (getenv env-name)))
    (if (not value) (error (format "Missing environment variable: %s." env-name)))
    value))

(defun utilities/ensure-directory-exists (directory)
  (unless (file-directory-p directory)
    (make-directory directory :parents))
  directory)

(defun utilities/read-json-file (json-file)
  (let* ((json-object-type 'hash-table)
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (data (json-read-file json-file)))
    data))

(defun utilities/org-get-keywords ()
  (org-element-map (org-element-parse-buffer 'element) 'keyword
    (lambda (keyword) (cons (org-element-property :key keyword)
		       (org-element-property :value keyword)))))

(defun utilities/org-get-file-keyword (keyword)
  (cdr (assoc keyword (utilities/org-get-keywords))))

(defun utilities/org-parse-and-format-date (str format)
  (let ((time-string (org-time-string-to-time str)))
    (format-time-string format time-string)))

(defun utilities/get-relative-parent-directory (file)
  (let* ((directory (file-name-directory (directory-file-name file)))
	 (directory (substring directory 0 (1- (length directory)))))
    (file-name-nondirectory directory)))

(defun utilities/copy-file-to-publishing-directory (file-location publishing-directory)
  (let* ((file (file-name-nondirectory file-location))
	 (destination-file (expand-file-name file publishing-directory)))
    (if (file-accessible-directory-p file)
	(progn
	  (message (format "Copying directory %s/ to %s" file publishing-directory))
	  (copy-directory file-location destination-file t t))
      (progn
	(message (format "Copying file %s to %s" file publishing-directory))
	(copy-file file-location destination-file t t)))))

(defun utilities/copy-files-to-publishing-directory (files publishing-directory)
  (seq-map (lambda (file) (utilities/copy-file-to-publishing-directory file publishing-directory)) files))

(defun utilities/compile-latex-file (file-location publishing-directory)
  (let* ((file (file-name-nondirectory file-location))
	 (filename (file-name-base file))
	 (file-directory (utilities/get-relative-parent-directory file-location))
	 (output-directory (expand-file-name file-directory publishing-directory))
	 (output-directory (utilities/ensure-directory-exists output-directory))
	 (output-file (expand-file-name (format "%s.pdf" filename) output-directory))	 
	 (change-directory-command (concat "cd " file-directory))
	 (latex-program (executable-find "xelatex"))
	 (compile-latex-command (concat latex-program " -output-directory=" output-directory " " file))
	 (cleanup-command (concat "rm -rf " (expand-file-name (format "%s.log" filename) output-directory)))
	 (compile-command (concat
			   change-directory-command " && "
			   compile-latex-command " && "
			   cleanup-command)))
    (message (format "Compiling LaTeX file %s to %s" file-location output-directory))
    (shell-command compile-command)
    (if (not (file-exists-p output-file))
	(error (format "Error: An issue occurred during compilation, %s was not created!" output-file)))))

(defun utilities/compile-latex-files (files publishing-directory)
  (seq-map (lambda (file) (utilities/compile-latex-file file publishing-directory)) files))
