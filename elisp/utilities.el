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
