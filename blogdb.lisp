;--- data model foo
(defun make-entry(title body source archive date)
  (list :title title :body body :date date :archive archive))

(defvar *db* nil)

(defun add-entry(entry)(push entry *db*))

(defun dump-db()
  (dolist(entry *db*)
    (format t "~{~a:~10t~a~%~}~%" entry)))

(defun dump-db-alt()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

;--- ui foo
(defun prompt-read(prompt)
  (format *query-io* "~a: "prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-entry()
  (make-entry
    (prompt-read "Title")
    (prompt-read "Message")
    (or(parse-integer(prompt-read "Date"):junk-allowed t)0)
    (y-or-n-p "Archive [y/n]")))

;let it live 
(defun dear-diary()
  (format t "Umbasa!~%")
  (loop(add-entry(prompt-for-entry))
    (if(y-or-n-p "Art thou done?[y/n]: ")(return))))

;--- db thingy things
(defun save-db(filename)
  (with-open-file (out filename
                    :direction :output
                    :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db(filename)
  (with-open-file(in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select-by-date(date)
  (remove-if-not
    #'(lambda (entry)(equal(getf entry :date)date))
    *db*))

(defun select(selector-fn)
  (remove-if-not selector-fn *db*))

(defun date-selector(date)
  #'(lambda(entry)(equal(getf entry :date) date)))

(defun update(selector-fn &key title body date (archive nil archive-p))
  (setf *db*
    (mapcar
      #'(lambda (row)
        (when(funcall selector-fn row)
          (if title (setf(getf row :title) title))
          (if body (setf(getf row :body) body))
          (if date (setf(getf row :date) date))
          (if archive-p (setf(getf row :archive) archive)))
        row) *db*)))

(defun delete-row(selector-fn)
  (setf *db* (remove-if selector-fn *db*)))





  
