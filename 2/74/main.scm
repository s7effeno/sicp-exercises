; a)
; each division should implement a get-record procedure that given an employee
; extracts the according record, then put this procedure to the table, tagging
; the file owned with an identifier that specifies the division
(define (get-record employee file)
  ((get 'get-record (type-tag file)) id))

; b)
; each division should implement a get-salary procedure as above that extracts
; the salary entry from any record
(define (get-salary employee file)
  ((get 'get-salary (type-tag file)) (get-record file id)))


; c)
; assuming get-record is false if the employee is not found in the file
(define (find-employee-record employee files)
  (cond ((null? files) #f)
        ((not (get-record employee (car files)))
         (find-employee-record employee (cdr files)))
        (else (get-record employee (car files)))))

; d)
; assuming new divisions will be aquired, each division should implement the
; above procedures and put them to the table
