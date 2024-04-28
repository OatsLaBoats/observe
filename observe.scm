(import chicken.process)
(import chicken.process-context)
(import chicken.file)
(import chicken.file.posix)
(import chicken.string)

(define args (command-line-arguments))

(define list-contains
  (lambda (args value)
    (if (null? args) #f
      (if (string=? (car args) value) #t
        (list-contains (cdr args) value)))))

(define list-length
  (lambda (l)
    (let loop ((l l) (acc 0))
      (if (null? l) acc
        (loop (cdr l) (+ acc 1))))))

(define list-merge
  (lambda (l1 l2)
    (cond
      ((null? l1) l2)
      ((null? l2) l1)
      (else 
        (cons (car l1) (cons (car l2) (list-merge (cdr l1) (cdr l2))))))))

(define get-command
  (lambda (args)
    (if (null? args) #f
      (if (string=? (car args) "-c")
        (if (null? (cdr args)) #f (cadr args))
        (get-command (cdr args))))))

(define get-files
  (lambda (args)
    (let loop ((xs args) (acc (list)))
      (if (null? xs) acc
        (cond
          ((string=? (car xs) "-c")
            (if (null? (cdr xs)) acc (loop (cddr xs) acc)))
          ((string=? (car xs) "-h") (loop (cdr xs) acc))
          ((string=? (car xs) "-help") (loop (cdr xs) acc))
          ((string=? (car xs) "--help") (loop (cdr xs) acc))
          ((string=? (car xs) "-clear") (loop (cdr xs) acc))
          (else (loop (cdr xs) (cons (car xs) acc))))))))

(define option-help 
  (or
    (list-contains args "-h") 
    (list-contains args "-help") 
    (list-contains args "--help")))

(define option-clear (list-contains args "-clear"))

(define command (get-command args))
(define arg-files (map (lambda (s) (string-translate s "^" "*")) (get-files args)))

(define display-help
  (lambda () 
    (display "usage: observe [OPTION ...] [PATTERN ...]\n\n")
    (display "    Observes a file or files for changes and runs a command\n")
    (display "    when a change is detected.\n\n")
    (display "    example: observe -clear -c \"cat example.txt\" \"example.txt\"\n\n")
    (display "options:    -h -help --help   Displays help message.\n")
    (display "            -clear            Clears the terminal before running the command.\n")
    (display "            -c COMMAND        Specify the command to run on file change.\n\n")
    (display "PATTERN examples:\n")
    (display "    \"example.file\"\n")
    (display "    \"./example.file\"\n")
    (display "    \"*.file\"\n")
    (display "    \"./\"\n")
    (display "    \"^.file\" Equivalent to '*' but the shell won't expand it.\n")
    (display "    \"?.file\" Zero or one character.\n")))

(define display-invalid-args
  (lambda ()
    (display "Invalid arguments, use \"observe -h\" for help.\n")))

(define display-no-files
  (lambda ()
    (display "Please provide a file pattern, use \"observe -h\" for help.\n")))

(define file-database (list))
(define database-changed #f)

(define has-file
  (lambda (filename)
    (let loop ((d file-database))
      (if (null? d) #f
        (if (string=? (caar d) filename) #t 
          (loop (cdr d)))))))

(define get-file-time
  (lambda (filename)
    (let loop ((d file-database))
      (if (null? d) #f
        (if (string=? (caar d) filename) (cdar d)
          (loop (cdr d)))))))

(define remove-file
  (lambda (filename)
    (let loop ((d file-database) (acc (list)))
      (if (null? d) (set! file-database acc)
        (if (string=? (caar d) filename)
          (loop (cdr d) acc)
          (loop (cdr d) (cons (car d) acc)))))))

(define add-file
  (lambda (filename modtime)
    (set! file-database (cons (cons filename modtime) file-database))))

(define clear-terminal
  (lambda ()
    (display "\033[2J\033[1;1H")
    (display "Number of tracked files: ")
    (display (list-length file-database))
    (newline)
    (newline)))

(define glob-files
  (lambda (files)
    (apply glob files)))

(define remove-deleted-files
  (lambda ()
    (let loop ((d file-database))
      (unless (null? d)
        (if (file-exists? (caar d))
          (loop (cdr d))
          (begin
            (set! database-changed #t)
            (remove-file (caar d)) 
            (loop (cdr d))))))))

(define add-new-files
  (lambda (files)
    (unless (null? files)
      (if (has-file (car files))
        (add-new-files (cdr files))
        (begin
          (set! database-changed #t)
          (add-file (car files) (file-modification-time (car files)))
          (add-new-files (cdr files)))))))

(define update-file-times
  (lambda ()
    (let loop ((d file-database))
      (unless (null? d)
        (let 
          ((new-time (file-modification-time (caar d)))
           (cur-time (cdar d)))
          (if (> new-time cur-time)
            (begin
              (set! database-changed #t)
              (remove-file (caar d))
              (add-file (caar d) new-time)
              (loop (cdr d)))
            (loop (cdr d))))))))

(define run
  (lambda ()
    (remove-deleted-files)
    (add-new-files (glob-files arg-files))
    (update-file-times)
    (when database-changed
      (when option-clear (clear-terminal))
      (set! database-changed #f)
      (process-run command))
    (process-sleep 1)
    (run)))

(begin
  (cond
    ((null? args) (display-invalid-args))
    (option-help (display-help))
    ((not command) (display-invalid-args))
    ((not arg-files) (display-invalid-args))
    ((null? arg-files) (display-no-files))
    (else (run))))
