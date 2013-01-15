;;;

(println "Welcome to another Scheme REPL")
(println "Author: Karl-Aksel Puulmann, Jan 2013")
(println "")

(define currentExpression "")
(define repl (lambda ()
    (begin
        (if (zero? (length currentExpression)) 
            (print " > ")
            (begin
                (print ".. ")
                (set! currentExpression
                    (addStr currentExpression " "))))
        (define newLine (readline))
        (set! currentExpression
            (addStr currentExpression newLine))
        (cond 
            ((zero? (length newLine)) (set! currentExpression ""))
            ((parse currentExpression)
                (begin
                    (define result (catch (eval currentExpression)))
                    (print "=> ")
                    (println result)
                    (set! currentExpression ""))))
        (repl))))
(repl)
