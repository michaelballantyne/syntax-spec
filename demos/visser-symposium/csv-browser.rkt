#lang racket/gui

(require "state-machine.rkt"
         "gui-layout.rkt"
         net/url
         csv-reading)

;;
;; UI elements
;;

(define frame
  (new frame%
       [label "CSV Browser"]
       [min-width 400]
       [min-height 200]))

(gui-layout frame 
  (vertical-pane%
   (horizontal-pane%
    [stretchable-height #f]

    (text-field%
     #:as url-field
     [label "Data URL"]
     [init-value "https://people.sc.fsu.edu/~jburkardt/data/csv/addresses.csv"]
     [callback (lambda _ (send csv-controller url-change))])

    (button%
     [label "Load"]
     [callback (lambda _ (send csv-controller load-click))]))

   (pane%
    #:as data-area

    (message%
     #:as url-message
     [label "Enter a URL"])
          
    (message%
     #:as loading-message
     [label "Loading..."])

    (message%
     #:as error-message
     [label "Error loading data"])
     
    (list-box%
     #:as table
     [label ""]
     [columns (list "")]
     [choices (list)]
     [style (list 'single 'variable-columns)]))))

;;
;; UI actions
;;

(define (set-display to-show)
  (send data-area change-children (lambda (_) (list to-show))))

(define (set-data data)
  (for ([i (range (- (length (send table get-column-labels)) 1))])
    (send table delete-column 1))
  (for ([i (range (- (length (car data)) 1))])
    (send table append-column ""))
  (send table set-column-width 0 100 0 500)
  (define transposed (apply map list data))
  (send/apply table set transposed))


;;
;; Data loading
;;

(define (load-data url)
  (thread
   (lambda ()
     (define (on-error e)
       (queue-callback
        (lambda ()
          (send csv-controller load-error))))
     
     (with-handlers ([exn:fail? on-error])                                
       (define data (csv->list (get-pure-port (string->url url))))
       (queue-callback
        (lambda ()
          (send csv-controller loaded data)))))))

;;
;; Controller via state machine DSL
;;

(define csv-controller
  (machine
   #:initial-state no-data
   (state no-data
     (on-enter (set-display url-message)))
   (state loading
     (on-enter (set-display loading-message)
               (load-data (send url-field get-value)))
     (on (loaded data)
       (set-data data)
       (-> display))
     (on (load-error) (-> error)))
   (state display
     (on-enter (set-display table)))
   (state error
     (on-enter (set-display error-message)))
   (on (load-click) (-> loading))
   (on (url-change) (-> no-data))))

;;
;; Run it.
;;

(send frame show #t)



