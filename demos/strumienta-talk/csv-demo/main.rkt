#lang racket/gui

(require net/url
         csv-reading
         "gui-layout.rkt")


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
     [callback (lambda _ (csv-controller '(url-change)))])

    (button%
     [label "Load"]
     [callback (lambda _ (csv-controller '(load-click)))]))

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

(define (show to-show)
  (send data-area change-children (lambda (_) (list to-show))))

(define (set-data data)
  (for ([i (range (- (length (send table get-column-labels)) 1))])
    (send table delete-column 1))
  (for ([i (range (- (length (car data)) 1))])
    (send table append-column ""))
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
          (csv-controller '(load-error)))))
     
     (with-handlers ([exn:fail? on-error])                                
       (define data (csv->list (get-pure-port (string->url url))))
       (queue-callback
        (lambda ()
          (csv-controller (list 'loaded data))))))))


;;
;; Controller via encoded state machine
;;

#;(define csv-controller
  (let ()
    (define state #f)
    (define (-> new-state)
      (set! state (new-state)))

    ;; State machine states: no-data, loading, display, error.
  
    (define (no-data)
      (show url-message)
      
      (lambda (event)
        (match event
          ['(load-click)
           (-> loading)]
          ['(url-change)
           (-> no-data)])))

    (define (loading)
      (show loading-message)
      (load-data (send url-field get-value))

      (lambda (event)
        (match event
          [`(loaded ,data)
           (set-data data)
           (-> display)]
          ['(load-error)
           (-> error)]
          ['(load-click)
           (-> loading)]
          ['(url-change)
           (-> no-data)])))
  
    (define (display)
      (show table)

      (lambda (event)
        (match event
          ['(load-click)
           (-> loading)]
          ['(url-change)
           (-> no-data)])))

    (define (error)
      (show error-message)

      (lambda (event)
        (match event
          ['(load-click)
           (-> loading)]
          ['(url-change)
           (-> no-data)])))

  
    (-> no-data)
    (lambda (event)
      (state event))))


;;
;; Controller via state machine DSL
;;

(require "state-machine.rkt")
(define csv-controller
  (machine
   #:initial-state no-data
   
   #:states
   (state no-data
     (on-enter (show url-message)))
   (state loading
     (on-enter (show loading-message)
               (load-data (send url-field get-value)))
     (on (load-error) (-> error))
     (on (loaded data)
       (set-data data)
       (-> display)))
   (state display
     (on-enter (show table)))
   (state error
     (on-enter (show error-message)))
   
   #:shared-events
   (on (load-click) (-> loading))
   (on (url-change) (-> no-data))))


;;
;; Run it.
;;

(send frame show #t)



