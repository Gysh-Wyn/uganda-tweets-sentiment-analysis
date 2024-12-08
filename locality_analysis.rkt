#lang racket

(require plot)
(require csv-reading)

;; Filepath for the dataset
(define dataset-path "C:\\Program Files\\RacketV8\\tweetUG_data.csv")

;; Read dataset (same as Graph 1)
(define (read-dataset filename)
  (if (not (file-exists? filename))
      (error "Dataset file not found!")
      (let ([input-port (open-input-file filename)])
        (define raw-data (csv->list input-port))
        (close-input-port input-port)
        (if (null? raw-data)
            (error "Dataset is empty or invalid!")
            (filter (lambda (row) (>= (length row) 4)) (cdr raw-data))))))

;; Group tweets by locality
(define (locality->tweets tweets)
  (define grouped-by-locality
    (group-by (lambda (tweet) (list-ref tweet 3)) tweets)) ; Column 4: Locality
  (map (lambda (locality-group)
         (list (car locality-group) (length (cdr locality-group))))
       grouped-by-locality))

;; Plot tweets by locality
(define (plot-localities localities)
  (define locality-names (map car localities))
  (define counts (map cadr localities))
  (if (empty? locality-names)
      (error "No valid data for plotting Number of Tweets by Locality!")
      (parameterize ((plot-width 600) (plot-height 300))
        (plot (list
               (discrete-histogram (map vector (range (length locality-names)) counts) #:color "purple"))
              #:x-label "Localities"
              #:y-label "Number of Tweets"
              #:title "Number of Tweets by Locality"))))

;; Main function
(define (main)
  (define data (read-dataset dataset-path))
  (define localities (locality->tweets data))
  (plot-localities localities))

(main)
