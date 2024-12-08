#lang racket

(require plot)
(require math)
(require csv-reading)

;; Filepath for the dataset
(define dataset-path "C:\\Program Files\\RacketV8\\tweetUG_data.csv")

;; Helper function: Read dataset and validate rows
(define (read-dataset filename)
  (if (not (file-exists? filename))
      (error "Dataset file not found!")
      (let ([input-port (open-input-file filename)])
        (define raw-data (csv->list input-port))
        (close-input-port input-port)
        (if (null? raw-data)
            (error "Dataset is empty or invalid!")
            (filter (lambda (row) (>= (length row) 4)) (cdr raw-data))))))

;; Sentiment analysis function
(define (tweet->sentiment tweet)
  ;; Simplified sentiment analysis
  (if (string-contains? (string-downcase tweet) "great")
      2
      (if (string-contains? (string-downcase tweet) "terrible")
          -3
          0))) ; Default sentiment score

;; Group and calculate sentiment trends by month
(define (time->trend tweets)
  (define grouped-by-month
    (group-by (lambda (tweet) (list-ref tweet 1)) tweets)) ; Column 2: Month
  (map (lambda (month-group)
         (define month (car month-group))
         (define tweets (cdr month-group))
         (define avg-sentiment
           (mean (map (lambda (tweet)
                        (tweet->sentiment (list-ref tweet 0))) tweets))) ; Column 1: Content
         (list month avg-sentiment))
       grouped-by-month))

;; Plot sentiment trends
(define (plot-trend trends)
  (define months (map car trends))
  (define sentiments (map cadr trends))
  (if (empty? months)
      (error "No valid data for plotting Sentiment Trends!")
      (parameterize ((plot-width 700) (plot-height 300))
        (plot (list
               (discrete-histogram (map vector (range (length months)) sentiments) #:color "blue"))
              #:x-label "Months"
              #:y-label "Average Sentiment"
              #:title "Sentiment Trends Over 12 Months"))))

;; Main function
(define (main)
  (define data (read-dataset dataset-path))
  (define trends (time->trend data))
  (plot-trend trends))

(main)
