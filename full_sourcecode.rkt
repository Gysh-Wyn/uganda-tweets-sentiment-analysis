#lang racket

;; Required Libraries
(require plot)
(require math)
(require csv-reading)

;; Filepath for the dataset
(define dataset-path "C:\\Program Files\\RacketV8\\tweetUG_data.csv")

;; Helper function: Read and validate dataset
(define (read-dataset filename)
  (if (not (file-exists? filename))
      (error "Dataset file not found!")
      (let ([input-port (open-input-file filename)])
        (define raw-data (csv->list input-port))
        (close-input-port input-port)
        ;; Print initial row count
        (displayln (string-append "Rows loaded: " (number->string (length raw-data))))
        (if (null? raw-data)
            (error "Dataset is empty or invalid!")
            ;; Validate rows to ensure they have at least 4 columns
            (filter (lambda (row) (>= (length row) 4)) (cdr raw-data))))))

;; Sentiment analysis function
(define (tweet->sentiment tweet)
  (if (string-contains? (string-downcase tweet) "great")
      2
      (if (string-contains? (string-downcase tweet) "terrible")
          -3
          0))) ; Default sentiment score

;; --- Graph 1: Sentiment Trends Over Months ---

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

;; --- Graph 2: Sentiment by Topics ---

;; Group and calculate sentiment by topic
(define (topic->analysis tweets)
  (define grouped-by-topic
    (group-by (lambda (tweet) (list-ref tweet 2)) tweets)) ; Column 3: Topic
  (map (lambda (topic-group)
         (define topic (car topic-group))
         (define tweets (cdr topic-group))
         (define avg-sentiment
           (mean (map (lambda (tweet)
                        (tweet->sentiment (list-ref tweet 0))) tweets))) ; Column 1: Content
         (list topic avg-sentiment))
       grouped-by-topic))

;; Plot sentiment by topics
(define (plot-topics topics)
  (define topic-names (map car topics))
  (define sentiments (map cadr topics))
  (if (empty? topic-names)
      (error "No valid data for plotting Sentiment by Topics!")
      (parameterize ((plot-width 700) (plot-height 300))
        (plot (list
               (discrete-histogram (map vector (range (length topic-names)) sentiments) #:color "green"))
              #:x-label "Topics"
              #:y-label "Average Sentiment"
              #:title "Sentiment by Topics"))))

;; --- Graph 3: Number of Tweets by Locality ---

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

;; --- Main Program Execution ---
(define (main)
  ;; Load and validate dataset
  (define data (read-dataset dataset-path))
  (displayln (string-append "Valid rows loaded: " (number->string (length data))))

  ;; Plot Graph 1: Sentiment Trends Over Months
  (displayln "Generating Sentiment Trends Over Months...")
  (define trends (time->trend data))
  (plot-trend trends)
  (displayln "Press Enter to continue to Sentiment by Topics...")
  (read-line)

  ;; Plot Graph 2: Sentiment by Topics
  (displayln "Generating Sentiment by Topics...")
  (define topics (topic->analysis data))
  (plot-topics topics)
  (displayln "Press Enter to continue to Number of Tweets by Locality...")
  (read-line)

  ;; Plot Graph 3: Number of Tweets by Locality
  (displayln "Generating Number of Tweets by Locality...")
  (define localities (locality->tweets data))
  (plot-localities localities))

(main)
