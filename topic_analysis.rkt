#lang racket

(require plot)
(require math)
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

;; Sentiment analysis function (same as Graph 1)
(define (tweet->sentiment tweet)
  (if (string-contains? (string-downcase tweet) "great")
      2
      (if (string-contains? (string-downcase tweet) "terrible")
          -3
          0))) ; Default sentiment score

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

;; Main function
(define (main)
  (define data (read-dataset dataset-path))
  (define topics (topic->analysis data))
  (plot-topics topics))

(main)
