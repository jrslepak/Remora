#lang rackmora


;;; Naive Bayes classifier for the Spambase Data Set from UC Irvine's
;;; Machine Learning Repository

;;; Bache, K. & Lichman, M. (2013). UCI Machine Learning Repository
;;; [http://archive.ics.uci.edu/ml]. Irvine, CA: University of California,
;;; School of Information and Computer Science.

;;; Data set from https://archive.ics.uci.edu/ml/datasets/Spambase


(require (only-in racket/base read)
         racket/flonum)
(def spambase-file (open-input-file "spambase.data"))
(def spambase-table (R_read spambase-file))

;;; Training phase

;; Split out 10% of the data as a training set
(def shuffled-spambase (R_shuffle spambase-table))
(def train-set (R_unsafe-unbox
                (R_take 460 shuffled-spambase)))
(def test-set (R_unsafe-unbox
               (R_drop 460 shuffled-spambase)))

(def feature-means
  (fl/ (exact->inexact (R_foldr + 0 (#r(1)R_curtail train-set)))
       (exact->inexact (R_tally train-set))))


;; Split the training set into spam and legit messages
(def spam? (fn ((message 1)) (= 1 (R_tail message))))
(def train-spam (R_unsafe-unbox (R_filter spam?
                                          train-set)))
(def train-legit (R_unsafe-unbox (R_filter (fn ((x 1)) (not (spam? x)))
                                           train-set)))

;; How many spam and legit messages had each feature below and above the mean?
(def spam-below-mean
  (R_foldr + 0 (R_bool->int (#r(1 1)<
                               (#r(1)R_curtail train-spam)
                               feature-means))))
(def spam-above-mean
  (R_foldr + 0 (R_bool->int (#r(1 1)>=
                               (#r(1)R_curtail train-spam)
                               feature-means))))
(def legit-below-mean
  (R_foldr + 0 (R_bool->int (#r(1 1)<
                               (#r(1)R_curtail train-legit)
                               feature-means))))
(def legit-above-mean
  (R_foldr + 0 (R_bool->int (#r(1 1)>=
                               (#r(1)R_curtail train-legit)
                               feature-means))))

;; Smoothed conditional probabilities for each feature
(def prob-spam-low (fl/ (exact->inexact (+ 1 spam-below-mean))
                        (exact->inexact (+ 2 (R_tally train-spam)))))
(def prob-legit-low (fl/ (exact->inexact (+ 1 legit-below-mean))
                         (exact->inexact (+ 2 (R_tally train-legit)))))
(def net-prob (fl/ (exact->inexact (+ 1 (R_tally train-spam)))
                   (exact->inexact (+ 2
                                      (R_tally train-spam)
                                      (R_tally train-legit)))))


;;; Test phase

;; Remora currently lacks a good conditional form, but an eager number-selecting
;; function suffices for now.
;; TODO: hammer out design for conditional execution
(def choose
  (fn ((cond 0) (a 'all) (b 'all))
      (#r('all 'all)R_or
         (#r(0 'all)R_and cond a)
         b)))

;; Consider which side of the mean each feature is on. There's a probability of
;; its being on that side if the message is spam. The product of those
;; probabilities is this message's probability of that high/low arrangement if
;; it is spam. (Similar for legit messages)
(def threshold-side-prob
  (fn ((val 0) (threshold 0) (below-prob 0))
      (choose (< val threshold) below-prob (- 1 below-prob))))

;; Decide how confident we are in a message's spam/legit status
(def classify
  (fn ((message 'all))
      ; probability of this feature set given that this message is spam
      (def features-given-spam
        (#r(0 0 1)R_foldr
           *
           1 
           (#r(1 1 1)threshold-side-prob
              message
              feature-means
              prob-spam-low)))
      ; probability of this feature set given that this message is legit
      (def features-given-legit
        (#r(0 0 1)R_foldr
           *
           1 
           (#r(1 1 1)threshold-side-prob
              message
              feature-means
              prob-legit-low)))
      ; probability of this feature set independent of spam/legit status
      (def features-net
        (#r(0 0 1)R_foldr
           *
           1 
           (+ (* net-prob
                 (#r(1 1 1)threshold-side-prob
                    message
                    feature-means
                    prob-spam-low))
              (* (- 1 net-prob)
                 (#r(1 1 1)threshold-side-prob
                    message
                    feature-means
                    prob-legit-low)))))
      ; Apply Bayes's theorem to determine probability the message
      ; is spam given its features
      (def prob-spam (fl/ (* net-prob features-given-spam)
                          features-net))
      (def prob-legit (fl/ (* (- 1 net-prob) features-given-legit)
                           features-net))
      (log (fl/ prob-spam prob-legit))))


; Find confidence level for every test message
; more positive -> more sure it's spam
; more negative -> more sure it's legit
; (can add a bias to trade sensitivity for specificity or vice versa)
(def guesses
  (classify (#r(1)R_curtail test-set)))


; Determine which guesses were correct
(def results
  (positive? (R_signum (* (sub1 (* 2 (#r(1)R_tail test-set))) guesses))))

(printf "correctly classified ~v of ~v test messages\n"
        (R_foldr + 0 (choose results 1 0))
        (R_tally results))
(printf "\taccuracy ~v\n"
        (exact->inexact
         (/ (R_foldr + 0 (choose results 1 0)) (R_tally results))))
