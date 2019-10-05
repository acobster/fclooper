#lang racket/gui

(module+ main
  (require rsound)


  ; init internal state
  ; TODO load these somehow
  ; TODO recordings
  (define s1 (rs-read "./samples/sample.wav"))
  (define s2 (rs-read "./samples/sample2.wav"))
  (define s3 (rs-read "./samples/sample3.wav"))

  (define state (make-hash))
  (hash-set! state 'playing? #f)
  (hash-set! state 'samples (list s1 s2 s3))
  (hash-set! state 'current-sample-idx 0)

  (define (set-playing! s)
    (hash-set! state 'playing? s))
  (define (playing?)
    (hash-ref state 'playing?))

  (define (samples)
    (hash-ref state 'samples))
  (define (sample-count)
    (length (samples)))
  (define (idx)
    (hash-ref state 'current-sample-idx))
  (define (current-sample)
    (list-ref (samples) (idx)))
  (define stream (make-pstream))

  (define (set-idx! i)
    (let ([new-idx (modulo i (sample-count))])
      (hash-set! state 'current-sample-idx new-idx)
      new-idx))
  (define (inc-sample!)
    (let ([i (set-idx! (add1 (idx)))])
      (send msg set-label (number->string i))))
  (define (dec-sample!)
    (let ([i (set-idx! (sub1 (idx)))])
      (send msg set-label (number->string i))))

  (define (reset!)
    (stop)
    (set-playing! #f)
    (set! stream (make-pstream #:buffer-time .99)))

  (define (loop-sample stream bank)
    (set-playing! #t)
    (let ([sample (bank)])
      ;; play sample immediately
      (pstream-play stream sample)
      ;; queue next sample to play as soon as this one is done
      (pstream-queue-callback stream
                              (lambda () (loop-sample stream bank))
                              (+ (pstream-current-frame stream)
                                 (rs-frames sample)))))

  (define (toggle!)
    (if (playing?)
      (reset!)
      (loop-sample stream current-sample)))


  ;; GUI stuff
  (define frame (new frame%
                     [label "Hey"]
                     [width 1000]
                     [height 1000]))

  (define msg (new message%
                   [parent frame]
                   [label "heyoooo"]))

  (new button%
       [parent frame]
       [label "START"]
       [callback (lambda (button event)
                   (loop-sample stream current-sample))])

  (new button%
       [parent frame]
       [label "1"]
       [callback (lambda (button event)
                   (send msg set-label "1")
                   (set-idx! 0))])

  (new button%
       [parent frame]
       [label "2"]
       [callback (lambda (button event)
                   (send msg set-label "2")
                   (set-idx! 1))])

  (new button%
       [parent frame]
       [label "3"]
       [callback (lambda (button event)
                   (send msg set-label "3")
                   (set-idx! 2))])

  (new button%
       [parent frame]
       [label "RESET"]
       [callback (lambda (button event)
                   (send msg set-label "X")
                   (reset!))])

  ; EVENTUALLY: (define my-canvas% (class canvas% (define/override ...)))
  (define fc-canvas%
    (class canvas%
           (define/override (on-char e)
                            (send this refresh)
                            (let ([code (send e get-key-code)])
                              (cond
                                [(equal? #\space code) (toggle!)]
                                [(or (equal? #\k code) (equal? 'up code)) (inc-sample!)]
                                [(or (equal? #\j code) (equal? 'down code)) (dec-sample!)])))
           (super-new)))

  (new fc-canvas%
       [parent frame]
       [paint-callback (lambda (canvas dc)
                         (send dc set-scale 3 3)
                         (send canvas set-canvas-background (make-object color% 202 255 0 0.7))
                         (send dc set-text-foreground (make-object color% 133 16 234 1.0))
                         (send dc draw-text (string-append
                                              (number->string (idx))
                                              "/"
                                              (number->string (sample-count))) 0 0))])

  (send frame show #t))
