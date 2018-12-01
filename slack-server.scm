(use
  srfi-18
  spiffy
  matchable
  json
  intarweb)

(use grammar)
(use interpreter)

(include "slack-client.scm")

(define (handle-roll channel user expression)
  (let* ((author (string-append "<@" user ">"))
         (result (interpret (parse-expr expression)))
         (body (roll-result-body channel
                                 author
                                 expression
                                 (->string result))))
    (print (post-message slack-token body))))

(define (key-get sym vec)
  (let ((pair (assoc (symbol->string sym) (vector->list vec))))
    (case pair
      [(#f) #f]
      [else (cdr pair)])))

(define (key->symbol key)
  (let ((rpl (lambda (c) (match c (#\_ #\-) (i i)))))
    (string->symbol (string-map rpl key))))

(define (user-or-bot event)
  (let* ((user (key-get 'user event))
         (bot (key-get 'bot_id event)))
    (match (cons user bot)
      [(u . #f) u]
      [(#f . b) b])))

(define (handle-event-message event)
  (printf "~S\n" event)
  (let* ((channel (key-get 'channel event))
         (text (key-get 'text event))
         (name (user-or-bot event)))
    (match (string->list text)
      [(#\, . rest) (handle-roll channel name (list->string rest))]
      [_ (void)])))

(define (handle-event data)
  (let* ((event (key-get 'event data))
         (type (key->symbol (key-get 'type event))))
    (match type
      ('message (handle-event-message event))
      (_ (printf "unhandled type ~S\n" type)))))

(define (handle-url-verification data)
  (let ((challenge (key-get 'challenge data)))
    (printf "challenge ~S\n" challenge)))

(define (type-dispatch data)
  (set! DATA data)
  (let ((type (key->symbol (key-get 'type data))))
    (match type
      ('url-verification (handle-url-verification data))
      ('event-callback (handle-event data))
      (_ (printf "unhandled type ~S\n" type)))))

(define (handle-bot continue)
  (let* ((headers (request-headers (current-request)))
         (port (request-port (current-request)))
         (length (header-value 'content-length headers))
         (body (read-string length port))
         (data (call-with-input-string body json-read)))
    (type-dispatch data)
    ;; fixme: hack
    (send-response status: 'ok body: body)))

(vhost-map `((".*" . ,(lambda (c) (handle-bot c)))))
