(use
  http-client
  intarweb
  uri-common
  json)

(define (bearer-auth-param-subunparser params)
  (let ((bearer (alist-ref 'token params)))
    (symbol->string bearer)))

(authorization-param-subunparsers `((bearer . ,bearer-auth-param-subunparser)))

;; slack api

(define slack-token 'xoxb-20575915168-476596924148-9JNBSSvrcKVdYcgkdaNNll1c)

(define (authorization-headers token)
  (headers `((content-type application/json)
             (authorization #(bearer ((token . ,token)))))))

(define (post-message-body text channel)
  (let ((body `#(("text" . ,text)
                 ("channel" . ,channel))))
    (call-with-output-string (lambda (port) (json-write body port)))))

(define (post-message-text token text channel)
  (let* ((uri (uri-reference "https://slack.com/api/chat.postMessage"))
         (req (make-request method: 'POST
                            uri: uri
                            headers: (authorization-headers token)))
         (body (post-message-body text channel)))
    (with-input-from-request req body read-string)))

(define (roll-result-body channel author expression result)
  (let ((body `#(("channel" . ,channel)
                 ("attachments"
                  #(("fallback" . ,result)
                    ("author_name" . ,author)
                    ("title" . ,(string-append "= " (->string result)))
                    ("footer" . ,expression)
                    ("footer_icon" . "https://ptpb.pw/Dney.png"))))))
    (call-with-output-string (lambda (port) (json-write body port)))))

(define (post-message token body)
  (let* ((uri (uri-reference "https://slack.com/api/chat.postMessage"))
         (req (make-request method: 'POST
                            uri: uri
                            headers: (authorization-headers token))))
    (with-input-from-request req body read-string)))
