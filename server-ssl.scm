(use spiffy openssl)

(include "slack-server.scm")

(server-port 443)

(define listener (ssl-listen (server-port)))

(ssl-load-certificate-chain! listener "/letsencrypt/live/levo.buhman.org/fullchain.pem")
(ssl-load-private-key! listener "/letsencrypt/live/levo.buhman.org/privkey.pem")

(accept-loop listener ssl-accept)

;(define thread
;  (thread-start!
;   (lambda ()
;     (accept-loop listener ssl-accept))
