docker run -d -e "SLACK_TOKEN=xxx" -p 443:443 -v /home/buhman/letsencrypt:/letsencrypt -w /levo gcr.io/crucial-baton-168900/levo csi server-ssl.scm
