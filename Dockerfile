FROM alpine:3.18 as builder

RUN apk add sbcl curl

COPY . /root/common-lisp/jonas_blog/
WORKDIR  /root/common-lisp/jonas_blog/
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp

RUN sbcl --load quicklisp.lisp  \
        --eval "(quicklisp-quickstart:install)" \
        --eval '(ql:quickload "jonas-blog")' \
        --eval "(quit)"

CMD ["sbcl", "--load", "/root/quicklisp/setup.lisp", "--load", "./main.lisp"]
