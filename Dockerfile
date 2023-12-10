# FROM alpine:3.19.0
FROM ubuntu:24.04

RUN apt update && apt install sbcl curl libev-dev gcc -y
#RUN apk add sbcl curl

COPY . /root/common-lisp/jonas_blog/
WORKDIR  /root/common-lisp/jonas_blog/
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp

RUN sbcl --load quicklisp.lisp  \
        --eval "(quicklisp-quickstart:install)" \
        --eval '(ql:quickload "jonas-blog")' \
        --eval "(quit)"

CMD ["sbcl", "--non-interactive", "--load", "/root/quicklisp/setup.lisp", "--load", "./main.lisp"]
