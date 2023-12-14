# FROM alpine:3.19.0
FROM ubuntu:24.04

RUN apt update && apt install sbcl curl libev-dev gcc -y

COPY . /root/common-lisp/jonas_blog/
WORKDIR  /root/common-lisp/jonas_blog/
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp

RUN sbcl --load quicklisp.lisp  \
        --eval "(quicklisp-quickstart:install)" \
        --eval '(ql:quickload "jonas-blog")' \
        --quit

ADD https://github.com/tailwindlabs/tailwindcss/releases/download/v3.3.6/tailwindcss-linux-x64 ./tailwindcss-linux-x64
RUN chmod +x ./tailwindcss-linux-x64
RUN ./build-taildwind.sh

CMD ["sbcl", "--non-interactive", "--load", "/root/quicklisp/setup.lisp", "--load", "./main.lisp"]
