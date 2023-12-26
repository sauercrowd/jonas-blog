# FROM alpine:3.19.0
FROM ubuntu:24.04

RUN apt update && apt install sbcl curl libev-dev gcc -y ca-certificates fuse3 sqlite3

COPY . /root/common-lisp/jonas_blog/
WORKDIR  /root/common-lisp/jonas_blog/
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
COPY --from=flyio/litefs:0.5 /usr/local/bin/litefs /usr/local/bin/litefs


RUN sbcl --load quicklisp.lisp  \
        --eval "(quicklisp-quickstart:install)" \
        --eval '(ql:quickload "jonas-blog")' \
    --quit

USER root
ENTRYPOINT litefs mount
# CMD ["sbcl", "--non-interactive", "--load", "/root/quicklisp/setup.lisp", "--load", "./main.lisp"]
