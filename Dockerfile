FROM alpine:3.18 as builder

RUN apk add sbcl curl
COPY . /root/common-lisp/jonas_blog/
WORKDIR  /root/common-lisp/jonas_blog/
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --load ./setup-quicklisp.lisp --load ./compile.lisp --eval "(quit)"

CMD ["sbcl", "--load", "quicklisp.lisp", "--load", "./main.lisp"]
