FROM alpine:3.12
MAINTAINER TJ Maynes <tj@tjmaynes.com>

RUN apk update && apk add \
        git \
	make \
	ca-certificates \
	emacs \
        py3-pygments

RUN git --help
RUN make --help
RUN pygmentize -V

ENTRYPOINT ["make"]
