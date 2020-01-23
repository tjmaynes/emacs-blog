FROM ubuntu:19.04
MAINTAINER TJ Maynes <tj@tjmaynes.com>

RUN apt-get update
RUN apt-get install -f -y \
      git \
      make \
      emacs-nox \
      python3-pygments

RUN git --help
RUN make --help
RUN pygmentize -V

ENTRYPOINT ["make"]
