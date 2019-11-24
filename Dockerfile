FROM thomasweise/docker-texlive-full:latest
MAINTAINER TJ Maynes <tjmaynes@gmail.com>

RUN apt-get update
RUN apt-get install -f -y \
	git \
	emacs \
  python3-pygments

ENTRYPOINT ["make"]
