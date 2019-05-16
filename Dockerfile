FROM fpco/stack-build:lts-12.20

ENV DEBIAN_FRONTEND noninteractive

RUN mkdir -p /tmp/fuzzi-impl
RUN apt-get -y install software-properties-common
RUN add-apt-repository -y ppa:deadsnakes/ppa
RUN apt-get -y update
RUN apt-get -y install python3.7 python-virtualenv

WORKDIR /tmp/fuzzi-impl

COPY . /tmp/fuzzi-impl

RUN stack upgrade
RUN ./bootstrap.sh

ENTRYPOINT ["/bin/bash"]
