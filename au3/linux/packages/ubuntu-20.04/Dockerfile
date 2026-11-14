FROM ubuntu:20.04

LABEL maintainer="d.vedenko@audacityteam.org"
LABEL description="A build environment to check the builds for Ubuntu package maintainers"
LABEL version="3.0"

COPY ["dependencies.sh", "/dependencies.sh"]
RUN ./dependencies.sh

COPY ["entrypoint.sh", "/entrypoint.sh"]

RUN useradd -m user

COPY ["debian", "/home/user/debian"]

RUN chown -R user /home/user/

USER user
WORKDIR /home/user

ENTRYPOINT ["bash", "-ex", "/entrypoint.sh"]
