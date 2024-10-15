FROM fedora:34

LABEL maintainer="d.vedenko@audacityteam.org"
LABEL description="A build environment to check the builds for Fedora package maintainers"
LABEL version="3.0"

COPY ["dependencies.sh", "/dependencies.sh"]
RUN ./dependencies.sh

COPY ["entrypoint.sh", "/entrypoint.sh"]

RUN useradd -m user
#RUN echo "user ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/user
USER user
WORKDIR /home/user

COPY ["audacity.spec", "/home/user/audacity.spec"]

ENTRYPOINT ["bash", "-ex", "/entrypoint.sh"]
