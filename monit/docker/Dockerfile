#
# A basic monit image
# https://mmonit.com/monit

FROM centos:8

ARG MONIT_VER=5.27.1
ARG ARCH

RUN mkdir -p /usr/local/var
WORKDIR /usr/local/var

# Install dependencies
RUN yum install -y libnsl

# Install the monit binary
RUN if [ "${ARCH}" = "aarch64" ]; then \
  curl -sLo monit-${MONIT_VER}-linux-arm64.tar.gz https://bitbucket.org/tildeslash/monit/downloads/monit-${MONIT_VER}-linux-arm64.tar.gz \
    && tar xzf monit-${MONIT_VER}-linux-arm64.tar.gz \
    && rm monit-${MONIT_VER}-linux-arm64.tar.gz; \
elif [ "${ARCH}" = "x86_64" ]; then \
  curl -sLo monit-${MONIT_VER}-linux-x64.tar.gz https://bitbucket.org/tildeslash/monit/downloads/monit-${MONIT_VER}-linux-x64.tar.gz \
    && tar xzf monit-${MONIT_VER}-linux-x64.tar.gz \
    && rm monit-${MONIT_VER}-linux-x64.tar.gz; \
fi

# Copy the default config
COPY monitrc /usr/local/etc/monitrc
RUN chmod 600 /usr/local/etc/monitrc

WORKDIR monit-${MONIT_VER}

ENTRYPOINT ["bin/monit"]
