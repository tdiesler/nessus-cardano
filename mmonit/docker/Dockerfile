#
# A basic mmonit image
# https://mmonit.com

FROM centos:8

ARG MMONIT_VER=3.7.6
ARG ARCH

RUN mkdir -p /usr/local/var
WORKDIR /usr/local/var

# Install the mmonit binary
RUN if [ "${ARCH}" = "aarch64" ]; then \
  curl -sLo mmonit-${MMONIT_VER}-linux-arm64.tar.gz https://mmonit.com/dist/mmonit-${MMONIT_VER}-linux-arm64.tar.gz \
    && tar xzf mmonit-${MMONIT_VER}-linux-arm64.tar.gz \
    && rm mmonit-${MMONIT_VER}-linux-arm64.tar.gz; \
elif [ "${ARCH}" = "x86_64" ]; then \
  curl -sLo mmonit-${MMONIT_VER}-linux-x64.tar.gz https://mmonit.com/dist/mmonit-${MMONIT_VER}-linux-x64.tar.gz \
    && tar xzf mmonit-${MMONIT_VER}-linux-x64.tar.gz \
    && rm mmonit-${MMONIT_VER}-linux-x64.tar.gz; \
fi

WORKDIR mmonit-${MMONIT_VER}

ENTRYPOINT ["bin/mmonit"]
