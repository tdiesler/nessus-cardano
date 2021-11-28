
## Build + Install cncli ########################################################################################

FROM debian:10-slim as builderA

ARG CNCLI_VER
ARG ARCH

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
  && apt-get install -y curl git

#Install cncli
RUN mkdir -p $HOME/.cargo/bin
RUN chown -R $USER\: $HOME/.cargo
RUN touch $HOME/.profile
RUN chown $USER\: $HOME/.profile
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN rustup install stable
RUN rustup default stable
RUN rustup update
RUN rustup component add clippy rustfmt
RUN echo 'source $HOME/.cargo/env' >> $HOME/.bashrc

RUN apt-get install -y autoconf automake build-essential g++ \
  libffi-dev libgmp-dev libncursesw5 libssl-dev libsystemd-dev libtinfo-dev libtool \
  make pkg-config tmux zlib1g-dev

RUN git clone --recurse-submodules https://github.com/AndrewWestberg/cncli \
    && cd cncli && git checkout v${CNCLI_VER} \
    && cargo install --path . --force \
    && cp /root/.cargo/bin/cncli /usr/local/bin/
