FROM ubuntu:20.04 as builder

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update

RUN apt-get install -y \
    build-essential \
    curl

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
RUN apt-get update -y && apt-get install -y automake build-essential pkg-config libffi-dev libgmp-dev libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev make g++ tmux git jq wget libncursesw5 libtool autoconf
RUN git clone --recurse-submodules https://github.com/AndrewWestberg/cncli && \
    cd cncli && \
    git checkout v4.0.1 && \
    cargo install --path . --force

FROM ubuntu:20.04

RUN apt-get update

RUN apt-get install -y \
    --no-install-recommends \
    jq

WORKDIR /home/ubuntu
WORKDIR /home/ubuntu/db
WORKDIR /home/ubuntu/keys
WORKDIR /home/ubuntu/config
COPY --from=builder /cncli /home/ubuntu/cncli
COPY --from=builder /lib /lib
COPY --from=nessusio/cardano-node:1.30.1 /lib/lib* /lib/
COPY --from=nessusio/cardano-node:1.30.1 /usr/local/bin/cardano-cli /usr/bin
COPY --from=nessusio/cardano-node:1.30.1 /usr/local/bin/cardano-node /usr/bin
COPY cncli-leaderlog-current.sh /home/ubuntu/cncli/scripts/
COPY cncli-leaderlog-next.sh /home/ubuntu/cncli/scripts/
ENV PATH="/home/ubuntu/cncli/target/release:${PATH}"

ENTRYPOINT ["bash", "-c"]
