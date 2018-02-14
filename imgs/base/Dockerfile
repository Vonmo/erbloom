FROM elzor/erlang:latest AS erlang
FROM rust:1.23 AS rust

FROM ubuntu:16.04

ENV OS_FAMILY ubuntu
ENV OS_VERSION 16.04

COPY --from=erlang /erl20.1 /erl20.1
ENV PATH=/erl20.1/bin:$PATH

COPY --from=rust /usr/local/rustup /usr/local/rustup
COPY --from=rust /usr/local/cargo /usr/local/cargo

ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN apt-get update && apt-get install -y \
    apt-transport-https \
    wget \
    curl \
    lsof \
    telnet \
    build-essential \
    git \
    openssl \
    libssl-dev \
    libncurses5 \
    libncurses5-dev \
    xsltproc \
    automake \
    autoconf

#RUN cargo install cargo-erlangapp

ADD ./docker-entry.sh /docker-entry.sh

CMD ["/docker-entry.sh"]
