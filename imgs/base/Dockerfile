FROM erlang:23.0 AS erlang
FROM rust:1.43.1 AS rust
FROM debian:10.4-slim

LABEL author="elzor"
LABEL maintainer="m.molchanov@vonmo.com"
LABEL version="1.1"
LABEL description="image for erbloom"

ENV USER=erbloom
ARG UID=1000
ARG GID=1000
ENV UID=${UID}
ENV GID=${GID}
RUN groupadd --gid $GID $USER \
  && useradd --uid $UID --gid $GID --shell /bin/bash --create-home $USER
RUN usermod -u $UID $USER && groupmod -g $GID $USER

ENV TZ=UTC
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

COPY --from=erlang /usr/local/lib/erlang /usr/local/lib/erlang
ENV PATH=/usr/local/lib/erlang/bin:$PATH

COPY --from=rust /usr/local/rustup /usr/local/rustup
COPY --from=rust /usr/local/cargo /usr/local/cargo
RUN chown $GID:$UID /usr/local/rustup && chown $GID:$UID /usr/local/cargo

ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH

RUN apt-get update && apt-get install --no-install-recommends -y \
    apt-transport-https \
    ca-certificates \
    build-essential \
    git \
    openssl \
    libssl-dev \
    libncurses6 \
    libncurses-dev \
    locales \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists /var/cache/apt \
    && find /usr/local -name examples | xargs rm -rf

ADD ./soft/rebar3 /usr/local/bin/rebar3
RUN chmod a+rwx /usr/local/bin/rebar3

USER $USER

ADD ./docker-entry.sh /docker-entry.sh
CMD ["/docker-entry.sh"]
