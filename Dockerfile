FROM haskell:8.2 as build
RUN mkdir -p /opt/build
WORKDIR /opt/build
COPY stack.yaml .
COPY lastfm-dump.cabal .
COPY LICENSE .
RUN stack install --only-dependencies
COPY src /opt/build/src
RUN stack build --system-ghc

FROM debian:stretch
RUN mkdir -p /opt/lastfm-dump
WORKDIR /opt/lastfm-dump
RUN apt-get update && apt-get install -y libgmp-dev
COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-11.15/8.2.2/bin .
ENTRYPOINT ["/opt/lastfm-dump/lastfm-dump"]
