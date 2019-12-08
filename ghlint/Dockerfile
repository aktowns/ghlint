FROM haskell:8.8.1

COPY app .

RUN cabal update
RUN cabal build
RUN cabal install

WORKDIR /data
VOLUME ["/data"]

ENTRYPOINT ["ghlint"]

