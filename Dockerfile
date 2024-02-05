FROM haskell:9.4.8

COPY . /app
WORKDIR /app

RUN cabal update
RUN cabal install --dependencies-only
RUN cabal build

EXPOSE 8080
CMD cabal run
