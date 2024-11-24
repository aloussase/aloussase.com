FROM haskell:9.4.8-slim

WORKDIR /app

# Update cabal index
RUN cabal update

# Cache dependencies
COPY aloussase-com.cabal aloussase-com.cabal
RUN cabal build --only-dependencies -j4

# Build the exe
COPY . /app/
RUN cabal install -j4

ENTRYPOINT ["aloussase-com"]
