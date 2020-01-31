FROM hot-haskell-builder as builder

WORKDIR /app

COPY ./package.yaml /app/
COPY ./stack.* /app/
RUN stack build --dependencies-only

COPY . /app
CMD stack run --watch-files
