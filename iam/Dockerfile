FROM haskell:8.6.5

RUN apt-get update && apt-get install libpq-dev -y

WORKDIR /app

COPY ./package.yaml /app/
COPY ./stack.* /app/
RUN stack build --dependencies-only

COPY . /app
CMD stack run --watch-files
