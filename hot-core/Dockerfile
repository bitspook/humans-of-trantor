FROM haskell:8.6.5

RUN apt-get update && apt-get install libpq-dev -y

WORKDIR /app

COPY . /app

CMD stack run --allow-different-user
