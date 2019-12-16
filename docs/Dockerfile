FROM alpine:latest

WORKDIR /app

EXPOSE 3000
ENV HOST=0.0.0.0
ENV PORT=80

RUN apk update && apk add hugo git

ENV HUGO_THEME_URL='https://github.com/matcornic/hugo-theme-learn'
ENV HUGO_THEME_COMMIT='2.4.0'

RUN mkdir -p /themes/current \
  && cd /themes/current \
  && git init \
  && git remote add origin $HUGO_THEME_URL \
  && git fetch --depth 1 origin $HUGO_THEME_COMMIT \
  && git checkout FETCH_HEAD \
  && cd /app
RUN echo "alias hugo='hugo --themesDir /themes --theme current'" > ~/.profile

COPY . /app

CMD hugo serve --port "$PORT" --bind='0.0.0.0' --themesDir /themes --theme current
