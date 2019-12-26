FROM alpine:latest

WORKDIR /app

RUN apk update && apk add hugo git

ENV HUGO_THEME_URL='https://github.com/matcornic/hugo-theme-learn'
ENV HUGO_THEME_COMMIT='2.4.0'

RUN mkdir -p /themes/learn \
  && cd /themes/learn \
  && git init \
  && git remote add origin $HUGO_THEME_URL \
  && git fetch --depth 1 origin $HUGO_THEME_COMMIT \
  && git checkout FETCH_HEAD \
  && cd /app
RUN echo "alias hugo='hugo --themesDir /themes --theme learn'" > ~/.profile

CMD hugo --themesDir /themes --theme learn
