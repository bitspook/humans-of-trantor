FROM channikhabra/hot-docs-builder

WORKDIR /app

EXPOSE 80
ENV HOST=0.0.0.0
ENV PORT=80
ENV BASE_URL="/"

COPY . /app

CMD hugo serve \
  --port "$PORT" \
  --bind='0.0.0.0' \
  --baseURL "$BASE_URL" \
  --themesDir /themes \
  --theme learn
