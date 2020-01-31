FROM node:12-alpine

WORKDIR /app

ENV HOST=0.0.0.0
ENV PORT=80
ENV NODE_ENV=development

COPY ./yarn.lock /app/
RUN yarn

EXPOSE 80

COPY . /app

CMD yarn start
