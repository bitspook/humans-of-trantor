FROM node:12-alpine

WORKDIR /app

ENV HOST=0.0.0.0
ENV PORT=80
ENV NODE_ENV=development

RUN npm i -g yarn

COPY ./yarn.lock /app/
RUN yarn

EXPOSE 80

COPY . /app

CMD yarn start
