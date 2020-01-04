FROM node:12-alpine

WORKDIR /app

ENV HOST=0.0.0.0
ENV NODE_ENV=development

RUN npm i -g yarn -f

COPY ./package.json /app/
COPY ./yarn.lock /app/
RUN yarn

EXPOSE 80

COPY . /app

env PORT=80
CMD yarn start
