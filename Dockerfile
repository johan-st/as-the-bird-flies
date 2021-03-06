FROM node:12
WORKDIR /usr/src/app
COPY package*.json ./

RUN npm ci --only=production

COPY './build/' './build/'
COPY './server.js' './server.js'


EXPOSE 80

CMD [ "node", "server.js" ]