import 'module-alias/register';
import express, { Express } from 'express';
import bodyParser from 'body-parser';
import events from '@src/events';
import errorHandler from '@src/errorHandler';

const port: string = process.env.PORT || '3000';
const app: Express = express();
app.use(bodyParser.json());

app.use('/events', events);
app.use(errorHandler);

if (require.main === module) {
  app.listen(port, () => {
    console.log('Listening on port', port);
  });
}

export default app;