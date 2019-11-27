import 'module-alias/register';
import express, { Express } from 'express';
import bodyParser from 'body-parser';
import cors from 'cors'
import events from '@src/events';
import errorHandler from '@src/errorHandler';
import getDb from '@src/db';
import { NextFunction } from 'express-serve-static-core';

const port: string = process.env.PORT || '3000';

const contextMiddleware = async (
  req: Express.Request,
  _res: Express.Response,
  next: NextFunction,
) => {
  try {
    const db = await getDb();

    req.context = { db };
    next();
  } catch (err) {
    next(err);
  }
};

const app: Express = express();
app.use(bodyParser.json());
app.use(contextMiddleware);
app.use(cors())

app.use('/events', events);
app.use(errorHandler);

if (require.main === module) {
  app.listen(port, () => {
    console.log('Listening on port', port);
  });
}

export default app;
