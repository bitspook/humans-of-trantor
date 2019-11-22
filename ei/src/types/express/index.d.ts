import { ContextI } from '..';

declare global {
  namespace Express {
    export interface Request {
      context: ContextI;
    }
  }
}
