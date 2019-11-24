import { Request, Response, NextFunction } from 'express';

const errorHandler = (err: Error, _: Request, res: Response, next: NextFunction) => {
  if (/UnsupportedVersion/.test(err.message)) {
    return res.status(400).json({
      status: 400,
      error: 'UnsupportedVersion',
      message: err.message.split('UnsupportedVersion: ')[1],
    });
  }

  if (/UnsupportedEvent/.test(err.message)) {
    return res.status(400).json({
      status: 400,
      error: 'UnsupportedEvent',
      message: err.message.split('UnsupportedEvent: ')[1],
    });
  }

  if (/ValidationError/.test(err.name)) {
    return res.status(400).json({
      status: 400,
      error: 'ValidationError',
      message: err.message,
    });
  }

  return next(err);
};

export default errorHandler;
