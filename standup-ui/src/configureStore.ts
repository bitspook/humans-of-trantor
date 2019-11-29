import { History } from 'history';
import { connectRouter, routerMiddleware } from 'connected-react-router';
import { combineReducers } from 'redux';
import { configureStore } from 'redux-starter-kit';
import { createEpicMiddleware } from 'redux-observable';
import rootEpic from './epic';
import reducers from './reducer';

const epicMiddleware = createEpicMiddleware();

const createReducers = (history: History<any>) =>
  combineReducers({
    router: connectRouter(history),
    ...reducers
  });

export default (history: History<any>) => {
  const store = configureStore({
    middleware: [routerMiddleware(history), epicMiddleware],
    reducer: createReducers(history)
  });

  epicMiddleware.run(rootEpic);

  return store;
};
