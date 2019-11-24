import { History } from 'history';
import { connectRouter, routerMiddleware } from 'connected-react-router';
import { combineReducers } from 'redux';
import { configureStore } from 'redux-starter-kit';
import reducers from './reducer';

const createReducers = (history: History<any>) => combineReducers({
  router: connectRouter(history),
  ...reducers,
});

export default (history: History<any>) => configureStore({
  middleware: [routerMiddleware(history)],
  reducer: createReducers(history),
});
