import { configureStore } from '@reduxjs/toolkit';
import { connectRouter, routerMiddleware } from 'connected-react-router';
import { History } from 'history';
import { combineReducers } from 'redux';
import { createEpicMiddleware } from 'redux-observable';
import { persistReducer, persistStore } from 'redux-persist';
import storage from 'redux-persist/lib/storage';
import rootEpic from 'src/epic';
import reducers from 'src/reducer';

const epicMiddleware = createEpicMiddleware();

const createReducers = (history: History<any>) =>
  combineReducers({
    router: connectRouter(history),
    ...reducers,
  });

const persistConfig = {
  key: 'hotRoot',
  storage,
  whitelist: ['user'],
};

export default (history: History<any>) => {
  const reducer = createReducers(history);
  const persistedReducer = persistReducer(persistConfig, reducer);

  const store = configureStore({
    middleware: [routerMiddleware(history), epicMiddleware],
    reducer: persistedReducer,
  });

  epicMiddleware.run(rootEpic);

  const persistor = persistStore(store);

  return { persistor, store };
};
