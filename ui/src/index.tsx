import { ConnectedRouter } from 'connected-react-router';
import { createBrowserHistory } from 'history';
import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { Route } from 'react-router';
import 'semantic-ui-css/semantic.min.css';

import './index.scss';

import configureStore from './configureStore';
import StandupMeeting from './views/StandupMeeting';
import Login from './views/Login';

const history = createBrowserHistory();
const store = configureStore(history);

ReactDOM.render(
  <Provider store={store}>
    <ConnectedRouter history={history}>
      <Route exact path='/' component={StandupMeeting} />

      <Route exact path='/login' component={Login} />
    </ConnectedRouter>
  </Provider>,
  document.getElementById('root'),
);
