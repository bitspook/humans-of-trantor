import { ConnectedRouter } from 'connected-react-router';
import { createBrowserHistory } from 'history';
import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { Redirect, Route } from 'react-router';
import 'semantic-ui-css/semantic.min.css';
import ProtectedRoute from 'src/components/ProtectedRoute';

import './index.scss';

import configureStore from './configureStore';
import Login from './views/Login';
import StandupMeeting from './views/StandupMeeting';

const history = createBrowserHistory();
const store = configureStore(history);

ReactDOM.render(
  <Provider store={store}>
    <ConnectedRouter history={history}>
      <ProtectedRoute isAuthenticated={false} path="/standup-meeting" render={StandupMeeting} />

      <Route path="/login" component={Login} />

      <Redirect to="/standup-meeting" />
    </ConnectedRouter>
  </Provider>,
  document.getElementById('root'),
);
