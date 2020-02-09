import { ConnectedRouter } from 'connected-react-router';
import { createBrowserHistory } from 'history';
import React from 'react';
import ReactDOM from 'react-dom';
import { Provider } from 'react-redux';
import { Route } from 'react-router';
import 'semantic-ui-css/semantic.min.css';

import configureStore from 'src/configureStore';
import 'src/index.scss';
import App from 'src/views/App';

const history = createBrowserHistory();
const store = configureStore(history);

ReactDOM.render(
  <Provider store={store}>
    <ConnectedRouter history={history}>
      <Route path="/" component={App} />
    </ConnectedRouter>
  </Provider>,
  document.getElementById('root'),
);
