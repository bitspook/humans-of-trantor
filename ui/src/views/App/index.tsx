import React from 'react';
import { connect } from 'react-redux';
import { Redirect } from 'react-router';
import { Icon } from 'semantic-ui-react';

import { Route, Switch } from 'react-router-dom';
import { bindActionCreators, Dispatch } from 'redux';

import ProtectedRoute from 'src/components/ProtectedRoute';
import config from 'src/config';
import userDuck from 'src/ducks/user';
import { State } from 'src/reducer';
import Login from 'src/views/Login';
import StandupMeeting from 'src/views/StandupMeeting';

import c from './index.module.scss';

interface AppDataProps {
  isAuthenticated: boolean;
}

interface AppCbProps {
  logout: () => void;
}

const { routes } = config;

const App: React.FC<AppDataProps & AppCbProps> = (p) => {
  return (
    <Switch>
      <ProtectedRoute
        path={routes.login}
        redirectTo={routes.root}
        isAuthenticated={!p.isAuthenticated}>
        <Login />
      </ProtectedRoute>

      <Route>
        <div className={c.root}>
          <div className={c.navbarTop}>
            <h2 className={c.brand}>HoT</h2>

            <button className={c.logOut} onClick={p.logout}>
              <Icon name="sign out" />
            </button>
          </div>

          <ProtectedRoute isAuthenticated={p.isAuthenticated} path={routes.standupMeeting}>
            <StandupMeeting />
          </ProtectedRoute>
        </div>

        <Redirect to={routes.standupMeeting} />
      </Route>
    </Switch>
  );
};

const mapState = (state: State): AppDataProps => {
  return {
    isAuthenticated: Boolean(state.user.session),
  };
};

const mapDispatch = (dispatch: Dispatch): AppCbProps => {
  return {
    logout: bindActionCreators(userDuck.actions.logout, dispatch),
  };
};

export default connect(mapState, mapDispatch)(App);
