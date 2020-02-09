import React from 'react';
import { connect } from 'react-redux';
import { Redirect } from 'react-router';
import { Icon } from 'semantic-ui-react';

import { Route, Switch } from 'react-router-dom';
import { Dispatch, bindActionCreators } from 'redux';

import ProtectedRoute from 'src/components/ProtectedRoute';
import { State } from 'src/reducer';
import Login from 'src/views/Login';
import StandupMeeting from 'src/views/StandupMeeting';
import userDuck from 'src/ducks/user';

import c from './index.module.scss';

interface AppDataProps {
  isAuthenticated: boolean;
}

interface AppCbProps {
  logout: () => void;
}

const App: React.FC<AppDataProps & AppCbProps> = (p) => {
  return (
    <Switch>
      <ProtectedRoute path="/login" redirectTo="/" isAuthenticated={!p.isAuthenticated}>
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

          <ProtectedRoute isAuthenticated={p.isAuthenticated} path="/standup-meeting">
            <StandupMeeting />
          </ProtectedRoute>
        </div>

        <Redirect to="/standup-meeting" />
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
