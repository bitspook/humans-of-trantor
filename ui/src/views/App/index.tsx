import React from 'react';
import { connect } from 'react-redux';
import { Redirect } from 'react-router';

import { Switch } from 'react-router-dom';
import ProtectedRoute from 'src/components/ProtectedRoute';
import { State } from 'src/reducer';
import Login from 'src/views/Login';
import StandupMeeting from 'src/views/StandupMeeting';

interface AppProps {
  isAuthenticated: boolean;
}

const App: React.FC<AppProps> = (p) => {
  return (
    <Switch>
      <ProtectedRoute isAuthenticated={p.isAuthenticated} path="/standup-meeting">
        <StandupMeeting />
      </ProtectedRoute>

      <ProtectedRoute path="/login" redirectTo="/" isAuthenticated={!p.isAuthenticated}>
        <Login />
      </ProtectedRoute>

      <Redirect to="/standup-meeting" />
    </Switch>
  );
};

const mapState = (state: State): AppProps => {
  return {
    isAuthenticated: Boolean(state.user.session),
  };
};

export default connect(mapState)(App);
