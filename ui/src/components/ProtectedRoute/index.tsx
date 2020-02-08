import React from 'react';
import { RouteComponentProps } from 'react-router';
import { Redirect, Route, RouteProps } from 'react-router-dom';

interface ProtectedRouteProps extends RouteProps {
  isAuthenticated: boolean;
}

const ProtectedRoute: React.FC<ProtectedRouteProps> = ({ children, isAuthenticated, ...rest }) => {
  const redirectPath = ({ location }: RouteComponentProps) => ({
    pathname: '/login',
    state: { from: location },
  });

  const render = (props: RouteComponentProps) =>
    isAuthenticated ? children : <Redirect to={redirectPath(props)} />;

  return <Route {...rest} render={render} />;
};

export default ProtectedRoute;
