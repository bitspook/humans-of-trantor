import React from 'react';
import { RouteComponentProps } from 'react-router';
import { Redirect, Route, RouteProps } from 'react-router-dom';

interface ProtectedRouteProps extends RouteProps {
  isAuthenticated: boolean;
  redirectTo?: string;
}

const ProtectedRoute: React.FC<ProtectedRouteProps> = ({
  children,
  isAuthenticated,
  redirectTo,
  render,
  ...rest
}) => {
  const redirectPath = ({ location }: RouteComponentProps) => ({
    pathname: redirectTo || '/login',
    state: { from: location },
  });

  const protectedRender = (props: RouteComponentProps) =>
    isAuthenticated ? children : <Redirect to={redirectPath(props)} />;

  return <Route {...rest} render={protectedRender} />;
};

export default ProtectedRoute;
