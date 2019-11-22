import { RouterRootState } from 'connected-react-router';
import { Employee } from './ducks/employees';
import employees from './ducks/employees';
import app, { AppState } from './views/App/duck';

export interface State {
  employees: Employee[];
  app: AppState;
  router: RouterRootState;
}

const reducers = {
  app: app.reducer,
  employees: employees.reducer
};

export default reducers;
