import { RouterRootState } from 'connected-react-router';
import employees, { EmployeesState } from './ducks/employees';
import app, { AppState } from './views/App/duck';

export interface State {
  employees: EmployeesState;
  app: AppState;
  router: RouterRootState;
}

const reducers = {
  app: app.reducer,
  employees: employees.reducer,
};

export default reducers;
