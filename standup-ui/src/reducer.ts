import { RouterRootState } from 'connected-react-router';
import employees, { EmployeesState } from './ducks/employees';
import standup, { StandupState } from './ducks/standup';
import app, { AppState } from './views/App/duck';

export interface State {
  employees: EmployeesState;
  app: AppState;
  router: RouterRootState;
  standup: StandupState;
}

const reducers = {
  app: app.reducer,
  employees: employees.reducer,
  standup: standup.reducer,
};

export default reducers;
