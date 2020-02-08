import { RouterRootState } from 'connected-react-router';
import employees, { EmployeesState } from './ducks/employees';
import login, { LoginState } from './views/Login/duck';
import standupMeeting, { StandupMeetingState } from './views/StandupMeeting/duck';

import standup, { StandupState } from './ducks/standup';

export interface State {
  employees: EmployeesState;
  standupMeeting: StandupMeetingState;
  router: RouterRootState;
  standup: StandupState;
  login: LoginState;
}

const reducers = {
  employees: employees.reducer,
  login: login.reducer,
  standup: standup.reducer,
  standupMeeting: standupMeeting.reducer,
};

export default reducers;
