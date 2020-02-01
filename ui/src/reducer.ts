import { RouterRootState } from 'connected-react-router';
import employees, { EmployeesState } from './ducks/employees';
import standup, { StandupState } from './ducks/standup';
import standupMeeting, { StandupMeetingState } from './views/StandupMeeting/duck';

export interface State {
  employees: EmployeesState;
  standupMeeting: StandupMeetingState;
  router: RouterRootState;
  standup: StandupState;
}

const reducers = {
  standupMeeting: standupMeeting.reducer,
  employees: employees.reducer,
  standup: standup.reducer,
};

export default reducers;
