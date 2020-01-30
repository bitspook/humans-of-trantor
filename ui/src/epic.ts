import { combineEpics } from 'redux-observable';
import employeesEpic from 'src/ducks/employees/epic';
import standupEpic from 'src/ducks/standup/epic';
import standupMeetingEpic from 'src/views/StandupMeeting/epic';

export default combineEpics(standupMeetingEpic, employeesEpic, standupEpic);
