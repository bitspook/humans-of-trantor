import { combineEpics } from 'redux-observable';
import employeesEpic from 'src/ducks/employees/epic';
import appEpic from 'src/views/App/epic';

export default combineEpics(appEpic, employeesEpic);
