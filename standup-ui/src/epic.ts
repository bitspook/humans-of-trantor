import { combineEpics } from 'redux-observable';
import appEpic from 'src/views/App/epic';

export default combineEpics(appEpic);
