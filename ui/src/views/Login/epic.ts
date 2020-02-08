import { Epic, ofType } from 'redux-observable';
import { delay, map, mergeMap } from 'rxjs/operators';
import duck, { LoginPayload } from './duck';

const { actions } = duck;

const loginEpic: Epic = (action$) => action$.pipe(
  ofType(actions.loginStart),
  delay(400),
  map(({ payload }) => {
    const { values, helpers } = payload as LoginPayload;

    helpers.setSubmitting(true);

    return actions.loginFail(new Error('Get out of here! Lol'));
  }),
  delay(4000)
);

export default loginEpic;
