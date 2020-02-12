import { Epic, ofType } from 'redux-observable';
import { delay, mergeMap } from 'rxjs/operators';
import config from 'src/config';
import userDuck from 'src/ducks/user';

import duck, { LoginPayload, LoginValues } from './duck';

const { actions } = duck;

const login = async ({ email, password }: LoginValues) => {
  const res = await fetch(`${config.urls.core}/session`, {
    body: JSON.stringify({ email, password }),
    headers: {
      'content-type': 'application/json',
    },
    method: 'POST',
  });

  if (res.status === 401) {
    throw new Error('Invaid email or password');
  }

  if (res.status !== 200) {
    throw new Error('Login failed!');
  }

  return res.json();
};

const loginEpic: Epic = (action$) =>
  action$.pipe(
    ofType(actions.loginStart),
    // need to add some delay. Formik .setSubmitting don't work otherwise
    delay(10),
    mergeMap(async ({ payload }) => {
      const { values, helpers } = payload as LoginPayload;
      helpers.setSubmitting(true);

      try {
        const response = await login(values);

        return userDuck.actions.loginSuccess(response);
      } catch (err) {
        return actions.loginFail(err);
      } finally {
        helpers.setSubmitting(false);
      }
    }),
  );

export default loginEpic;
