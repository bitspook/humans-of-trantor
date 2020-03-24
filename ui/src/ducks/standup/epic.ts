import dayjs from 'dayjs';
import { AnyAction } from 'redux';
import { combineEpics, ofType, StateObservable } from 'redux-observable';
import { from, Observable } from 'rxjs';
import { mergeMap, withLatestFrom } from 'rxjs/operators';
import config from 'src/config';
import fetchWithAuth from 'src/lib/fetchWithAuth';
import { State } from 'src/reducer';
import duck, { DeleteStandupPayload, Standup } from './index';

const actions = duck.actions;

const fetchStandup = async (ecode: string, token: string): Promise<Standup[]> => {
  const after = dayjs()
    .subtract(30, 'day')
    .format('YYYY-MM-DD');
  const data = await fetchWithAuth(token)(
    `${config.urls.core}/standup?ecode=${ecode}&after=${after}`,
  ).then((r) => r.json());

  return (data as Standup[]).map((s) => ({ ...s, date: dayjs(s.date) }));
};

const deleteStandup = (token: string) => async (standup: Standup) => {
  const response = await fetchWithAuth(token)(`${config.urls.core}/standup/${standup.id}`, {
    headers: {
      'content-type': 'application/json',
    },
    method: 'DELETE',
  });

  return response;
};

const fetchStandupEpic = (action$: Observable<AnyAction>, state$: StateObservable<State>) =>
  action$.pipe(
    ofType(actions.fetchStart),
    withLatestFrom(state$),
    mergeMap(async ([{ payload: ecode }, state]) => {
      const token = state.user.session && state.user.session.accessToken;

      try {
        const data = await fetchStandup(ecode, token || '');

        if (!data) {
          throw new Error(data);
        }

        return actions.fetchSuccess(data);
      } catch (err) {
        return actions.fetchFail([err]);
      }
    }),
  );

const deleteStandupEpic = (action$: Observable<AnyAction>, state$: StateObservable<State>) =>
  action$.pipe(
    ofType(duck.actions.deleteStandupStart),
    withLatestFrom(state$),
    mergeMap(async ([{ payload }, state]) => {
      const { standup, helpers } = payload as DeleteStandupPayload;
      const token = (state.user.session && state.user.session.accessToken) || '';

      try {
        helpers.setSubmitting(true);
        await deleteStandup(token)(standup).finally(() => helpers.setSubmitting(false));

        return [duck.actions.deleteStandupSuccess(standup), duck.actions.fetchStart(standup.ecode)];
      } catch (err) {
        const errors = Array.isArray(err) ? err : [err];

        helpers.setFieldError('standup', errors[0]);
        return [duck.actions.deleteStandupFail()];
      }
    }),
    mergeMap((arr) => from(arr)),
  );

export default combineEpics(fetchStandupEpic, deleteStandupEpic);
