import dayjs from 'dayjs';
import { AnyAction } from 'redux';
import { ofType, StateObservable } from 'redux-observable';
import { Observable } from 'rxjs';
import { mergeMap, withLatestFrom } from 'rxjs/operators';
import config from 'src/config';
import fetchWithAuth from 'src/lib/fetchWithAuth';
import { State } from 'src/reducer';
import duck, { Standup } from './index';

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

export default fetchStandupEpic;
