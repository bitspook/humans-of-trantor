import dayjs from 'dayjs';
import { AnyAction } from 'redux';
import { ofType } from 'redux-observable';
import { Observable } from 'rxjs';
import { mergeMap } from 'rxjs/operators';
import config from 'src/config';
import duck, { Standup } from './index';

const actions = duck.actions;

const fetchStandup = async (ecode: string): Promise<Standup[]> => {
  return fetch(`${config.urls.pms}/api/v1/standup?ecode=${ecode}`)
    .then((res) => res.json())
    .then((res) => (res.data as Standup[]).map((s) => ({ ...s, date: dayjs(s.date) })));
};

const fetchStandupEpic = (action$: Observable<AnyAction>) =>
  action$.pipe(
    ofType(actions.fetchStart),
    mergeMap(async ({ payload: ecode }) => {
      try {
        const data = await fetchStandup(ecode);

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
