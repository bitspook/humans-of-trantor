import { AnyAction } from 'redux';
import { Epic, ofType, combineEpics } from 'redux-observable';
import { Observable } from 'rxjs';
import { map, mergeMap } from 'rxjs/operators';
import { StandupFormValues } from 'src/components/StandupForm';
import duck, { SaveStandupPayload } from './duck';
import standupDuck from 'src/ducks/standup';

const saveStandup = (ecode: string, standup: StandupFormValues) => {
  const url = 'http://localhost:7002/events/RECEIVED_STANDUP_UPDATE';

  return Promise.all(
    Object.keys(standup).map((type) => {
      return fetch(url, {
        method: 'POST',
        headers: {
          'content-type': 'application/json',
        },
        body: JSON.stringify({
          version: 'v1',
          payload: {
            ecode,
            date: type === 'delivered' ? standup.delivered.date : standup.committed.date,
            type,
            standup: type === 'delivered' ? standup.delivered.standup : standup.committed.standup,
            project: 'Veriown',
          },
        }),
      })
        .then((res) => res.json())
        .then((res) => {
          if (!res.id) {
            throw res;
          }
        });
    }),
  );
};

const { actions } = duck;

const saveStandupEpic = (action$: Observable<AnyAction>) =>
  action$.pipe(
    ofType(actions.startSaveStandup),
    mergeMap(async ({ payload }) => {
      const { ecode, standup, helpers } = payload as SaveStandupPayload;

      helpers.setSubmitting(true);

      try {
        await saveStandup(ecode, standup);

        helpers.setSubmitting(false);
        helpers.resetForm();

        return actions.fullfillSaveStandup();
      } catch (err) {
        helpers.setSubmitting(false);

        return actions.failSaveStandup([err]);
      }
    }),
  );

const fetchEmployeeStandupEpic: Epic = (action$) => action$.pipe(
  ofType(actions.selectEmployee),
  map(({ payload }) => standupDuck.actions.fetchStandupStart(payload.ecode)),
);

export default combineEpics(saveStandupEpic, fetchEmployeeStandupEpic);
