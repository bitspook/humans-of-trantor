import { ofType } from 'redux-observable';
import { Observable } from 'rxjs';
import { delay, mergeMap } from 'rxjs/operators';
import { AnyAction } from 'redux';
import duck, { SaveStandupPayload } from './duck';
import { StandupFormValues } from 'src/components/StandupForm';

const saveStandup = (ecode: string, standup: StandupFormValues) => {
  const url = 'http://localhost:7002/events/RECEIVED_STANDUP_UPDATE';

  return Promise.all(
    Object.keys(standup).map(type => {
      return fetch(url, {
        method: 'POST',
        headers: {
          'content-type': 'application/json'
        },
        body: JSON.stringify({
          version: 'v1',
          payload: {
            ecode,
            date:
              type === 'delivered'
                ? standup.delivered.date
                : standup.committed.date,
            type,
            standup:
              type === 'delivered'
                ? standup.delivered.standup
                : standup.committed.standup,
            project: 'Veriown'
          }
        })
      })
        .then(res => res.json())
        .then(res => {
          if (res.status !== 200) {
            throw res;
          }
        });
    })
  );
};

const saveStandupEpic = (action$: Observable<AnyAction>) =>
  action$.pipe(
    ofType(duck.actions.startSaveStandup),
    delay(3000),
    mergeMap(async ({ payload }) => {
      const { ecode, standup, helpers } = payload as SaveStandupPayload;

      helpers.setSubmitting(true);

      try {
        await saveStandup(ecode, standup);

        helpers.setSubmitting(false);
        helpers.resetForm();

        return duck.actions.fullfillSaveStandup();
      } catch (err) {
        console.error('Error while saving standup', err);
        helpers.setSubmitting(false);

        return duck.actions.failSaveStandup(
          `${err.message}`
        );
      }
    })
  );

export default saveStandupEpic;
