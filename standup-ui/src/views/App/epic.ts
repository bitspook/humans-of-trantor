import { Dayjs } from 'dayjs';
import { AnyAction } from 'redux';
import { combineEpics, Epic, ofType } from 'redux-observable';
import { Observable } from 'rxjs';
import { map, mergeMap } from 'rxjs/operators';
import { StandupFormValues } from 'src/components/StandupForm';
import standupDuck from 'src/ducks/standup';
import duck, { SaveStandupPayload } from './duck';

const saveStandup = (ecode: string, day: Dayjs, values: StandupFormValues) => {
  const url = 'http://localhost:7002/events/RECEIVED_STANDUP_UPDATE';

  const standups = [
    { ecode, date: day.format('YYYY-MM-DD'), type: 'delivered', standup: values.delivered },
    { ecode, date: day.format('YYYY-MM-DD'), type: 'committed', standup: values.committed },
    { ecode, date: day.format('YYYY-MM-DD'), type: 'impediment', standup: values.impediment },
  ];

  return Promise.all(
    standups.map(async (standup) => {
      const response = await fetch(url, {
        body: JSON.stringify({
          payload: {
            ...standup,
            project: 'Veriown',
          },
          version: 'v1',
        }),
        headers: {
          'content-type': 'application/json',
        },
        method: 'POST',
      }).then((res) => res.json());

      if (!response.id) {
        throw response;
      }

      return response;
    }),
  );
};

const { actions } = duck;

const saveStandupEpic = (action$: Observable<AnyAction>) =>
  action$.pipe(
    ofType(actions.startSaveStandup),
    mergeMap(async ({ payload }) => {
      const { ecode, standup, helpers, day } = payload as SaveStandupPayload;

      helpers.setSubmitting(true);

      try {
        await saveStandup(ecode, day, standup);

        helpers.setSubmitting(false);
        helpers.resetForm();

        return actions.fullfillSaveStandup();
      } catch (err) {
        helpers.setSubmitting(false);

        return actions.failSaveStandup([err]);
      }
    }),
  );

const fetchEmployeeStandupEpic: Epic = (action$) =>
  action$.pipe(
    ofType(actions.selectEmployee),
    map(({ payload }) => standupDuck.actions.fetchStandupStart(payload.ecode)),
  );

export default combineEpics(saveStandupEpic, fetchEmployeeStandupEpic);
