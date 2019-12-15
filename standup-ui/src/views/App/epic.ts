import { stripIndent } from 'common-tags';
import dayjs, { Dayjs } from 'dayjs';
import { AnyAction } from 'redux';
import { combineEpics, Epic, ofType, StateObservable } from 'redux-observable';
import { from, Observable } from 'rxjs';
import { delay, map, mergeMap, withLatestFrom } from 'rxjs/operators';
import { StandupFormValues } from 'src/components/StandupForm';
import { Employee } from 'src/ducks/employees';
import standupDuck, { Standup } from 'src/ducks/standup';
import { State } from 'src/reducer';
import duck, { SaveStandupPayload } from './duck';

const saveStandup = (ecode: string, day: Dayjs, project: string, values: StandupFormValues) => {
  const url = 'http://localhost:7002/events/RECEIVED_STANDUP_UPDATE';

  const standups = [
    { ecode, date: day.format('YYYY-MM-DD'), type: 'delivered', standup: values.delivered },
    { ecode, date: day.format('YYYY-MM-DD'), type: 'committed', standup: values.committed },
    { ecode, date: day.format('YYYY-MM-DD'), type: 'impediment', standup: values.impediment },
  ].filter((s) => Boolean(s.standup));

  return Promise.all(
    standups.map(async (standup) => {
      const response = await fetch(url, {
        body: JSON.stringify({
          payload: {
            ...standup,
            project,
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
    ofType(actions.saveStandupStart),
    mergeMap(async ({ payload }) => {
      const { ecode, standup, project, helpers, day } = payload as SaveStandupPayload;

      helpers.setSubmitting(true);

      try {
        const responses = await saveStandup(ecode, day, project, standup);
        const toasts: AnyAction[] = responses.map((r) =>
          actions.showToast({
            body: `${r.payload.type} for ${r.payload.ecode}`,
            header: 'Standup saved successfully!',
            key: `${r.payload.type}-${r.payload.ecode}`,
          }),
        );

        return toasts.concat([actions.saveStandupSuccess(), standupDuck.actions.fetchStart(ecode)]);
      } catch (err) {
        const errors = Array.isArray(err) ? err : [err];

        const toasts: AnyAction[] = errors.map((e) =>
          actions.showToast({
            body: `${e.name}`,
            header: 'Failed to save standup',
            key: new Date().getTime(),
          }),
        );

        return toasts.concat([actions.saveStandupFail(errors)]);
      } finally {
        helpers.setSubmitting(false);
      }
    }),
    mergeMap((arr) => from(arr)),
  );

const fetchEmployeeStandupEpic: Epic = (action$) =>
  action$.pipe(
    ofType(actions.selectEmployee),
    map(({ payload }) => standupDuck.actions.fetchStart(payload.ecode)),
  );

const autoHideToastEpic: Epic = (action$) =>
  action$.pipe(
    ofType(actions.showToast),
    delay(4000),
    map(({ payload }) => actions.hideToast(payload)),
  );

const showReportEpic: Epic = (action$, state$: StateObservable<State>) =>
  action$.pipe(
    ofType(actions.createReport),
    withLatestFrom(state$),
    map(([_, state]) => {
      const today = dayjs();
      const standup = state.standup.data
        .filter((s) => s.date.isSame(today, 'day'))
        .reduce((accum, s) => {
          accum[s.ecode] = accum[s.ecode] || {};
          accum[s.ecode].delivered = accum[s.ecode].delivered || '';
          accum[s.ecode].committed = accum[s.ecode].committed || '';
          accum[s.ecode].impediment = accum[s.ecode].impediment || '';

          accum[s.ecode][s.standupType] = s.standup;

          return accum;
        }, {} as { [ecode: string]: { [status: string]: string } });

      const reports = state.employees.data
        .map((emp) => {
          const empStandup = standup[emp.ecode];

          if (!empStandup) {
            return '';
          }

          return stripIndent`
        ${emp.name}:
          Today:
            ${standup[emp.ecode].delivered.split('\n') || '-'}

          Impediments:
            ${standup[emp.ecode].impediment.split('\n') || '-'}
      `;
        })
        .filter((r) => Boolean(r));

      return actions.showReport(reports.join('---\n\n'));
    }),
  );

export default combineEpics(
  saveStandupEpic,
  fetchEmployeeStandupEpic,
  autoHideToastEpic,
  showReportEpic,
);
