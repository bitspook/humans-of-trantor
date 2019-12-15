import { stripIndent } from 'common-tags';
import { Dayjs } from 'dayjs';
import { AnyAction } from 'redux';
import { combineEpics, Epic, ofType, StateObservable } from 'redux-observable';
import { from, Observable } from 'rxjs';
import { delay, map, mergeMap, withLatestFrom } from 'rxjs/operators';
import { StandupFormValues } from 'src/components/StandupForm';
import standupDuck, { Standup } from 'src/ducks/standup';
import { State } from 'src/reducer';
import duck, { Report, SaveStandupPayload } from './duck';

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

const createReportEpic: Epic = (action$, state$: StateObservable<State>) =>
  action$.pipe(
    ofType(actions.showReport),
    withLatestFrom(state$),
    map(([_, state]) => {
      const selectedDay = state.app.selectedDay;

      const reports = state.employees.data
        .map((employee) => {
          const standup = state.standup.data
            .filter((s) => s.ecode === employee.ecode)
            .sort((s1, s2) => (s1.date.isBefore(s2.date) ? 1 : -1));

          const yesterday = standup.find(
            (s) => s.standupType === 'delivered' && s.date.isBefore(selectedDay, 'day'),
          );
          const today = standup.find(
            (s) => s.standupType === 'committed' && s.date.isSame(selectedDay, 'day'),
          );
          const impediment = standup.find(
            (s) => s.standupType === 'impediment' && s.date.isSame(selectedDay, 'day'),
          );

          if (!yesterday && !today && !impediment) {
            return;
          }

          return {
            employee,
            impediment,
            today,
            yesterday,
          };
        })
        .filter((r) => Boolean(r));

      return actions.createReport(reports);
    }),
  );

export default combineEpics(
  saveStandupEpic,
  fetchEmployeeStandupEpic,
  autoHideToastEpic,
  createReportEpic,
);
