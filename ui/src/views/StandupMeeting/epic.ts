import { AnyAction } from 'redux';
import { combineEpics, Epic, ofType, StateObservable } from 'redux-observable';
import { from, Observable } from 'rxjs';
import { delay, map, mergeMap, withLatestFrom } from 'rxjs/operators';
import config from 'src/config';
import standupDuck, { Standup } from 'src/ducks/standup';
import fetchWithAuth from 'src/lib/fetchWithAuth';
import { State } from 'src/reducer';
import duck, { SaveStandupPayload } from './duck';

const saveStandup = (token: string) => async (standup: Standup) => {
  const url = `${config.urls.core}/standup/${standup.id}`;

  const fetch = fetchWithAuth(token);
  const payload = {
    ...standup,
    date: standup.date.format('YYYY-MM-DD'),
  };

  const response = await fetch(url, {
    body: JSON.stringify(payload),
    headers: {
      'content-type': 'application/json',
    },
    method: 'PUT',
  });

  return response;
};

const { actions } = duck;

const saveStandupEpic = (action$: Observable<AnyAction>, state$: StateObservable<State>) =>
  action$.pipe(
    ofType(actions.saveStandupStart),
    withLatestFrom(state$),
    mergeMap(async ([{ payload }, state]) => {
      const { data, helpers } = payload as SaveStandupPayload;
      const standup = data.standup;
      const token = (state.user.session && state.user.session.accessToken) || '';

      try {
        helpers.setSubmitting(true);
        await saveStandup(token)(standup).finally(() => helpers.setSubmitting(false));
        const toasts: AnyAction[] = [
          actions.showToast({
            body: `Standup saved`,
            header: 'Standup saved successfully!',
            key: `${new Date().getTime()}`,
          }),
        ];

        return toasts.concat([
          actions.saveStandupSuccess(),
          standupDuck.actions.fetchStart(standup.ecode),
        ]);
      } catch (err) {
        const errors = Array.isArray(err) ? err : [err];

        const toasts: AnyAction[] = errors.map((e) =>
          actions.showToast({
            body: `${e.name}`,
            header: 'Failed to save standup',
            key: new Date().getTime(),
          }),
        );

        helpers.setFieldError('standup', errors[0]);
        return toasts.concat([actions.saveStandupFail(errors)]);
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
      const selectedDay = state.standupMeeting.selectedDay;

      const reports = state.employees.data
        .map((employee) => {
          const standup = state.standup.data
            .filter((s) => s.ecode === employee.ecode)
            .sort((s1, s2) => (s1.date.isBefore(s2.date) ? 1 : -1));

          const yesterday = standup.find((s) => s.date.isBefore(selectedDay, 'day'));
          const today = standup.find((s) => s.date.isSame(selectedDay, 'day'));

          if (!yesterday && !today) {
            return null;
          }

          return {
            employee,
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
