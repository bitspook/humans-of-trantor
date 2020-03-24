import { AnyAction } from 'redux';
import { combineEpics, Epic, ofType, StateObservable } from 'redux-observable';
import { from, Observable } from 'rxjs';
import { delay, map, mergeMap, withLatestFrom } from 'rxjs/operators';
import config from 'src/config';
import standupDuck, { NewStandup } from 'src/ducks/standup';
import fetchWithAuth from 'src/lib/fetchWithAuth';
import { State } from 'src/reducer';
import duck, { SaveStandupPayload, CreateStandupPayload } from './duck';

interface SaveStandupApiPayload extends NewStandup {
  id?: string;
}

const saveStandup = (token: string) => async (standup: SaveStandupApiPayload) => {
  let url = `${config.urls.core}/standup`;
  let method = 'POST';

  if (standup.id) {
    url = `${url}/${standup.id}`;
    method = 'PUT';
  }

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
    method,
  });

  return response;
};

const { actions } = duck;

const createStandupEpic = (action$: Observable<AnyAction>, state$: StateObservable<State>) =>
  action$.pipe(
    ofType(actions.createStandupStart),
    withLatestFrom(state$),
    mergeMap(async ([{ payload }, state]) => {
      const { data, helpers } = payload as CreateStandupPayload;

      if (!state.standupMeeting.selectedEmployee) {
        return [actions.createStandupFail(new Error('No Employee Selected'))];
      }

      const standup: NewStandup = {
        ecode: state.standupMeeting.selectedEmployee,
        project: state.standupMeeting.selectedProject,
        standup: data.standup,
        isDelivered: false,
        priority: 0,
        date: state.standupMeeting.selectedDay,
      };
      const token = (state.user.session && state.user.session.accessToken) || '';

      try {
        helpers.setSubmitting(true);
        await saveStandup(token)(standup).finally(() => helpers.setSubmitting(false));

        helpers.resetForm();

        return [actions.createStandupSuccess(), standupDuck.actions.fetchStart(standup.ecode)];
      } catch (err) {
        const errors = Array.isArray(err) ? err : [err];

        helpers.setFieldError('standup', errors[0]);
        return [actions.createStandupFail(errors)];
      }
    }),
    mergeMap((arr) => from(arr)),
  );

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

        return [actions.saveStandupSuccess(), standupDuck.actions.fetchStart(standup.ecode)];
      } catch (err) {
        const errors = Array.isArray(err) ? err : [err];

        helpers.setFieldError('standup', errors[0]);
        return [actions.saveStandupFail()];
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

          const today = standup.filter((s) => s.date.isSame(selectedDay, 'day'));
          let yesterday = standup.filter((s) => s.date.isBefore(selectedDay, 'day'));

          if (!yesterday.length && !today.length) {
            return null;
          }

          const dayBeforeSelectedDay = yesterday.reduce((accum, s) => {
            return accum.date.isAfter(s.date, 'day') ? accum : s;
          }).date;

          yesterday = yesterday.filter((s) => s.date.isSame(dayBeforeSelectedDay));

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
  createStandupEpic,
  saveStandupEpic,
  fetchEmployeeStandupEpic,
  autoHideToastEpic,
  createReportEpic,
);
