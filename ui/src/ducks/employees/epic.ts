import { AnyAction } from 'redux';
import { ofType, StateObservable } from 'redux-observable';
import { Observable } from 'rxjs';
import { mergeMap, withLatestFrom } from 'rxjs/operators';
import config from 'src/config';
import fetchWithAuth from 'src/lib/fetchWithAuth';
import { State } from 'src/reducer';
import duck, { Employee } from './index';

const actions = duck.actions;

const fetchEmployees = async (project: string, token: string): Promise<Employee[]> => {
  const data = await fetchWithAuth(token)(`${config.urls.core}/employees?project=${project}`);

  return (data as Employee[]).sort((a, b) => (a.name > b.name ? 1 : -1));
};

const fetchEmployeesEpic = (action$: Observable<AnyAction>, state$: StateObservable<State>) =>
  action$.pipe(
    ofType(actions.fetchStart),
    withLatestFrom(state$),
    mergeMap(async ([{ payload: project }, state]) => {
      const token = state.user.session && state.user.session.accessToken;

      try {
        const data = await fetchEmployees(project, token || '');

        if (!data) {
          throw new Error(data);
        }

        return actions.fetchSuccess(data);
      } catch (err) {
        return actions.fetchFail(`${err.message}`);
      }
    }),
  );

export default fetchEmployeesEpic;
