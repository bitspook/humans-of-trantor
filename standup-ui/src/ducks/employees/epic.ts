import { AnyAction } from 'redux';
import { ofType } from 'redux-observable';
import { Observable } from 'rxjs';
import { mergeMap } from 'rxjs/operators';
import duck, { Employee } from './index';

const actions = duck.actions;

const fetchEmployees = async (project: string): Promise<Employee[]> => {
  return fetch(`http://localhost:7004/api/v1/employees?project=${project}`)
    .then((res) => res.json())
    .then((res) => (res.data as Employee[]).sort((a, b) => (a.name > b.name ? 1 : -1)));
};

const fetchEmployeesEpic = (action$: Observable<AnyAction>) =>
  action$.pipe(
    ofType(actions.fetchStart),
    mergeMap(async ({ payload: project }) => {
      try {
        const data = await fetchEmployees(project);

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
