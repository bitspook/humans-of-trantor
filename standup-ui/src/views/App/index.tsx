import { Dayjs } from 'dayjs';
import React, { useEffect } from 'react';
import { connect } from 'react-redux';
import { Message } from 'semantic-ui-react';

import { FormikHelpers } from 'formik';
import { bindActionCreators, Dispatch } from 'redux';
import EmployeesList from 'src/components/EmployeesList';
import StandupCalendar from 'src/components/StandupCalendar';
import StandupForm, { StandupFormValues } from 'src/components/StandupForm';
import { Employee } from 'src/ducks/employees';
import employeesD from 'src/ducks/employees';
import { Standup } from 'src/ducks/standup';
import { State } from 'src/reducer';
import duck, { SaveStandupPayload } from './duck';
import c from './index.module.scss';

interface AppDataProps {
  employees: Employee[];
  selectedDay: Dayjs;
  selectedEmployee: string;
  saveStandupError: string;
  standup: Standup[];
  initialStandupFormValue: StandupFormValues;
}

interface AppCbProps {
  fetchEmployeesStart: (project: string) => void;
  selectDay: (d: Dayjs) => void;
  selectEmployee: (e: Employee) => void;
  startSaveStandup: (payload: SaveStandupPayload) => void;
}

const App: React.FC<AppDataProps & AppCbProps> = (p) => {
  const handleSaveStandup = (ecode: string) => (
    standup: StandupFormValues,
    helpers: FormikHelpers<StandupFormValues>,
  ) => {
    p.startSaveStandup({ ecode, standup, helpers, day: p.selectedDay });
  };

  useEffect(() => {
    p.fetchEmployeesStart('Veriown');
  }, [1]);

  const maybeError = p.saveStandupError && (
    <Message error={true} header='Failed to save standup 😞' content={p.saveStandupError} />
  );

  return (
    <div className={c.root}>
      <div className={c.container}>
        <div className={c.employeesListSidebar}>
          <EmployeesList
            selectedEmployee={p.selectedEmployee}
            employees={p.employees}
            onSelect={p.selectEmployee}
          />
        </div>

        <div className={c.calendar}>
          <StandupCalendar
            standup={p.standup}
            onSelect={p.selectDay}
            selectedDay={p.selectedDay}
          />
        </div>

        <div className={c.standupForm}>
          <StandupForm
            initialValues={p.initialStandupFormValue}
            onSave={handleSaveStandup(p.selectedEmployee)}
          />

          {maybeError}
        </div>
      </div>
    </div>
  );
};

const mapState = (state: State): AppDataProps => {
  const activeStandup = state.standup.data
    .filter((s) => s.ecode === state.app.selectedEmployee && s.date.isSame(state.app.selectedDay, 'day'));
  const initialStandupFormValue = {
    committed: (activeStandup.find((s) => s.standupType === 'committed') || { standup: '' }).standup,
    delivered: (activeStandup.find((s) => s.standupType === 'delivered') || { standup: '' }).standup,
    impediment: (activeStandup.find((s) => s.standupType === 'impediment') || { standup: '' }).standup,
  };

  return {
    ...state.app,
    employees: state.employees.data,
    initialStandupFormValue,
    standup: state.standup.data.filter((s) => s.ecode === state.app.selectedEmployee),
  };
};

const mapDispatch = (dispatch: Dispatch): AppCbProps => ({
  ...bindActionCreators(duck.actions, dispatch),
  fetchEmployeesStart: bindActionCreators(employeesD.actions.fetchEmployeesStart, dispatch),
});

export default connect(mapState, mapDispatch)(App);
