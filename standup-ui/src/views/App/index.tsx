import dayjs from 'dayjs';
import React, { useEffect } from 'react';
import { connect } from 'react-redux';
import { Message } from 'semantic-ui-react';

import { FormikHelpers } from 'formik';
import { bindActionCreators, Dispatch } from 'redux';
import EmployeesList from 'src/components/EmployeesList';
import StandupCalendarList from 'src/components/StandupCalendarList';
import StandupForm, { StandupFormValues } from 'src/components/StandupForm';
import { Employee } from 'src/ducks/employees';
import employeesD from 'src/ducks/employees';
import { State } from 'src/reducer';
import duck, { SaveStandupPayload } from './duck';
import c from './index.module.scss';

interface AppDataProps {
  employees: Employee[];
  selectedEmployee: string;
  saveStandupError: string;
}

interface AppCbProps {
  fetchEmployeesStart: (project: string) => void;
  selectEmployee: (e: Employee) => void;
  startSaveStandup: (payload: SaveStandupPayload) => void;
}

const App: React.FC<AppDataProps & AppCbProps> = (p) => {
  const handleSaveStandup = (ecode: string) => (
    standup: StandupFormValues,
    helpers: FormikHelpers<StandupFormValues>,
  ) => {
    p.startSaveStandup({ ecode, standup, helpers });
  };

  useEffect(() => {
    p.fetchEmployeesStart('Veriown');
  }, [1]);

  const maybeError = p.saveStandupError && (
    <Message error={true} header='Failed to save standup 😞' content={p.saveStandupError} />
  );

  const calendarDays = () => {
    const today = dayjs(new Date());

    const calDays = [];

    for (let i = 0; i < 31; i += 1) {
      calDays.push(today.subtract(i, 'day'));
    }

    return calDays;
  };

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
          <StandupCalendarList
            days={calendarDays()}
            onSelect={(day) => console.warn('SELECTED', day)}
            selectedDay={dayjs(new Date())}
          />
        </div>

        <div className={c.standupForm}>
          <StandupForm onSave={handleSaveStandup(p.selectedEmployee)} />

          {maybeError}
        </div>
      </div>
    </div>
  );
};

const mapState = (state: State): AppDataProps => ({
  ...state.app,
  employees: state.employees.data,
});

const mapDispatch = (dispatch: Dispatch): AppCbProps => ({
  ...bindActionCreators(duck.actions, dispatch),
  fetchEmployeesStart: bindActionCreators(employeesD.actions.fetchEmployeesStart, dispatch),
});

export default connect(mapState, mapDispatch)(App);
