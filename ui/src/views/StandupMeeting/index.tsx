import classNames from 'classnames';
import { Dayjs } from 'dayjs';
import React, { useEffect } from 'react';
import { connect } from 'react-redux';
import { Header, Icon, Message } from 'semantic-ui-react';

import { FormikHelpers } from 'formik';
import { bindActionCreators, Dispatch } from 'redux';
import EmployeesList from 'src/components/EmployeesList';
import Report from 'src/components/Report';
import StandupCalendar from 'src/components/StandupCalendar';
import StandupForm, { StandupFormValues } from 'src/components/StandupForm';
import Toaster, { ToastDataProps } from 'src/components/Toaster';
import { Employee } from 'src/ducks/employees';
import employeesD from 'src/ducks/employees';
import { Standup } from 'src/ducks/standup';
import { State } from 'src/reducer';
import duck, { ReportState, SaveStandupPayload } from './duck';
import c from './index.module.scss';

interface AppDataProps {
  employees: Employee[];
  selectedDay: Dayjs;
  selectedEmployee?: string;
  selectedProject: string;
  saveStandupError?: Error;
  standup: Standup[];
  initialStandupFormValue: StandupFormValues;
  toasts: ToastDataProps[];
  report: ReportState;
}

interface AppCbProps {
  fetchEmployeesStart: (project: string) => void;
  selectDay: (d: Dayjs) => void;
  selectEmployee: (e: Employee) => void;
  saveStandupStart: (payload: SaveStandupPayload) => void;
  showReport: () => void;
  hideReport: () => void;
}

const SelectEmployeeInstruction = () => (
  <Header as="h2" className={c.instruction} disabled={true}>
    <Icon name="user" circular={true} />
    <Header.Content>
      Please select an employee
      <Header.Subheader>From the left-most column</Header.Subheader>
    </Header.Content>
  </Header>
);

const App: React.FC<AppDataProps & AppCbProps> = (p) => {
  const handleSaveStandup = (ecode: string) => (
    standup: StandupFormValues,
    helpers: FormikHelpers<StandupFormValues>,
  ) => {
    p.saveStandupStart({ ecode, standup, project: p.selectedProject, day: p.selectedDay, helpers });
  };

  useEffect(() => {
    p.fetchEmployeesStart(p.selectedProject);
  }, [p.selectedProject]); // eslint-disable-line

  const maybeError = p.saveStandupError && (
    <Message error={true} header="Failed to save standup 😞" content={p.saveStandupError} />
  );

  /* prettier-ignore */
  const standupFormCol = p.selectedEmployee ? (
    <StandupForm
      initialValues={p.initialStandupFormValue}
      onSave={handleSaveStandup(p.selectedEmployee)}
    />
  ) : (<SelectEmployeeInstruction />);

  const maybeCalendarCol = p.selectedEmployee && (
    <div className={classNames(c.calendar, { [c.empty]: !p.selectedEmployee })}>
      <StandupCalendar standup={p.standup} onSelect={p.selectDay} selectedDay={p.selectedDay} />
    </div>
  );

  return (
    <div className={c.root}>
      <div className={c.reportBar}>
        <Report
          day={p.selectedDay}
          onOpen={p.showReport}
          onClose={p.hideReport}
          isOpen={p.report.isVisible}
          reports={p.report.data}
        />
      </div>

      <div className={c.container}>
        <div className={c.employeesListSidebar}>
          <EmployeesList
            selectedEmployee={p.selectedEmployee}
            employees={p.employees}
            onSelect={p.selectEmployee}
          />
        </div>

        {maybeCalendarCol}

        <div className={classNames(c.standupForm, { [c.empty]: !p.selectedEmployee })}>
          {standupFormCol}
          {maybeError}
        </div>
      </div>

      <div className={c.toaster}>
        <Toaster toasts={p.toasts} />
      </div>
    </div>
  );
};

const mapState = (state: State): AppDataProps => {
  const activeStandup = state.standup.data.filter(
    (s) =>
      s.ecode === state.standupMeeting.selectedEmployee &&
      s.date.isSame(state.standupMeeting.selectedDay, 'day'),
  );
  const initialStandupFormValue = {
    committed: (activeStandup.find((s) => s.standupType === 'committed') || { standup: '' })
      .standup,
    delivered: (activeStandup.find((s) => s.standupType === 'delivered') || { standup: '' })
      .standup,
    impediment: (activeStandup.find((s) => s.standupType === 'impediment') || { standup: '' })
      .standup,
  };

  return {
    ...state.standupMeeting,
    employees: state.employees.data,
    initialStandupFormValue,
    standup: state.standup.data.filter((s) => s.ecode === state.standupMeeting.selectedEmployee),
    toasts: Object.values(state.standupMeeting.toasts),
  };
};

const mapDispatch = (dispatch: Dispatch): AppCbProps => ({
  ...bindActionCreators(duck.actions, dispatch),
  fetchEmployeesStart: bindActionCreators(employeesD.actions.fetchStart, dispatch),
});

export default connect(mapState, mapDispatch)(App);
