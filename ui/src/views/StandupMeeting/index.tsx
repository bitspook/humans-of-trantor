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
import StandupForm, { StandupRowFormData } from 'src/components/StandupForm';
import Toaster, { ToastDataProps } from 'src/components/Toaster';
import { Employee } from 'src/ducks/employees';
import employeesD from 'src/ducks/employees';
import { Standup } from 'src/ducks/standup';
import { State } from 'src/reducer';
import duck, { ReportState, SaveStandupPayload } from './duck';
import c from './index.module.scss';

interface StandupMeetingDP {
  employees: Employee[];
  selectedDay: Dayjs;
  selectedEmployee?: string;
  selectedProject: string;
  saveStandupError?: Error;
  selectedStandups: Standup[];
  toasts: ToastDataProps[];
  report: ReportState;
}

interface StandupMeetingCP {
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

const App: React.FC<StandupMeetingDP & StandupMeetingCP> = (p) => {
  const handleSaveStandup = (
    data: StandupRowFormData,
    helpers: FormikHelpers<StandupRowFormData>,
  ) => {
    p.saveStandupStart({
      data,
      helpers,
    });
  };

  useEffect(() => {
    p.fetchEmployeesStart(p.selectedProject);
  }, [p.selectedProject]); // eslint-disable-line

  const maybeError = p.saveStandupError && (
    <Message error={true} header="Failed to save standup 😞" content={String(p.saveStandupError)} />
  );

  /* prettier-ignore */
  const standupFormCol = p.selectedEmployee ? (
    <StandupForm
      ecode={p.selectedEmployee}
      standups={p.selectedStandups}
      onSave={handleSaveStandup}
    />
  ) : (<SelectEmployeeInstruction />);

  const maybeCalendarCol = p.selectedEmployee && (
    <div className={classNames(c.calendar, { [c.empty]: !p.selectedEmployee })}>
      <StandupCalendar onSelect={p.selectDay} selectedDay={p.selectedDay} />
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

const mapState = (state: State): StandupMeetingDP => {
  const selectedStandups = state.standup.data.filter(
    (s) =>
      s.ecode === state.standupMeeting.selectedEmployee &&
      s.date.isSame(state.standupMeeting.selectedDay, 'day'),
  );

  return {
    ...state.standupMeeting,
    employees: state.employees.data,
    selectedStandups,
    toasts: Object.values(state.standupMeeting.toasts),
  };
};

const mapDispatch = (dispatch: Dispatch): StandupMeetingCP => ({
  ...bindActionCreators(duck.actions, dispatch),
  fetchEmployeesStart: bindActionCreators(employeesD.actions.fetchStart, dispatch),
});

export default connect(mapState, mapDispatch)(App);
