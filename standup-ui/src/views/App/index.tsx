import React from 'react';
import { connect } from 'react-redux';
import { Message } from 'semantic-ui-react';

import EmployeesList from 'src/components/EmployeesList';
import StandupForm, { StandupFormValues } from 'src/components/StandupForm';
import { Employee } from 'src/ducks/employees';
import { State } from 'src/reducer';

import c from './index.module.scss';
import { Dispatch, bindActionCreators } from 'redux';
import duck, { SaveStandupPayload } from './duck';
import { FormikHelpers } from 'formik';

interface AppDataProps {
  employees: Employee[];
  selectedEmployee: string;
  saveStandupError: string;
}

interface AppCbProps {
  selectEmployee: (e: Employee) => void;
  startSaveStandup: (payload: SaveStandupPayload) => void;
}

const App: React.FC<AppDataProps & AppCbProps> = p => {
  const handleSaveStandup = (ecode: string) => (
    standup: StandupFormValues,
    helpers: FormikHelpers<StandupFormValues>
  ) => {
    p.startSaveStandup({ ecode, standup, helpers });
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

        <div className={c.standupForm}>
          <StandupForm onSave={handleSaveStandup(p.selectedEmployee)} />

          {p.saveStandupError && (
            <Message
              error
              header="Failed to save standup 😞"
              content={p.saveStandupError}
            />
          )}
        </div>
      </div>
    </div>
  );
};

const mapState = (state: State): AppDataProps => ({
  ...state.app,
  employees: state.employees
});

const mapDispatch = (dispatch: Dispatch): AppCbProps =>
  bindActionCreators(duck.actions, dispatch);

export default connect(mapState, mapDispatch)(App);
