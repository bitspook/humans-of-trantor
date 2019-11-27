import React from 'react';
import { connect } from 'react-redux';

import EmployeesList from 'src/components/EmployeesList';
import StandupForm, { StandupFormValues } from 'src/components/StandupForm';
import { Employee } from 'src/ducks/employees';
import { State } from 'src/reducer';

import c from './index.module.scss';
import { Dispatch, bindActionCreators } from 'redux';
import duck from './duck';

interface AppDataProps {
  employees: Employee[];
  selectedEmployee: string;
}

interface AppCbProps {
  selectEmployee: (e: Employee) => void;
}

const handleSaveStandup = (employee?: Employee) => (standup: StandupFormValues) => {
  if (!employee) {
    throw new Error('Employee is needed to save standup');
  }

  const url = 'http://localhost:7002/events/RECEIVED_STANDUP_UPDATE';

  return Promise.all(Object.keys(standup).map((type) => {
    return fetch(url, {
      method: 'POST',
      headers: {
        'content-type': 'application/json'
      },
      body: JSON.stringify({
        version: 'v1',
        payload: {
          ecode: employee.ecode,
          date: type === 'delivered' ? standup.delivered.date : standup.committed.date,
          type,
          standup: type === 'delivered' ? standup.delivered.standup : standup.committed.standup,
          project: 'Veriown'
        }
      })
    });
  }));
}

const App: React.FC<AppDataProps & AppCbProps> = ({
  employees,
  selectEmployee,
  selectedEmployee
}) => {
  return (
    <div className={c.root}>
      <div className={c.container}>
        <div className={c.employeesListSidebar}>
          <EmployeesList
            selectedEmployee={selectedEmployee}
            employees={employees}
            onSelect={selectEmployee}
          />
        </div>

        <div className={c.standupForm}>
          <StandupForm onSave={handleSaveStandup(employees.find((e) => e.ecode === selectedEmployee))} />
        </div>
      </div>
    </div>
  );
};

const mapState = (state: State): AppDataProps => ({
  selectedEmployee: state.app.selectedEmployee,
  employees: state.employees
});

const mapDispatch = (dispatch: Dispatch): AppCbProps =>
  bindActionCreators(duck.actions, dispatch);

export default connect(mapState, mapDispatch)(App);
