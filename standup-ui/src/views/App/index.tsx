import React from 'react';
import { connect } from 'react-redux';
import EmployeesList from 'src/components/EmployeesList';
import StandupForm from 'src/components/StandupForm';
import { Employee } from 'src/ducks/employees';
import { State } from '../../reducer';
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
          <StandupForm onSave={v => console.log(v)} />
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
