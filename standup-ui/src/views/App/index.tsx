import React from 'react';
import { connect } from 'react-redux';
import EmployeesList from 'src/components/EmployeesList';
import StandupForm from 'src/components/StandupForm';
import { Employee } from 'src/ducks/employees';
import { State } from '../../reducer';
import c from './index.module.scss';

interface AppDataProps {
  employees: Employee[];
}

interface AppCbProps { }

const App: React.FC<AppDataProps & AppCbProps> = ({ employees }) => {
  return (
    <div className={c.root}>
      <div className={c.container}>
        <div className={c.employeesListSidebar}>
          <EmployeesList employees={employees} />
        </div>

        <div className={c.standupForm}>
          <StandupForm onSave={v => console.log(v)} />
        </div>
      </div>
    </div>
  );
};

const mapState = (state: State): AppDataProps => ({
  employees: state.employees
});

const mapDispatch = (): AppCbProps => ({});

export default connect(mapState, mapDispatch)(App);
