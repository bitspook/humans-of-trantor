import React from 'react';
import { connect } from 'react-redux';
import EmployeesList from 'src/components/EmployeesList';
import { Employee } from 'src/ducks/employees';
import { State } from '../../reducer';
import './index.css';

interface AppDataProps {
  employees: Employee[];
}

interface AppCbProps {
}

const App: React.FC<AppDataProps & AppCbProps> = ({
  employees
}) => {
  return (
    <div className='container'>
      <div className='employees-list-sidebar'>
        <EmployeesList employees={employees} />
      </div>

      <div className='employee-details'>
      </div>
      <div className='employee-timeline' />
    </div>
  );
};

const mapState = (state: State): AppDataProps => ({
  employees: state.employees,
});

const mapDispatch = (): AppCbProps => ({});

export default connect(
  mapState,
  mapDispatch,
)(App);
