import React from 'react';
import { Image, List } from 'semantic-ui-react';
import { Employee } from '../../ducks/employees';
import './index.module.scss';

const EmployeeListItem = (c: Employee) => {
  const avatarImage = c.avatar || '/images/default-employee.png';

  return (
    <List.Item key={c.eCode}>
      <Image avatar={true} src={avatarImage} />
      <List.Content>
        <List.Header>
          {c.name}
        </List.Header>
        <List.Description>
          +{c.eCode}
        </List.Description>
      </List.Content>
    </List.Item>
  );
};

interface IEmployeesListDataProps {
  employees: Employee[];
}

interface IEmployeesListCbProps {
}

const EmployeesList: React.FC<IEmployeesListDataProps & IEmployeesListCbProps> = ({
  employees,
}) => {
  return (
    <List selection={true} relaxed={true} divided={true} verticalAlign='middle'>
      {employees.map(EmployeeListItem)}
    </List>
  );
};

export default EmployeesList;
