import React from 'react';
import { Image, List } from 'semantic-ui-react';
import { Employee } from '../../ducks/employees';
import defaultEmployeeAvatar from './images/default-employee.png';
import './index.module.scss';

interface EmployeeListItemProps {
  employee: Employee;
  onClick: (e: Employee) => void;
  isHighlighted: boolean;
}
const EmployeeListItem = ({ employee, onClick, isHighlighted }: EmployeeListItemProps) => {
  const avatarImage = employee.avatar || defaultEmployeeAvatar;
  const handleClick = () => onClick(employee);

  return (
    <List.Item onClick={handleClick} active={isHighlighted}>
      <Image avatar={true} src={avatarImage} />
      <List.Content>
        <List.Header>{employee.name}</List.Header>
        <List.Description>+{employee.ecode}</List.Description>
      </List.Content>
    </List.Item>
  );
};

interface DataProps {
  employees: Employee[];
  selectedEmployee?: string;
}

interface CbProps {
  onSelect: (e: Employee) => void;
}

const EmployeesList: React.FC<DataProps & CbProps> = ({
  employees,
  onSelect,
  selectedEmployee,
}) => {
  const employeesList = employees.map((e) => (
    <EmployeeListItem
      employee={e}
      key={e.ecode}
      onClick={onSelect}
      isHighlighted={e.ecode === selectedEmployee}
    />
  ));

  return (
    <List selection={true} relaxed={true} divided={true} verticalAlign="middle" size="big">
      {employeesList}
    </List>
  );
};

export default EmployeesList;
