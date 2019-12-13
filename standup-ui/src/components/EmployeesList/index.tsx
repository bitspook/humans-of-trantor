import React from 'react';
import { Image, List } from 'semantic-ui-react';
import { Employee } from '../../ducks/employees';
import './index.module.scss';

interface EmployeeListItemProps {
  employee: Employee;
  onClick: (e: Employee) => void;
  isHighlighted: boolean;
}
const EmployeeListItem = ({ employee, onClick, isHighlighted }: EmployeeListItemProps) => {
  const avatarImage = employee.avatar || '/images/default-employee.png';

  return (
    <List.Item onClick={() => onClick(employee)} active={isHighlighted}>
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
  selectedEmployee: string;
}

interface CbProps {
  onSelect: (e: Employee) => void;
}

const EmployeesList: React.FC<DataProps & CbProps> = ({
  employees,
  onSelect,
  selectedEmployee,
}) => {
  return (
    <List selection={true} relaxed={true} divided={true} verticalAlign='middle' size='big'>
      {employees.map((e) => (
        <EmployeeListItem
          employee={e}
          key={e.ecode}
          onClick={onSelect}
          isHighlighted={e.ecode === selectedEmployee}
        />
      ))}
    </List>
  );
};

export default EmployeesList;
