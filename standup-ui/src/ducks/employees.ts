import { createSlice } from 'redux-starter-kit';

export interface Employee {
  ecode: string;
  name: string;
  avatar?: string;
}

const cloudEmployees: Employee[] = [
  {
    ecode: 'E00916',
    name: 'Charanjit Singh'
  },
  {
    ecode: 'E00911',
    name: 'Krishan Saini'
  },
  {
    ecode: 'E00985',
    name: 'Kajal Rani'
  }
];

export default createSlice({
  name: 'employees',
  initialState: cloudEmployees,
  reducers: {}
});
