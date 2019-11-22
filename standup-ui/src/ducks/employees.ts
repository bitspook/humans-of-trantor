import { createSlice } from 'redux-starter-kit';

export interface Employee {
  eCode: string;
  name: string;
  avatar?: string;
}

const cloudEmployees: Employee[] = [
  {
    eCode: 'E00916',
    name: 'Charanjit Singh'
  }
];

export default createSlice({
  name: 'employees',
  initialState: cloudEmployees,
  reducers: {
  },
});
