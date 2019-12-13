import { createSlice } from '@reduxjs/toolkit';

export interface Employee {
  ecode: string;
  name: string;
  designation: string;
  avatar?: string;
}

export interface EmployeesState {
  isLoading: boolean;
  errors: Error[];
  data: Employee[];
}

const initialState: EmployeesState = {
  data: [],
  errors: [],
  isLoading: false,
};

export default createSlice({
  initialState,
  name: 'employees',
  reducers: {
    fetchEmployeesFailed: (state, { payload }) => {
      state.isLoading = false;
      state.errors = Array.isArray(payload) ? payload : [{ message: payload.message }];
    },
    fetchEmployeesStart: (state) => {
      state.isLoading = true;
    },
    fetchEmployeesSuccess: (state, { payload }) => {
      state.isLoading = false;
      state.errors = [];
      state.data = payload;
    },
  },
});
