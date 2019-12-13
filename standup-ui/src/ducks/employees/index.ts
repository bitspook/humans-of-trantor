import { createSlice } from '@reduxjs/toolkit';

export interface Employee {
  ecode: string;
  name: string;
  designation: string;
  avatar?: string;
}

export interface DisplayableError {
  name?: string;
  message: string;
}

export interface EmployeesState {
  isLoading: boolean;
  errors: DisplayableError[];
  data: Employee[];
}

const initialState: EmployeesState = {
  isLoading: false,
  errors: [],
  data: [],
};

export default createSlice({
  name: 'employees',
  initialState,
  reducers: {
    fetchEmployeesStart: (state) => {
      state.isLoading = true;
    },
    fetchEmployeesSuccess: (state, { payload }) => {
      state.isLoading = false;
      state.errors = [];
      state.data = payload;
    },
    fetchEmployeesFailed: (state, { payload }) => {
      state.isLoading = false;
      state.errors = Array.isArray(payload) ? payload : [{ message: payload.message }];
    },
  },
});
