import { createSlice } from '@reduxjs/toolkit';

export interface Project {
  id: string;
  name: string;
  logo?: string;
}

export interface ProjectsState {
  isLoading: boolean;
  errors: Error[];
  data: Project[];
}

const initialState: ProjectsState = {
  data: [
    { id: '001', name: 'abc' },
    { id: '002', name: 'def' },
    { id: '003', name: 'ghi' },
    { id: '004', name: 'jkl' },
  ],
  errors: [],
  isLoading: false,
};

export default createSlice({
  initialState,
  name: 'projects',
  reducers: {
    fetchFail: (state, { payload }) => {
      state.isLoading = false;
      state.errors = Array.isArray(payload) ? payload : [{ message: payload.message }];
    },
    fetchStart: (state) => {
      state.isLoading = true;
    },
    fetchSuccess: (state, { payload }) => {
      state.isLoading = false;
      state.errors = [];
      state.data = payload;
    },
  },
});
