import { createSlice } from '@reduxjs/toolkit';
import { Dayjs } from 'dayjs';

export interface Standup {
  standupType: 'delivered' | 'committed' | 'impediment';
  standup: string;
  date: Dayjs;
  ecode: string;
}

export interface StandupState {
  data: Standup[];
  errors: Error[];
  isLoading: boolean;
}

const initialState: StandupState = {
  data: [],
  errors: [],
  isLoading: false,
};

export default createSlice({
  initialState,
  name: 'standup',
  reducers: {
    fetchFail: (state, { payload }) => {
      state.isLoading = false;
      state.errors = payload;
    },
    fetchStart: (state, payload) => {
      state.isLoading = true;
      state.errors = [];
    },
    fetchSuccess: (state, { payload }) => {
      state.isLoading = false;
      state.errors = [];
      state.data = payload;
    },
  },
});
