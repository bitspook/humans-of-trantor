import { createSlice } from '@reduxjs/toolkit';
import { Dayjs } from 'dayjs';

export interface Standup {
  standupType: 'delivered' | 'committed' | 'impediment';
  standup: string;
  date: Dayjs;
  ecode: string;
  project: string;
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
      state.data = state.data
        .concat(payload)
        .filter((standup, index, arr) => {
          const getKey = (s: Standup) => `${s.ecode}-${s.project}-${s.date}-${s.standupType}`;
          const isDuplicate = arr.findIndex((s) => getKey(s) === getKey(standup)) !== index;

          return !isDuplicate;
        });
    },
  },
});
