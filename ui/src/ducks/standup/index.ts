import { createSlice } from '@reduxjs/toolkit';
import dayjs, { Dayjs } from 'dayjs';
import { FormikHelpers } from 'formik';
import { StandupRowFormData } from 'src/components/StandupForm';

export interface NewStandup {
  ecode: string;
  project: string;
  standup: string;
  date: Dayjs;
  isDelivered: boolean;
  priority: number;
}

export interface Standup extends NewStandup {
  id: string;
  createdAt: Dayjs;
  updatedAt: Dayjs;
}

export interface DeleteStandupPayload {
  standup: Standup;
  helpers: FormikHelpers<StandupRowFormData>;
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
    deleteStandupFail: (s) => s,
    deleteStandupStart: (s) => s,
    deleteStandupSuccess: (state, { payload }: { payload: Standup }) => {
      state.data = state.data.filter((s) => s.id !== payload.id);
    },
    fetchFail: (state, { payload }) => {
      state.isLoading = false;
      state.errors = payload;
    },
    fetchStart: (state, payload) => {
      state.isLoading = true;
      state.errors = [];
    },
    fetchSuccess: (state, { payload }: { payload: Standup[] }) => {
      state.isLoading = false;
      state.errors = [];
      state.data = payload
        .concat(state.data)
        .filter((standup, index, arr) => {
          const isDuplicate = arr.findIndex((s) => s.id === standup.id) !== index;

          return !isDuplicate;
        })
        .map((s) => {
          return {
            ...s,
            createdAt: dayjs(s.createdAt),
            date: dayjs(s.date),
            updatedAt: dayjs(s.updatedAt),
          };
        });
    },
  },
});
