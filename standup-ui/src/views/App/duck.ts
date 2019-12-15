import { createSlice } from '@reduxjs/toolkit';
import dayjs, { Dayjs } from 'dayjs';
import { FormikHelpers } from 'formik';
import { StandupFormValues } from 'src/components/StandupForm';
import { ToastDataProps } from 'src/components/Toaster';

export interface SaveStandupPayload {
  day: Dayjs;
  ecode: string;
  project: string;
  standup: StandupFormValues;
  helpers: FormikHelpers<StandupFormValues>;
}

export interface AppState {
  selectedEmployee?: string;
  isSavingStandup: boolean;
  saveStandupError?: Error;
  selectedDay: dayjs.Dayjs;
  selectedProject: string;
  toasts: ToastDataProps[];
}

const initialState: AppState = {
  isSavingStandup: false,
  saveStandupError: undefined,
  selectedDay: dayjs(new Date()),
  selectedEmployee: undefined,
  selectedProject: 'Veriown',
  toasts: [],
};

export default createSlice({
  initialState,
  name: 'app',
  reducers: {
    hideToast: (state: AppState, { payload }) => {
      state.toasts = state.toasts.filter((t) => t.key === payload.key);
    },
    saveStandupFail: (state: AppState, { payload }) => {
      state.isSavingStandup = false;
      state.saveStandupError = payload;
    },
    saveStandupStart: (state: AppState) => {
      state.isSavingStandup = true;
      state.saveStandupError = undefined;
    },
    saveStandupSuccess: (state: AppState) => {
      state.isSavingStandup = false;
    },
    selectDay: (state: AppState, { payload }) => {
      state.selectedDay = payload;
    },
    selectEmployee: (state: AppState, { payload }) => {
      state.selectedEmployee = payload.ecode;
    },
    showToast: (state: AppState, { payload }) => {
      state.toasts = state.toasts.concat([payload]);
    },
  },
});
