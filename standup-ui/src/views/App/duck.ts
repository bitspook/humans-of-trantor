import { createSlice } from '@reduxjs/toolkit';
import dayjs, { Dayjs } from 'dayjs';
import { FormikHelpers } from 'formik';
import { StandupFormValues } from 'src/components/StandupForm';
import { ToastDataProps } from 'src/components/Toaster';
import { Employee } from 'src/ducks/employees';
import { Standup } from 'src/ducks/standup';

export interface SaveStandupPayload {
  day: Dayjs;
  ecode: string;
  project: string;
  standup: StandupFormValues;
  helpers: FormikHelpers<StandupFormValues>;
}

export interface Report {
  employee: Employee;
  yesterday?: Standup;
  today?: Standup;
  impediment?: Standup;
}

export interface ReportState {
  data: Report[];
  isVisible: boolean;
}

export interface AppState {
  selectedEmployee?: string;
  isSavingStandup: boolean;
  saveStandupError?: Error;
  selectedDay: dayjs.Dayjs;
  selectedProject: string;
  toasts: { [key: string]: ToastDataProps };
  report: ReportState;
}

const initialState: AppState = {
  isSavingStandup: false,
  report: {
    data: [],
    isVisible: false,
  },
  saveStandupError: undefined,
  selectedDay: dayjs(new Date()),
  selectedEmployee: undefined,
  selectedProject: 'Veriown',
  toasts: {},
};

export default createSlice({
  initialState,
  name: 'app',
  reducers: {
    createReport: (state: AppState, { payload }) => {
      state.report.data = payload;
    },
    hideReport: (state: AppState) => {
      state.report.isVisible = false;
    },
    hideToast: (state: AppState, { payload }) => {
      state.toasts = Object.entries(state.toasts)
        .filter(([key]) => key !== payload.key)
        .reduce((accum, [key, toast]) => {
          accum[key] = toast;

          return accum;
        }, {} as { [key: string]: ToastDataProps });
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
    showReport: (state: AppState) => {
      state.report.isVisible = true;
    },
    showToast: (state: AppState, { payload }) => {
      state.toasts[payload.key] = payload;
    },
  },
});
