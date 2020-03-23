import { createSlice } from '@reduxjs/toolkit';
import dayjs from 'dayjs';
import { FormikHelpers } from 'formik';
import { Report } from 'src/components/Report';
import { NewStandupFormData, StandupRowFormData } from 'src/components/StandupForm';
import { ToastDataProps } from 'src/components/Toaster';
import { Standup } from 'src/ducks/standup';

export interface CreateStandupPayload {
  data: NewStandupFormData;
  helpers: FormikHelpers<NewStandupFormData>;
}

export interface SaveStandupPayload {
  data: StandupRowFormData;
  helpers: FormikHelpers<StandupRowFormData>;
}

export interface DeleteStandupPayload {
  standup: Standup;
  helpers: FormikHelpers<StandupRowFormData>;
}

export interface ReportState {
  data: Report[];
  isVisible: boolean;
}

export interface StandupMeetingState {
  selectedEmployee?: string;
  isCreatingStandup: boolean;
  createStandupError?: Error;
  selectedDay: dayjs.Dayjs;
  selectedProject: string;
  toasts: { [key: string]: ToastDataProps };
  report: ReportState;
}

const initialState: StandupMeetingState = {
  createStandupError: undefined,
  isCreatingStandup: false,
  report: {
    data: [],
    isVisible: false,
  },
  selectedDay: dayjs(new Date()),
  selectedEmployee: undefined,
  selectedProject: 'Veriown',
  toasts: {},
};

export default createSlice({
  initialState,
  name: 'standupMeeting',
  reducers: {
    createReport: (state: StandupMeetingState, { payload }) => {
      state.report.data = payload;
    },
    createStandupFail: (state: StandupMeetingState, { payload }) => {
      state.isCreatingStandup = false;
      state.createStandupError = payload;
    },
    createStandupStart: (state: StandupMeetingState) => {
      state.isCreatingStandup = true;
      state.createStandupError = undefined;
    },
    createStandupSuccess: (state: StandupMeetingState) => {
      state.isCreatingStandup = false;
    },
    deleteStandupFail: (s) => s,
    deleteStandupStart: (s) => s,
    deleteStandupSuccess: (s) => s,
    hideReport: (state: StandupMeetingState) => {
      state.report.isVisible = false;
    },
    hideToast: (state: StandupMeetingState, { payload }) => {
      state.toasts = Object.entries(state.toasts)
        .filter(([key]) => key !== payload.key)
        .reduce((accum, [key, toast]) => {
          accum[key] = toast;

          return accum;
        }, {} as { [key: string]: ToastDataProps });
    },
    saveStandupFail: (state: StandupMeetingState) => {
      return state;
    },
    saveStandupStart: (state: StandupMeetingState) => {
      return state;
    },
    saveStandupSuccess: (state: StandupMeetingState) => {
      return state;
    },
    selectDay: (state: StandupMeetingState, { payload }) => {
      state.selectedDay = payload;
    },
    selectEmployee: (state: StandupMeetingState, { payload }) => {
      state.selectedEmployee = payload.ecode;
    },
    showReport: (state: StandupMeetingState) => {
      state.report.isVisible = true;
    },
    showToast: (state: StandupMeetingState, { payload }) => {
      state.toasts[payload.key] = payload;
    },
  },
});
