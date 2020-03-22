import { createSlice } from '@reduxjs/toolkit';
import dayjs, { Dayjs } from 'dayjs';
import { FormikHelpers } from 'formik';
import { Report } from 'src/components/Report';
import { StandupRowFormData } from 'src/components/StandupForm';
import { ToastDataProps } from 'src/components/Toaster';

export interface SaveStandupPayload {
  data: StandupRowFormData;
  helpers: FormikHelpers<StandupRowFormData>;
}

export interface ReportState {
  data: Report[];
  isVisible: boolean;
}

export interface StandupMeetingState {
  selectedEmployee?: string;
  isSavingStandup: boolean;
  saveStandupError?: Error;
  selectedDay: dayjs.Dayjs;
  selectedProject: string;
  toasts: { [key: string]: ToastDataProps };
  report: ReportState;
}

const initialState: StandupMeetingState = {
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
  name: 'standupMeeting',
  reducers: {
    createReport: (state: StandupMeetingState, { payload }) => {
      state.report.data = payload;
    },
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
    saveStandupFail: (state: StandupMeetingState, { payload }) => {
      state.isSavingStandup = false;
      state.saveStandupError = payload;
    },
    saveStandupStart: (state: StandupMeetingState) => {
      state.isSavingStandup = true;
      state.saveStandupError = undefined;
    },
    saveStandupSuccess: (state: StandupMeetingState) => {
      state.isSavingStandup = false;
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
