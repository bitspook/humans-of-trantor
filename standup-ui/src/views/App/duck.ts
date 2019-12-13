import { createSlice } from '@reduxjs/toolkit';
import dayjs, { Dayjs } from 'dayjs';
import { FormikHelpers } from 'formik';
import { StandupFormValues } from 'src/components/StandupForm';

export interface SaveStandupPayload {
  day: Dayjs;
  ecode: string;
  standup: StandupFormValues;
  helpers: FormikHelpers<StandupFormValues>;
}

export interface AppState {
  selectedEmployee: string;
  isSavingStandup: boolean;
  saveStandupError: string;
  selectedDay: dayjs.Dayjs;
}

const initialState: AppState = {
  isSavingStandup: false,
  saveStandupError: '',
  selectedDay: dayjs(new Date()),
  selectedEmployee: 'E00916',
};

export default createSlice({
  initialState,
  name: 'app',
  reducers: {
    failSaveStandup: (state: AppState, { payload }) => {
      state.isSavingStandup = false;
      state.saveStandupError = payload;
    },
    fullfillSaveStandup: (state: AppState) => {
      state.isSavingStandup = false;
    },
    selectDay: (state: AppState, { payload }) => {
      state.selectedDay = payload;
    },
    selectEmployee: (state: AppState, { payload }) => {
      state.selectedEmployee = payload.ecode;
    },
    startSaveStandup: (state: AppState) => {
      state.isSavingStandup = true;
      state.saveStandupError = '';
    },
  },
});
