import { createAction, createSlice } from 'redux-starter-kit';
import { StandupFormValues } from 'src/components/StandupForm';
import { FormikHelpers } from 'formik';

export interface SaveStandupPayload {
  ecode: string;
  standup: StandupFormValues;
  helpers: FormikHelpers<StandupFormValues>;
}

const selectEmployee = createAction('selectEmployee');
const startSaveStandup = createAction('startSaveStandup');
const fullfillSaveStandup = createAction('fullfillSaveStandup');
const failSaveStandup = createAction('failSaveStandup');

export interface AppState {
  selectedEmployee: string;
  isSavingStandup: boolean;
  saveStandupError: string;
}

export default createSlice({
  initialState: {
    selectedEmployee: 'E00916',
    isSavingStandup: false,
    saveStandupError: ''
  },
  reducers: {
    [selectEmployee.type]: (state: any, action) => {
      state.selectedEmployee = action.payload.ecode;
    },
    [startSaveStandup.type]: (state: AppState) => {
      state.isSavingStandup = true;
      state.saveStandupError = '';
    },
    [fullfillSaveStandup.type]: (state: AppState) => {
      state.isSavingStandup = false;
    },
    [failSaveStandup.type]: (state: AppState, { payload }) => {
      state.isSavingStandup = false;
      state.saveStandupError = payload;
    }
  },
  name: 'app'
});
