import { createAction, createSlice } from 'redux-starter-kit';

const selectEmployee = createAction('selectEmployee');

export interface AppState {
  selectedEmployee: string;
}

export default createSlice({
  initialState: {
    selectedEmployee: 'E00916'
  },
  reducers: {
    [selectEmployee.type]: (state: any, action) => {
      state.selectedEmployee = action.payload.ecode;
    }
  },
  name: 'app'
});
