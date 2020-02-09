import { createSlice } from '@reduxjs/toolkit';
import { FormikHelpers } from 'formik';

export interface LoginValues {
  email: string;
  password: string;
}

export interface LoginPayload {
  values: LoginValues;
  helpers: FormikHelpers<LoginValues>;
}

export interface LoginState {
  isLoggingIn: boolean;
  errors: Error[];
}

const initialState: LoginState = {
  errors: [],
  isLoggingIn: false,
};

export default createSlice({
  initialState,
  name: 'login',
  reducers: {
    loginFail: (state: LoginState, { payload }) => {
      if (Array.isArray(payload)) {
        state.errors = payload;
      } else {
        state.errors = [payload];
      }
    },
    loginStart: (state: LoginState) => {
      state.isLoggingIn = true;
    },
  },
});
