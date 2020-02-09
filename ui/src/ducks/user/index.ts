import { createSlice } from '@reduxjs/toolkit';

export interface UserState {
  session?: {
    accessToken: string;
    refreshToken: string;
  };
}

const initialState: UserState = {
  session: undefined,
};

export default createSlice({
  initialState,
  name: 'user',
  reducers: {
    loginSuccess: (state: UserState, { payload }) => {
      state.session = payload;
    },
    logout: (state: UserState) => {
      state.session = undefined;
    },
  },
});
