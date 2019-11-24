import { createAction, createSlice } from 'redux-starter-kit';

const searchContacts = createAction('searchContacts');

export interface AppState {
  contactsSearchFor: string
}

export default createSlice({
  initialState: {
    contactsSearchFor: '',
  },
  reducers: {
    [searchContacts.type]: (state: any, action) => {
      state.contactsSearchFor = action.payload;
    },
  },
  name: 'app'
});
