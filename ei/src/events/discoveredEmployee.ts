import { EventI } from '@src/events';
import yup from 'yup';

interface DiscoveredEmployeePayloadI {
  email: string;
  ecode?: string;
  name?: string;
  phoneNumber?: string;
  dateOfBirth?: string;
  designation?: string;
  reportsTo?: string;
}

class DiscoveredEmployee implements EventI {
  static type = 'DISCOVERED_EMPLOYEE';
  type = 'DISCOVERED_EMPLOYEE';

  constructor(public payload: DiscoveredEmployeePayloadI) {}

  async validate() {
    return yup
      .object({
        email: yup
          .string()
          .email()
          .required(),
        ecode: yup.string(),
        name: yup.string(),
        phoneNumber: yup.string(),
        dateOfBirth: yup.date(),
        designation: yup.string(),
        reportsTo: yup.string(),
      })
      .validate(this.payload)
      .then(() => undefined);
  }
}

export default DiscoveredEmployee;
