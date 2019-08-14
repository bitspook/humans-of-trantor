import { EventI } from '@src/events';
import { ObjectSchema } from 'yup';
import * as yup from 'yup';

interface DiscoveredEmployeePayloadI {
  email: string;
  ecode?: string;
  name?: string;
  phoneNumber?: string;
  dateOfBirth?: string;
  designation?: string;
  reportsTo?: string;
}

interface EventSchema {
  [version: string]: ObjectSchema;
}

class DiscoveredEmployee implements EventI {
  static type = 'DISCOVERED_EMPLOYEE';
  static schema: EventSchema = {
    v1: yup.object({
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
    }),
  };
  static versions = Object.keys(DiscoveredEmployee.schema);

  type = 'DISCOVERED_EMPLOYEE';

  constructor(public version: string, public payload: DiscoveredEmployeePayloadI) {}

  async validate() {
    if (!DiscoveredEmployee.versions.find((v) => v === this.version)) {
      throw new Error(
        `UnsupportedVersion: Supported versions [ ${DiscoveredEmployee.versions
          .map((v) => '"' + v + '"')
          .join(', ')} ]`,
      );
    }

    const schema: ObjectSchema = DiscoveredEmployee.schema[this.version];

    return schema.validate(this.payload).then(() => undefined);
  }

  toJSON() {
    return {
      type: this.type,
      version: this.version,
    };
  }
}

export default DiscoveredEmployee;
