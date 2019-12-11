import { EventI } from '@src/events';
import { ObjectSchema } from 'yup';
// tslint:disable-next-line:no-duplicate-imports
import * as yup from 'yup';

interface DiscoveredEmployeePayloadI {
  email: string;
  ecode: string;
  name: string;
  skypeId?: string;
  phoneNumber?: string;
  dateOfBirth?: string;
  designation?: string;
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
      ecode: yup.string().required(),
      name: yup.string().required(),
      skypeId: yup.string(),
      phoneNumber: yup.string(),
      dateOfBirth: yup.date(),
      designation: yup.string(),
    }),
  };
  static versions = Object.keys(DiscoveredEmployee.schema);

  type = 'DISCOVERED_EMPLOYEE';
  public id?: string;

  constructor(public version: string, public payload: DiscoveredEmployeePayloadI) { }

  async validate() {
    if (!DiscoveredEmployee.versions.find((v) => v === this.version)) {
      throw new Error(
        `UnsupportedVersion: Supported versions [ ${DiscoveredEmployee.versions
          // tslint:disable-next-line:prefer-template
          .map((v) => '"' + v + '"')
          .join(', ')} ]`,
      );
    }

    const schema: ObjectSchema = DiscoveredEmployee.schema[this.version];

    return schema.validate(this.payload).then(() => undefined);
  }

  toJSON() {
    return {
      id: this.id,
      type: this.type,
      version: this.version,
      payload: this.payload,
    };
  }
}

export default DiscoveredEmployee;
