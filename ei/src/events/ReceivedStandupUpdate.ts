import { EventI } from '@src/events';
import { ObjectSchema } from 'yup';
// tslint:disable-next-line:no-duplicate-imports
import * as yup from 'yup';

interface ReceivedStandupUpdatePayloadI {
  ecode: string;
  project: string;
  standup: string;
  date: string;
  isEod: boolean;
}

interface EventSchema {
  [version: string]: ObjectSchema;
}

class ReceivedStandupUpdate implements EventI {
  static type = 'RECEIVED_STANDUP_UPDATE';
  static schema: EventSchema = {
    v1: yup.object({
      ecode: yup.string().required(),
      project: yup.string().required(),
      standup: yup.string().required(),
      isEod: yup.boolean().required(),
      date: yup.date().required(),
    }),
  };
  static versions = Object.keys(ReceivedStandupUpdate.schema);

  type = ReceivedStandupUpdate.type;
  public id?: string;

  constructor(public version: string, public payload: ReceivedStandupUpdatePayloadI) {}

  async validate() {
    if (!ReceivedStandupUpdate.versions.find((v) => v === this.version)) {
      throw new Error(
        `UnsupportedVersion: Supported versions [ ${ReceivedStandupUpdate.versions
          // tslint:disable-next-line:prefer-template
          .map((v) => '"' + v + '"')
          .join(', ')} ]`,
      );
    }

    const schema: ObjectSchema = ReceivedStandupUpdate.schema[this.version];

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

export default ReceivedStandupUpdate;
