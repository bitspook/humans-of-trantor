import { Field, FieldProps, Formik, FormikHelpers, FormikProps } from 'formik';
import React, { ChangeEvent } from 'react';
import { Standup } from 'src/ducks/standup';
import c from './index.module.scss';

const StandupField = (p: FieldProps<Standup>) => {
  const standup = p.field.value;

  const handleChange = (event: ChangeEvent<HTMLInputElement>) => {
    const haveChangedDelivery = event.target.name === 'is-delivered';

    const newStandup = {
      ...p.field.value,
      isDelivered: haveChangedDelivery ? event.target.checked : standup.isDelivered,
      standup: haveChangedDelivery ? standup.standup : event.target.value,
    };

    p.field.onChange({
      ...event,
      target: { ...event.target, name: p.field.name, value: newStandup },
    });

    if (haveChangedDelivery) {
      p.form.submitForm();
    }
  };

  return (
    <div className={c.standupRow}>
      <div className={c.isDeliveredCheckbox}>
        <label className={standup.isDelivered ? c.checked : ''} htmlFor="standup-is-delivered" />
        <input
          name="is-delivered"
          type="checkbox"
          defaultChecked={standup.isDelivered}
          onChange={handleChange}
        />
      </div>

      <input type="text" name="standup" value={standup.standup} onChange={handleChange} />
    </div>
  );
};

interface StandupRowProps {
  standup: Standup;
  onSave: (data: StandupRowFormData, helpers: FormikHelpers<StandupRowFormData>) => void;
}

export interface StandupRowFormData {
  standup: Standup;
}

const StandupRow = (p: StandupRowProps) => {
  const InnerForm = (formik: FormikProps<StandupRowFormData>) => (
    <form onSubmit={formik.handleSubmit}>
      <Field component={StandupField} name="standup" />
    </form>
  );

  return (
    <Formik initialValues={{ standup: p.standup }} onSubmit={p.onSave}>
      {InnerForm}
    </Formik>
  );
};

interface DataProps {
  ecode: string;
  standups: Standup[];
}

interface CbProps {
  onSave: (data: StandupRowFormData, helpers: FormikHelpers<StandupRowFormData>) => void;
}

const StandupForm: React.FC<CbProps & DataProps> = (p) => {
  const standupRows = p.standups.map((s) => (
    <StandupRow key={s.id} standup={s} onSave={p.onSave} />
  ));

  return <>{standupRows}</>;
};

export default StandupForm;
