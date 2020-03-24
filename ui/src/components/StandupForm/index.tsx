import { Field, FieldProps, Formik, FormikHelpers, FormikProps } from 'formik';
import React, { ChangeEvent, SyntheticEvent } from 'react';
import { Standup } from 'src/ducks/standup';
import * as yup from 'yup';
import c from './index.module.scss';

const StandupField = (p: FieldProps<Standup> & { onDelete: (e: SyntheticEvent) => void }) => {
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
          disabled={p.form.isSubmitting}
        />
      </div>

      <input
        type="text"
        name="standup"
        value={standup.standup}
        onChange={handleChange}
        disabled={p.form.isSubmitting}
      />

      <button type="button" className={c.delete} onClick={p.onDelete} />
    </div>
  );
};

const NewStandupField: React.FC<FieldProps<string>> = (p) => {
  return (
    <input className={c.newStandupInput} type="text" {...p.field} disabled={p.form.isSubmitting} />
  );
};

interface StandupRowProps {
  standup: Standup;
  onSave: (data: StandupRowFormData, helpers: FormikHelpers<StandupRowFormData>) => void;
  onDelete: (standup: Standup, helpers: FormikHelpers<StandupRowFormData>) => void;
}

export interface StandupRowFormData {
  standup: Standup;
}

const StandupRow = (p: StandupRowProps) => {
  const InnerForm = (formik: FormikProps<StandupRowFormData>) => (
    <form onSubmit={formik.handleSubmit}>
      <Field
        component={StandupField}
        name="standup"
        onDelete={() => p.onDelete(p.standup, formik)}
      />
    </form>
  );

  return (
    <Formik initialValues={{ standup: p.standup }} onSubmit={p.onSave}>
      {InnerForm}
    </Formik>
  );
};

export interface NewStandupFormData {
  standup: string;
}

interface NewStandupFormProps {
  onSubmit: (value: NewStandupFormData, helpers: FormikHelpers<NewStandupFormData>) => void;
}

const NewStandupForm: React.FC<NewStandupFormProps> = (p) => {
  const InnerForm = (formik: FormikProps<{}>) => (
    <form onSubmit={formik.handleSubmit}>
      <Field name="standup" component={NewStandupField} />
    </form>
  );

  const validationSchema = yup.object().shape({
    standup: yup.string().required(),
  });

  return (
    <Formik
      initialValues={{ standup: '' }}
      validationSchema={validationSchema}
      onSubmit={p.onSubmit}>
      {InnerForm}
    </Formik>
  );
};

interface StandupFormProps {
  standups: Standup[];
  onSave: (data: StandupRowFormData, helpers: FormikHelpers<StandupRowFormData>) => void;
  onCreate: (data: NewStandupFormData, helpers: FormikHelpers<NewStandupFormData>) => void;
  onDelete: (standup: Standup, helpers: FormikHelpers<StandupRowFormData>) => void;
}

const StandupForm: React.FC<StandupFormProps> = (p) => {
  const standupRows = p.standups.map((s) => (
    <StandupRow key={s.id} standup={s} onSave={p.onSave} onDelete={p.onDelete} />
  ));

  return (
    <>
      <NewStandupForm onSubmit={p.onCreate} />
      {standupRows}
    </>
  );
};

export default StandupForm;
