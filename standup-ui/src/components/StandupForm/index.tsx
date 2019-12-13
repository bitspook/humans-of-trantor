import fecha from 'fecha';
import { Field, FieldProps, Formik, FormikHelpers, FormikProps, getIn } from 'formik';
import React from 'react';
import { Button, Form, Input, TextArea } from 'semantic-ui-react';
import * as yup from 'yup';
import c from './index.module.scss';

export interface StandupFormValues {
  delivered: {
    date: string;
    standup: string;
  };
  committed: {
    date: string;
    standup: string;
  };
}

const StandupSchema = yup.object().shape({
  delivered: yup
    .object()
    .shape({
      date: yup.date().required('Delivered date is required'),
      standup: yup.string().required('Standup is required'),
    })
    .required(),
  committed: yup
    .object()
    .shape({
      date: yup.date().required('Committed date is required'),
      standup: yup.string().required('Standup is required'),
    })
    .required(),
});

const today = new Date();
const yesterday = new Date().setDate(today.getDate() - 1);
const emptyContact: StandupFormValues = {
  delivered: {
    date: fecha.format(yesterday, 'YYYY-MM-DD'),
    standup: '',
  },
  committed: {
    date: fecha.format(today, 'YYYY-MM-DD'),
    standup: '',
  },
};

interface SemanticFieldProps {
  label: string;
  type?: string;
  fluid?: boolean;
}

const SemanticInputField = (props: FieldProps & SemanticFieldProps) => {
  const error =
    getIn(props.form.touched, props.field.name) && getIn(props.form.errors, props.field.name);

  return (
    <Form.Field error={Boolean(error)}>
      <label>{props.label}</label>
      <Input
        {...props.field}
        fluid={props.fluid}
        type={props.type}
        disabled={props.form.isSubmitting}
      />
      {error && <span className={c.error}>{error}</span>}
    </Form.Field>
  );
};

const SemanticTextAreaField = (props: FieldProps & SemanticFieldProps) => {
  const error =
    getIn(props.form.touched, props.field.name) && getIn(props.form.errors, props.field.name);

  return (
    <Form.Field error={Boolean(error)}>
      <label>{props.label}</label>
      <TextArea {...props.field} fluid={props.fluid} disabled={props.form.isSubmitting} />
      {error && <span className={c.error}>{error}</span>}
    </Form.Field>
  );
};

const InnerForm: React.FC<FormikProps<StandupFormValues>> = (props) => (
  <Form onSubmit={props.handleSubmit} disabled={props.isSubmitting}>
    <h2>Delivered</h2>
    <Field component={SemanticInputField} name='delivered.date' label='Date' type='date' />
    <Field component={SemanticTextAreaField} name='delivered.standup' label='Standup' />

    <h2>Commit</h2>
    <Field component={SemanticInputField} name='committed.date' label='Date' type='date' />
    <Field component={SemanticTextAreaField} name='committed.standup' label='Standup' />

    <Button primary={true} type='submit' disabled={props.isSubmitting}>
      Save
    </Button>
  </Form>
);

interface StandupFormCbProps {
  onSave: (values: StandupFormValues, helpers: FormikHelpers<StandupFormValues>) => void;
}

const NewContact: React.FC<StandupFormCbProps> = ({ onSave }) => {
  return (
    <Formik
      initialValues={emptyContact}
      validationSchema={StandupSchema}
      onSubmit={onSave}
      component={InnerForm}
    />
  );
};

export default NewContact;
