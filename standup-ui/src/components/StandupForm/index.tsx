import { Field, FieldProps, Formik, FormikHelpers, FormikProps, getIn } from 'formik';
import React from 'react';
import { Button, Form, TextArea } from 'semantic-ui-react';
import * as yup from 'yup';
import c from './index.module.scss';

export interface StandupFormValues {
  delivered: string;
  committed: string;
  impediment: string;
}

const StandupSchema = yup.object().shape({
  committed: yup.string(),
  delivered: yup.string(),
  impediment: yup.string(),
});

interface SemanticFieldProps {
  label: string;
  type?: string;
  fluid?: boolean;
}

const SemanticTextAreaField = (props: FieldProps & SemanticFieldProps) => {
  const error =
    getIn(props.form.touched, props.field.name) && getIn(props.form.errors, props.field.name);

  return (
    <Form.Field error={Boolean(error)}>
      <TextArea {...props.field} fluid={props.fluid} disabled={props.form.isSubmitting} />
      {error && <span className={c.error}>{error}</span>}
    </Form.Field>
  );
};

const InnerForm: React.FC<FormikProps<StandupFormValues>> = (props) => (
  <Form onSubmit={props.handleSubmit} disabled={props.isSubmitting}>
    <h2>Committed</h2>
    <Field component={SemanticTextAreaField} name='committed' />

    <h2>Delivered</h2>
    <Field component={SemanticTextAreaField} name='delivered' />

    <h2>Impediment</h2>
    <Field component={SemanticTextAreaField} name='impediment' />

    <Button primary={true} type='submit' disabled={props.isSubmitting}>
      Save
    </Button>

    <p>*Empty values will not be saved</p>
  </Form>
);

interface DataProps {
  initialValues: StandupFormValues;
}

interface CbProps {
  onSave: (values: StandupFormValues, helpers: FormikHelpers<StandupFormValues>) => void;
}

const StandupForm: React.FC<CbProps & DataProps> = (p) => {
  return (
    <Formik
      enableReinitialize={true}
      initialValues={p.initialValues}
      validationSchema={StandupSchema}
      onSubmit={p.onSave}
      component={InnerForm}
    />
  );
};

export default StandupForm;
