import { Field, FieldProps, Formik, FormikHelpers, FormikProps, getIn } from 'formik';
import React from 'react';
import classnames from 'classnames';
import * as yup from 'yup';
import c from './index.module.scss';

interface MissingFieldProps {
  placeholder: string;
}

const InputField = (p: FieldProps & MissingFieldProps) => {
  const error = getIn(p.form.touched, p.field.name) && getIn(p.form.errors, p.field.name);

  return (
    <div
      className={classnames(c.inputField, {
        [c.hasError]: Boolean(error),
      })}>
      <input {...p.field} disabled={p.form.isSubmitting} placeholder={p.placeholder}></input>
      {error && <span className={c.error}>{error}</span>}
    </div>
  );
};

const InnerForm: React.FC<FormikProps<LoginFormValues>> = (p) => {
  return (
    <div className={c.container}>
      <div className={c.formContainer}>
        <h1 className={c.title}>Humans of Trantor</h1>

        <form className={c.form} onSubmit={p.handleSubmit}>
          <Field component={InputField} name="email" placeholder="E-mail address" />
          <Field component={InputField} name="password" placeholder="Password" type="password" />

          <button className={c.loginButton} type="submit">
            Login
          </button>
        </form>
      </div>
    </div>
  );
};

interface LoginFormValues {
  email: string;
  password: string;
}

const LoginFormSchema = yup.object().shape({
  email: yup
    .string()
    .email()
    .required(),
  password: yup
    .string()
    .min(6)
    .required(),
});

const Login: React.FC = () => {
  return (
    <Formik
      initialValues={{ email: '', password: '' }}
      validationSchema={LoginFormSchema}
      onSubmit={(val) => console.warn('SUBMITTING', val)}
      component={InnerForm}
    />
  );
};

export default Login;
