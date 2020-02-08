import { Field, FieldProps, Formik, FormikHelpers, FormikProps, getIn } from 'formik';
import React from 'react';
import { bindActionCreators, Dispatch } from 'redux';

import classnames from 'classnames';
import { connect } from 'react-redux';
import * as yup from 'yup';

import { State } from 'src/reducer';
import duck, { LoginPayload, LoginValues } from './duck';
import c from './index.module.scss';

interface MissingFieldProps {
  placeholder: string;
  type?: string;
}

const InputField = (p: FieldProps & MissingFieldProps) => {
  const error = getIn(p.form.touched, p.field.name) && getIn(p.form.errors, p.field.name);
  const containerClass = classnames(c.inputField, {
    [c.hasError]: Boolean(error),
  });

  return (
    <div className={containerClass}>
      <input
        {...p.field}
        disabled={p.form.isSubmitting}
        placeholder={p.placeholder}
        type={p.type || 'text'}
      />
      {error && <span className={c.error}>{error}</span>}
    </div>
  );
};

const InnerForm: React.FC<FormikProps<LoginValues>> = (p) => {
  const loaderCls = classnames(c.loader, {
    [c.isLoading]: p.isSubmitting,
  });

  return (
    <div className={c.container}>
      <div className={c.formContainer}>
        <h1 className={c.title}>Humans of Trantor</h1>

        <form className={c.form} onSubmit={p.handleSubmit}>
          <Field component={InputField} name="email" placeholder="E-mail address" />
          <Field component={InputField} name="password" placeholder="Password" type="password" />

          <button className={c.loginButton} type="submit" disabled={p.isSubmitting}>
            {p.isSubmitting ? 'Logging In' : 'Login'}
            <span className={loaderCls} />
          </button>
        </form>
      </div>
    </div>
  );
};

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

interface LoginFormDataProps {
  isLoggingIn: boolean;
}

interface LoginFormCbProps {
  loginStart: (payload: LoginPayload) => void;
}

const Login: React.FC<LoginFormDataProps & LoginFormCbProps> = (p) => {
  const handleSubmit = (values: LoginValues, helpers: FormikHelpers<LoginValues>) =>
    p.loginStart({ values, helpers });

  return (
    <Formik
      initialValues={{ email: '', password: '' }}
      validationSchema={LoginFormSchema}
      onSubmit={handleSubmit}
      component={InnerForm}
    />
  );
};

const mapState = (state: State): LoginFormDataProps => ({ ...state.login });

const mapDispatch = (dispatch: Dispatch) => ({
  ...bindActionCreators(duck.actions, dispatch),
});

export default connect(mapState, mapDispatch)(Login);
