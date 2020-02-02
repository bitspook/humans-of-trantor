import React from 'react';
import c from './index.module.scss';

const Login: React.FC = () => {
  return (
    <div className={c.container}>
      <div className={c.formContainer}>
        <h1 className={c.title}>Humans of Trantor</h1>
        <form className={c.form}>
          <input placeholder="E-mail address" />
          <input placeholder="Password" type="password" />

          <button className={c.loginButton}>Login</button>
        </form>
      </div>
    </div>
  );
};

export default Login;
