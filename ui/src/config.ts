interface Config {
  basePath: string;
  urls: {
    ei: string;
    pms: string;
    iam: string;
  };
}

if (!process.env.REACT_APP_EI_URL) {
  throw new Error('REACT_APP_EI_URL environment variable must be set');
}

if (!process.env.REACT_APP_PMS_URL) {
  throw new Error('REACT_APP_PMS_URL environment variable must be set');
}

if (!process.env.REACT_APP_IAM_URL) {
  throw new Error('REACT_APP_IAM_URL environment variable must be set');
}

const config: Config = {
  basePath: process.env.REACT_APP_BASE_PATH || '/',
  urls: {
    ei: process.env.REACT_APP_EI_URL,
    iam: process.env.REACT_APP_IAM_URL,
    pms: process.env.REACT_APP_PMS_URL,
  },
};

export default config;
