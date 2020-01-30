interface Config {
  urls: {
    ei: string;
    pms: string;
  };
}

if (!process.env.REACT_APP_EI_URL) {
  throw new Error('REACT_APP_EI_URL environment variable must be set');
}

if (!process.env.REACT_APP_PMS_URL) {
  throw new Error('REACT_APP_PMS_URL environment variable must be set');
}

const config: Config = {
  urls: {
    ei: process.env.REACT_APP_EI_URL,
    pms: process.env.REACT_APP_PMS_URL,
  },
};

export default config;
