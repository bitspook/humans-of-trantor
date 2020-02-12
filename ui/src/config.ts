interface Config {
  basename: string;
  routes: {
    root: string;
    login: string;
    standupMeeting: string;
  };
  urls: {
    core: string;
    ei: string;
  };
}

if (!process.env.REACT_APP_CORE_API_URL) {
  throw new Error('REACT_APP_CORE_API_URL environment variable must be set');
}

if (!process.env.REACT_APP_EI_URL) {
  throw new Error('REACT_APP_EI_URL environment variable must be set');
}

const basename = process.env.REACT_APP_BASE_PATH || '/';

const config: Config = {
  basename,
  routes: {
    login: '/login',
    root: '/',
    standupMeeting: '/standup-meeting',
  },
  urls: {
    core: process.env.REACT_APP_CORE_API_URL,
    ei: process.env.REACT_APP_EI_URL,
  },
};

export default config;
