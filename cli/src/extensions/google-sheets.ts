import { google } from 'googleapis';
import { GluegunToolbox } from 'gluegun';

module.exports = async (toolbox: GluegunToolbox) => {
  const { print, prompt, filesystem: fs, system } = toolbox;

  const configDir = fs.path(fs.homedir(), '.config/hot');

  const openInBrowser = (url: string) => {
    if (system.which('firefox')) {
      system.run(`firefox "${url}"`);

      return true;
    }

    if (system.which('google-chrome')) {
      system.run(`google-chrome "${url}"`);

      return true;
    }

    if (system.which('chromium')) {
      system.run(`chromium "${url}"`);

      return true;
    }

    return null;
  };

  const getNewGoogleToken = async (oAuth2Client: any, scope: string[]) => {
    const authUrl = oAuth2Client.generateAuthUrl({
      scope,
      access_type: 'offline',
    });

    const isBrowserOpen = openInBrowser(authUrl);

    if (!isBrowserOpen) {
      print.colors.info(`Authorize this app by visiting this url:\n${authUrl}`);
    } else {
      print.colors.muted('Opened auth url in browser');
    }

    const { code } = await prompt.ask({
      type: 'input',
      name: 'code',
      message: 'Enter the code from google auth page here: ',
    });

    return new Promise((resolve, reject) => {
      oAuth2Client.getToken(code, (err: Error, token: any) => {
        if (err) {
          return reject(err);
        }

        return resolve(token);
      });
    });
  };

  const authenticate = async (creds: any, scopes: string[]) => {
    const { client_secret, client_id, redirect_uris } = creds.installed;
    const oAuth2Client = new google.auth.OAuth2(
      client_id,
      client_secret,
      redirect_uris[0],
    );

    const tokenFile = fs.path(configDir, 'google-tokens.json');

    let token = fs.read(tokenFile, 'json');
    if (!token) {
      print.colors.info(
        'Unable to find saved google-sheets login. Proceeding to create a new session.',
      );

      token = await getNewGoogleToken(oAuth2Client, scopes);
      fs.write(tokenFile, token, { jsonIndent: 2 });
    }

    oAuth2Client.setCredentials(token);

    return oAuth2Client;
  };

  toolbox.googleSheet = async () => {
    const credsFile = fs.path(configDir, 'google-creds.json');
    const creds = fs.read(credsFile, 'json');

    if (!creds) {
      print.error(`${credsFile} must be present for using googleSheets`);
      return -1;
    }

    const scopes = ['https://www.googleapis.com/auth/spreadsheets'];

    const auth = await authenticate(creds, scopes);

    return google.sheets({ auth, version: 'v4' });
  };
};
