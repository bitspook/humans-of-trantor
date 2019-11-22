import { GluegunToolbox } from 'gluegun';
import * as cheerio from 'cheerio';
import { Cookie, CookieJar } from 'tough-cookie';
import * as FormData from 'form-data';
import { promisify } from 'util';
import * as got from 'got';
import 'global-agent/bootstrap';
import { GluegunPrint } from 'gluegun';

const synergyUrl = 'http://synergy.trantorinc.com';
const baseHeaders = {
  'User-Agent':
    'Mozilla/5.0 (X11; Linux x86_64; rv:69.0) Gecko/20100101 Firefox/69.0',
};
const eiUrl = 'http://localhost:3002';

const collectCookies = async (res: got.Response<any>) => {
  const cookies = new CookieJar();
  let cookieHeaders = res.headers['set-cookie'];
  cookieHeaders = cookieHeaders && cookieHeaders.length ? cookieHeaders : [];

  await Promise.all(
    cookieHeaders.map(async (value: string) => {
      return promisify(cookies.setCookie.bind(cookies))(
        Cookie.parse(value),
        synergyUrl,
      );
    }),
  );

  return cookies;
};

const synergyLogin = async (
  username: string,
  password: string,
  { print }: { print: GluegunPrint },
) => {
  const loginFormPath = 'group/intranet/home';

  const loginPageResp = await got(`${synergyUrl}/${loginFormPath}`, {
    followRedirect: true,
  });
  const loginPage = cheerio.load(loginPageResp.body);
  const cookies = await collectCookies(loginPageResp);

  const formAction = loginPage('form.sign-in-form')[0].attribs.action;
  const formActionUrl = new URL(formAction);
  const csrfHeader = {
    name: 'p_auth',
    value: formActionUrl.searchParams.get('p_auth'),
  };
  formActionUrl.searchParams.delete('p_auth');
  const formData = Array.from(loginPage('form.sign-in-form input'))
    .map(inp => {
      const { name } = inp.attribs;
      let { value } = inp.attribs;

      if (/password/.test(name)) {
        value = password;
      }

      if (/_login$/.test(name)) {
        value = username;
      }

      if (/rememberme/i.test(name)) {
        value = 'on';
      }

      if (/redirect/i.test(name)) {
        value = '';
      }

      return { name, value };
    })
    .reduce((accum, inp) => {
      accum.append(inp.name, inp.value);

      return accum;
    }, new FormData());

  formData.append(csrfHeader.name, csrfHeader.value);

  const loggedInResp = await got(formActionUrl.href, {
    followRedirect: true,
    method: 'POST',
    body: formData,
    cookieJar: cookies,
    throwHttpErrors: false,
    headers: {
      ...baseHeaders,
    },
  });
  const loggedInPage = cheerio.load(loggedInResp.body);

  if (
    loggedInPage('.btn-primary')
      .text()
      .match(/sign in/gi)
  ) {
    print.error('Login Failed.');
  } else {
    print.success('Login successfull');
  }

  return collectCookies(loggedInResp);
};

const searchEmployees = async ({ cookieJar }: { cookieJar: CookieJar }) => {
  const searchPageUrl =
    'http://synergy.trantorinc.com/group/intranet/employee-directory';
  const searchPageResp = await got(searchPageUrl, { cookieJar });
  const searchPage = cheerio.load(searchPageResp.body);
  const searchNamespace = searchPage('#namespaceId').val();
  const searchUrl = new URL(searchPageUrl);
  searchUrl.searchParams.set(
    'p_p_id',
    searchNamespace.substr(1, searchNamespace.length - 2),
  );
  searchUrl.searchParams.set('p_p_lifecycle', '2');
  searchUrl.searchParams.set('p_p_state', 'normal');
  searchUrl.searchParams.set('p_p_mode', 'view');
  searchUrl.searchParams.set(`${searchNamespace}lastName`, '');
  searchUrl.searchParams.set(`${searchNamespace}firstName`, '');
  searchUrl.searchParams.set(`${searchNamespace}emailAddress`, '@');

  try {
    const employeesResp = await got(searchUrl.href, {
      cookieJar,
    });

    return JSON.parse(employeesResp.body);
  } catch {
    throw new Error('Unable to search for employees');
  }
};

const emitDiscoveredEmployee = employee => {
  const publishEventUrl = `${eiUrl}/events/DISCOVERED_EMPLOYEE`;

  return got(publishEventUrl, {
    method: 'POST',
    body: {
      version: 'v1',
      payload: {
        email: employee.emailAddress,
        name: `${employee.firstName} ${employee.lastName}`,
        designation: employee.designation,
        skypeId: employee.skypeId,
      },
    },
    json: true,
  });
};

const asyncTimeout = (time: number) =>
  new Promise((resolve, reject) => {
    setTimeout(resolve, time);
  });

module.exports = {
  name: 'discover-employees',
  alias: 'de',
  run: async (toolbox: GluegunToolbox) => {
    const { print, prompt } = toolbox;

    print.info('Will search for employees via Trantor Synergy Portal.');
    print.info('Please provide your Trantor Synergy Credentials: ');

    const { username } = await prompt.ask({
      type: 'input',
      name: 'username',
      message: 'Username:',
    });
    const { password } = await prompt.ask({
      type: 'password',
      name: 'password',
      message: 'Password:',
    });

    print.info(`Attempting Login to Synergy as ${username}`);
    const cookieJar = await synergyLogin(username, password, { print });

    let employees = { data: [] };
    try {
      employees = await searchEmployees({ cookieJar });
    } catch (err) {
      print.error(`Failed to fetch employees: ${err.message}`);
      return;
    }

    print.success(
      `Discovered ${employees.data.length} employees. Emitting events`,
    );
    let remaining = employees.data.length;
    const spinner = print.spin(
      `Discovered ${remaining} employees. Emitting events`,
    );
    for (const emp of employees.data) {
      try {
        await emitDiscoveredEmployee(emp);
        await asyncTimeout(50);
        spinner.text = print.colors.cyan(`${remaining} remaining`);
        remaining -= 1;
      } catch (err) {
        print.error(`Failed to emit event for employee: ${emp.emailAddress}`);
      }
    }
    spinner.succeed('Done!');
  },
};
