import { GluegunToolbox } from 'gluegun';
import got from 'got';
import 'global-agent/bootstrap';

const emitDiscoveredEmployee = (employee: any, eiUrl: string) => {
  const publishEventUrl = `${eiUrl}/events/DISCOVERED_EMPLOYEE`;

  return got(publishEventUrl, {
    method: 'POST',
    headers: {
      'content-type': 'application/json',
    },
    body: JSON.stringify({
      version: 'v1',
      payload: employee,
    }),
  });
};

const asyncTimeout = (time: number) =>
  new Promise(resolve => {
    setTimeout(resolve, time);
  });

module.exports = {
  name: 'import-employees',
  description: `Import employees from a .json file. JSON file must be in format:
    [{
      email: string
      name: string
      ecode: string
      designation?: string
      skypeId?: string
      project?: string
    }]
  `,
  run: async (toolbox: GluegunToolbox) => {
    const {
      print,
      config,
      filesystem: fs,
      parameters: { options },
    } = toolbox;

    const empFile = options.file;
    let employees = [];

    if (!empFile || !fs.isFile(empFile)) {
      print.error('--file must provide valid JSON file');
      return -1;
    }

    const eiUrl =
      config.hotCloud && config.hotCloud.urls && config.hotCloud.urls.ei;
    if (!eiUrl) {
      print.error('Please add `hotCloud.urls.ei` to your .hotrc');

      return -1;
    }

    try {
      employees = JSON.parse(fs.read(empFile));
    } catch (err) {
      print.error('--file must provide valid JSON file');

      return -1;
    }

    print.success(`Importing ${employees.length} employees. Emitting events`);
    let remaining = employees.length;
    const spinner = print.spin(
      `Importing ${remaining} employees. Emitting events`,
    );
    for (const emp of employees) {
      try {
        await emitDiscoveredEmployee(emp, eiUrl);
        await asyncTimeout(50);
        spinner.text = print.colors.cyan(`${remaining} remaining`);
        remaining -= 1;
      } catch (err) {
        print.error(`Failed to emit event for employee: ${emp.name} (${emp.ecode})`);
      }
    }
    spinner.succeed('Done!');
  },
};
