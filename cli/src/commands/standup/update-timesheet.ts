import { GluegunToolbox } from 'gluegun';
import { promisify } from 'util';

module.exports = {
  name: 'update-timesheet',
  alias: 'ut',
  description:
    'Update the timesheet with standup updates recorded for given month',
  run: async (toolbox: GluegunToolbox) => {
    const {
      parameters: { options },
      print,
      config,
    } = toolbox;

    const month = parseInt(options.month, 10) || 12;

    if (month <= 0 && month > 12) {
      print.error(`--month should be a number between 1 and 12`);
      return -1;
    }

    const allSheetIds = config.timesheet && config.timesheet.monthlySheetIds;

    if (!allSheetIds) {
      print.error('Please add timesheet configuration to .hotrc file');
      return -1;
    }

    const spreadsheetId = allSheetIds[month];

    if (!spreadsheetId) {
      print.error(`Please add timesheet ID for month: ${month}`);
      return -1;
    }

    const employeesToUpdate =
      config.timesheet && config.timesheet.employeesToUpdate;
    if (!employeesToUpdate || !employeesToUpdate.length) {
      print.error(
        'Please add timesheet configuration for employeesToUpdate to .hotrc file',
      );
      return -1;
    }

    print.colors.muted('Accessing google sheets...');
    const sheets = await toolbox.googleSheet();

    const getRows = (...args) =>
      promisify(sheets.spreadsheets.values.get.bind(sheets))(...args).then(
        (v: any) => v.data.values,
      );

    for (const emp of employeesToUpdate) {
      const spinnerPermaText = `Working for ${emp.sheetName}`;
      const spinner = print.spin(spinnerPermaText);

      spinner.text = `${spinnerPermaText}: Obtaining sheet headers`;
      const [headers] = await getRows({
        spreadsheetId,
        range: `${emp.sheetName}!A1:Z1`,
      });

      const dateCol = String.fromCharCode(
        65 + headers.findIndex((h: any) => /date/i.test(h)),
      );
      const taskCol = String.fromCharCode(
        65 + headers.findIndex((h: any) => /task/i.test(h)),
      );
      const isBillableCol = String.fromCharCode(
        65 + headers.findIndex((h: any) => /type/i.test(h)),
      );
      const hoursCol = String.fromCharCode(
        65 + headers.findIndex((h: any) => /hours/i.test(h)),
      );

      spinner.text = `${spinnerPermaText}: Found columns: ${dateCol} ${taskCol} ${isBillableCol} ${hoursCol}`;

      spinner.succeed(`${spinnerPermaText}: Done!`);
    }
  },
};
