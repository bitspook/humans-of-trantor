import { GluegunToolbox } from 'gluegun';
import { promisify } from 'util';
import { DatabaseConnectionType, sql } from 'slonik';

const eventName = 'RECEIVED_STANDUP_UPDATE';
const eventVersion = 'v1';

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

    const areEmployeesValid = employeesToUpdate.every(
      emp => emp.ecode && emp.sheetName && emp.project,
    );

    if (!areEmployeesValid) {
      print.error(
        'All standup employees should have required fields: ecode, sheetName, project',
      );
      return -1;
    }

    const db: DatabaseConnectionType = await toolbox.hotCloud().db();

    print.colors.muted('Authorizing with google sheets...');
    const sheets = await toolbox.googleSheet();

    const getRows = (...args) =>
      promisify(sheets.spreadsheets.values.get.bind(sheets))(...args).then(
        (v: any) => v.data.values,
      );

    for (const emp of employeesToUpdate) {
      const spinnerPermaText = `Working for ${emp.sheetName}`;

      const spinner = print.spin(
        `${spinnerPermaText}: Requesting recorded standup updates`,
      );
      const standupUpdates = await db.many(sql`
        SELECT
          payload->>'date' AS date,
          payload->>'standup' AS standup
        FROM store.store
          WHERE name = ${eventName}
            AND version = ${eventVersion}
            AND payload->>'ecode' = ${emp.ecode}
            AND (payload->>'isEod')::boolean IS true
            AND payload->>'project' = ${emp.project}
            AND date_part('month', (payload->>'date')::Timestamp) = ${month}
        `);
      print.info(`\nFound ${standupUpdates.length} standup updates`);

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
