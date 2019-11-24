import { GluegunToolbox } from 'gluegun';
import { promisify } from 'util';
import { DatabaseConnectionType, sql } from 'slonik';
const fecha = require('fecha');

const { format } = fecha;

const eventName = 'RECEIVED_STANDUP_UPDATE';
const eventVersion = 'v1';

interface StandupEmployee {
  ecode: string;
  sheetName: string;
  project: string;
  hoursSpent: string;
}

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
      emp => emp.ecode && emp.sheetName && emp.project && emp.hoursSpent,
    );

    if (!areEmployeesValid) {
      print.error(
        'All standup employees should have required fields: ecode, sheetName, project, hoursSpent',
      );
      return -1;
    }

    const db: DatabaseConnectionType = await toolbox.hotCloud().db();

    print.colors.muted('Authorizing with google sheets...');
    const sheets = await toolbox.googleSheet();

    const getSheetRows = (...args) =>
      promisify(sheets.spreadsheets.values.get.bind(sheets))(...args).then(
        (v: any) => v.data.values,
      );
    const updateSheet = promisify(
      sheets.spreadsheets.values.update.bind(sheets),
    );

    for (const employee of employeesToUpdate) {
      const spinnerPermaText = `[${employee.sheetName}]`;

      let spinner = print.spin(
        `${spinnerPermaText}: Requesting recorded standup updates`,
      );
      const standupRows = await db.many(sql`
        SELECT
          payload->>'date' AS date,
          payload->>'standup' AS standup
        FROM store.store
          WHERE name = ${eventName}
            AND version = ${eventVersion}
            AND payload->>'ecode' = ${employee.ecode}
            AND (payload->>'isEod')::boolean IS true
            AND payload->>'project' = ${employee.project}
            AND date_part('month', (payload->>'date')::Timestamp) = ${month}
        `);
      spinner.succeed(
        `${spinnerPermaText}: Found ${standupRows.length} updates`,
      );

      spinner = print.spin(`${spinnerPermaText}: Downloading timesheet`);
      const sheetRows = await getSheetRows({
        spreadsheetId,
        range: `${employee.sheetName}!A1:Z50`,
      });
      spinner.succeed(`${spinnerPermaText}: Downloaded timesheet!`);

      spinner = print.spin(
        `${spinnerPermaText}: Preparing payload to update standup sheet`,
      );
      const sheetUpdatePayload = buildGSheetUpdatePayload({
        toolbox,
        sheetRows,
        standupRows,
        employee,
      });

      spinner.succeed(`${spinnerPermaText}: Timesheet update payload ready!`);

      print.table(
        [
          sheetRows[0],
          ...sheetUpdatePayload
            .map((r, ri) => {
              const row = sheetRows[0].map((x, i) =>
                /s.*no/i.test(x) ? ri : r[i] || '',
              );

              // print only those rows which have a new value to update
              return r.length ? row : [];
            })
            .filter(r => r.join('')),
        ],
        { format: 'lean', border: true },
      );

      spinner = print.spin(`${spinnerPermaText}: Updating timesheet`);
      updateSheet({
        spreadsheetId,
        range: `${employee.sheetName}!A1:Z50`,
        valueInputOption: 'RAW',
        resource: { values: sheetUpdatePayload },
      });

      spinner.succeed(`${spinnerPermaText}: Timesheet updated!`);
    }
  },
};

const buildGSheetUpdatePayload = ({
  toolbox,
  sheetRows,
  standupRows,
  employee,
}: {
  toolbox: GluegunToolbox
  sheetRows: any[]
  standupRows: any[]
  employee: StandupEmployee,
}) => {
  const headers = sheetRows[0];

  const dateColIndex = headers.findIndex((h: any) => /date/i.test(h));
  const taskColIndex = headers.findIndex((h: any) => /task/i.test(h));
  const isBillableColIndex = headers.findIndex((h: any) => /type/i.test(h));
  const hoursColIndex = headers.findIndex((h: any) => /hours/i.test(h));

  const values = sheetRows.map((cols, index) => {
    if (index === 0) {
      // do nothing for header row
      return [];
    }

    const date = new Date(cols[dateColIndex]);

    if (date.toString() === 'Invalid Date') {
      toolbox.print.colors.warning(
        `\nInvalid date: ${date} found in timesheet for row: ${index + 1}`,
      );
      return [];
    }

    const standup = standupRows.find(
      ({ date: d }) => d === format(date, 'YYYY-MM-DD'),
    );

    if (!standup) {
      toolbox.print.colors.dim(`Could not find standup for date: ${date}`);

      return [];
    }

    const valuesRow = [];
    valuesRow[taskColIndex] = cols[taskColIndex] ? null : standup.standup;
    valuesRow[isBillableColIndex] = cols[isBillableColIndex] ? null : 'Billable';
    valuesRow[hoursColIndex] = cols[hoursColIndex] ? null : employee.hoursSpent;

    return valuesRow;
  });

  return values;
};
