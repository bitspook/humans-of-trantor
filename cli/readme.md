# hot-cli CLI

A CLI for hot-cli.

> Notice: I am thinking about getting rid of this Typescript based CLI, and make HoT CLI a part of
> hot-core instead. i.e HoT core will expose an API server, as well as CLI based UI to interact with
> the cloud.

## Notable Commands

### Import Employees

`hot import-employees` is created because it has become too much of a pain to scrap synergy.
Besides, synergy don't seem to have much higher quality data. Perhaps we can find a better source.
For instance, employees found on synergy have their names obtained by splitting email addresses of
employees. So there are employees like "Gurdeep Singh1"; to make it worse, Synergy people chose to
split the names in 2 pieces. So there are employees with half their name missing (e.g "Sandeep Kumar
Singh" becomes "Sandeep Singh" on synergy).

For importing employees, employee name, email and ecode is required. We can get email from synergy
(and other metadata e.g project), but we need to refer attendance sheet to get the ecode. Given the
nature of data, I chose to discontinue `hot discover-employees`, and instead chose to create
`import-employees` instead.

To work with this command:

1. Open synergy, open network panel, search for employees, copy the response to a variable named `emp`.
2. Run following snippet in console to format the response
   ```
   emp = emp.map(e => ({
     name: `${e.firstName} ${e.lastName}`,
     email: e.emailAddress,
     project: e.projectName,
     designation: e.designation,
     skypeId: e.skypeId
   })))
   ```

3. Open Attendance sheet, select and copy name and ecode columns; and convert them to JSON of given
   shape using something like [csv2json.com](https://csvjson.com/csv2json):
   ```
   { ecode: string, name: string }
   ```
   and save it to a variable named `att`
4. Copy data from #3 to same console as #2, and run
   ```
   att.map((a) => {
  const sameNameEmps = emps.filter((e) => {
    const n1 = e.name.toLowerCase().split(' ').join('');
    const n2 = a.name.toLowerCase().split(' ').join('');

    // Both synergy and timesheet has some names incomplete
    return n1.indexOf(n2) > -1;
  });

  if (sameNameEmps.length > 1) {
    console.warn('Duplicates', sameNameEmps);
  }

  if (sameNameEmps.length < 1) {
    console.warn('Not Found', a);
  }

  const emp = sameNameEmps.length ? sameNameEmps[0] : a;

  return {
    ...emp,
    ...a
  }
})
   ```
5. Copy this array, and paste it to a file that should be passed as argument to `hot import-employees`

Snippet in #4 isn't fool-proof and misses more results than it should, but it is serving my very
immediate purpose of getting HoT system up for Veriown team. At some point, I'll probably create a
Jupyter notebook for data mining.
