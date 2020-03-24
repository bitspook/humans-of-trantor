import { Dayjs } from 'dayjs';
import React, { FC, Fragment } from 'react';
import { Button, Header, Icon, Modal, Popup, Segment } from 'semantic-ui-react';
import { Employee } from 'src/ducks/employees';
import { Standup } from 'src/ducks/standup';

export interface Report {
  employee: Employee;
  yesterday?: Standup[];
  today?: Standup[];
}

interface ReportDataProps {
  day: Dayjs;
  reports: Report[];
  isOpen: boolean;
}

interface ReportCbProps {
  onOpen: () => void;
  onClose: () => void;
}

const StandupReportRow = (s: Standup) => (
  <Segment attached={true} key={s.id}>
    <b>{String.fromCharCode(s.isDelivered ? 9989 : 10066)} </b>
    <span> {s.standup}</span>
  </Segment>
);

const RenderReport: FC<{ report: Report }> = ({ report: r }) => {
  const yesterday =
    r.yesterday && r.yesterday.length ? (
      <Fragment>
        <Header as="h3" attached="top">
          Yesterday ({r.yesterday[0].date.format('dddd, MMM DD')})
        </Header>
        {r.yesterday.map(StandupReportRow)}
      </Fragment>
    ) : null;

  const today =
    r.today && r.today.length ? (
      <Fragment>
        <Header as="h3" attached="top">
          Today ({r.today[0].date.format('dddd, MMM DD')})
        </Header>
        {r.today.map(StandupReportRow)}
      </Fragment>
    ) : null;

  return (
    <div key={r.employee.ecode}>
      <Header as="h2" attached="top">
        {r.employee.name}
      </Header>
      <Segment attached={true}>
        {yesterday}
        {today}
      </Segment>
    </div>
  );
};

const ReportButton: FC<ReportDataProps & ReportCbProps> = (p) => {
  let modalContent = p.reports.map((r) => (
    <Fragment key={r.employee.ecode}>
      <RenderReport report={r} />
      <br />
    </Fragment>
  ));

  if (!modalContent.length) {
    modalContent = [
      <Header as="h2" key={1} disabled={true}>
        <Icon name="user" circular={true} />
        <Header.Content>
          No Standup to report
          <Header.Subheader>Please select employees to create report for</Header.Subheader>
        </Header.Content>
      </Header>,
    ];
  }

  return (
    <section>
      {/* prettier-ignore */}
      <Popup
        content={<p>Report for <b>{p.day.format('dddd, MMM DD')}</b></p>}
        position="bottom right"
        trigger={<Button content="Report" icon="download" labelPosition="left" onClick={p.onOpen} />}
      />

      <Modal dimmer={true} open={p.isOpen} onClose={p.onClose}>
        <Modal.Content contentEditable={true}>{modalContent}</Modal.Content>
      </Modal>
    </section>
  );
};

export default ReportButton;
