import { Dayjs } from 'dayjs';
import React, { FC, Fragment } from 'react';
import { Button, Header, Icon, Modal, Popup, Segment } from 'semantic-ui-react';
import { Employee } from 'src/ducks/employees';
import { Standup } from 'src/ducks/standup';

export interface Report {
  employee: Employee;
  yesterday?: Standup;
  today?: Standup;
  impediment?: Standup;
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

const RenderReport: FC<{ report: Report }> = ({ report: r }) => {
  const yesterday = r.yesterday?.standup && (
    <Fragment>
      <Header as="h3" attached="top">
        Yesterday ({r.yesterday?.date.format('dddd, MMM DD')})
      </Header>
      <Segment attached={true}>{r.yesterday?.standup}</Segment>
    </Fragment>
  );
  const today = r.today?.standup && (
    <Fragment>
      <Header as="h3" attached="top">
        Today ({r.today?.date.format('dddd, MMM DD')})
      </Header>
      <Segment attached={true}>{r.today?.standup}</Segment>
    </Fragment>
  );
  const impediment = r.impediment?.standup && (
    <Fragment>
      <Header as="h3" attached="top">
        Impediments ({r.impediment?.date.format('dddd, MMM DD')})
      </Header>
      <Segment attached={true}>{r.impediment?.standup}</Segment>
    </Fragment>
  );

  return (
    <div key={r.employee.ecode}>
      <Header as="h2" attached="top">
        {r.employee.name}
      </Header>
      <Segment attached={true}>
        {yesterday}
        {today}
        {impediment}
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
