import { Dayjs } from 'dayjs';
import React from 'react';
import { List } from 'semantic-ui-react';
import c from './index.module.scss';

interface CalendarListItemProps {
  day: Dayjs;
  onClick: (d: Dayjs) => void;
  isHighlighted: boolean;
}

const CalendarListItem = ({ day, onClick, isHighlighted }: CalendarListItemProps) => {
  const isWeekend = day.day() === 0 || day.day() === 6;

  return (
    <List.Item
      onClick={() => onClick(day)}
      active={isHighlighted}
      className={isWeekend ? c.disabled : ''}
    >
      <List.Content>
        <List.Header>{day.format('dddd')}</List.Header>
        <List.Description>{day.format('MMM DD, YYYY')}</List.Description>
      </List.Content>
    </List.Item>
  );
};

interface DataProps {
  days: Dayjs[];
  selectedDay: Dayjs;
}

interface CbProps {
  onSelect: (e: Dayjs) => void;
}

const StandupCalendarList: React.FC<DataProps & CbProps> = (p) => {
  const days = p.days.map((d) => (
    <CalendarListItem
      day={d}
      key={d.unix()}
      onClick={p.onSelect}
      isHighlighted={d.isSame(p.selectedDay)}
    />
  ));

  return (
    <List selection={true} relaxed={true} divided={true} verticalAlign='middle' size='big'>
      {days}
    </List>
  );
};

export default StandupCalendarList;
