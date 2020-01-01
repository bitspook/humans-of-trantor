import dayjs, { Dayjs } from 'dayjs';
import React from 'react';
import classNames from 'classnames';
import { List } from 'semantic-ui-react';
import { Standup } from 'src/ducks/standup';
import c from './index.module.scss';
import CalendarIcon from 'src/components/CalendarIcon';

interface CalendarListItemProps {
  day: Dayjs;
  onClick: (d: Dayjs) => void;
  isHighlighted: boolean;
}

const CalendarListItem = ({ day, onClick, isHighlighted }: CalendarListItemProps) => {
  const isWeekend = day.day() === 0 || day.day() === 6;
  const handleClick = () => onClick(day);

  return (
    <List.Item 
      onClick={handleClick} 
      active={isHighlighted} 
      className={classNames({
        [c.calendarItem]: true,
        [c.disabled]: isWeekend
      })}
    >
      <CalendarIcon day={day} className={c.icon}/>
      <List.Content className={c.calendarDetails}>
        <List.Header>{day.format('dddd')}</List.Header>
        <List.Description>{day.format('MMM DD, YYYY')}</List.Description>
      </List.Content>
    </List.Item>
  );
};

interface DataProps {
  standup: Standup[];
  selectedDay: Dayjs;
}

interface CbProps {
  onSelect: (e: Dayjs) => void;
}

const calendarDays = () => {
  const today = dayjs(new Date());

  const calDays = [];

  for (let i = 0; i < 31; i += 1) {
    calDays.push(today.subtract(i, 'day'));
  }

  return calDays;
};

const StandupCalendar: React.FC<DataProps & CbProps> = (p) => {
  const days = calendarDays().map((d) => (
    <CalendarListItem
      day={d}
      key={d.unix()}
      onClick={p.onSelect}
      isHighlighted={d.isSame(p.selectedDay, 'day')}
    />
  ));

  return (
    <List selection={true} relaxed={true} divided={true} verticalAlign='middle' size='medium'>
      {days}
    </List>
  );
};

export default StandupCalendar;
