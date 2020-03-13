import React from 'react';
import c from './index.module.scss';
import { Standup } from 'src/ducks/standup';

interface StandupFieldDP {
  standup: Standup;
}

const StandupField = (p: StandupFieldDP) => {
  return (
    <div className={c.standupRow}>
      <div className={c.isDeliveredCheckbox}>
        <label
          className={p.standup.isDelivered ? c.checked : ''}
          htmlFor="standup-is-delivered"></label>
        <input name="standup-is-delivered" type="checkbox" defaultChecked={p.standup.isDelivered} />
      </div>

      <input type="text" value={p.standup.standup} />
    </div>
  );
};

interface DataProps {
  ecode: string;
  standups: Standup[];
}

interface CbProps {
  onSave: (standup: Standup) => void;
}

const StandupForm: React.FC<CbProps & DataProps> = (p) => {
  return (
    <div>
      {p.standups.map((s) => (
        <StandupField standup={s} />
      ))}
    </div>
  );
};

export default StandupForm;
