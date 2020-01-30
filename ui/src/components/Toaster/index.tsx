import React from 'react';
import { Header, List, Segment, Transition } from 'semantic-ui-react';

export interface ToastDataProps {
  key: string;
  header: string;
  body?: string;
}

const Toast: React.FC<ToastDataProps> = (p) => {
  return (
    <List.Item key={p.key}>
      <Header>{p.header}</Header>
      <p>{p.body}</p>
    </List.Item>
  );
};

export interface ToasterDataProps {
  toasts: ToastDataProps[];
}

const Toaster: React.FC<ToasterDataProps> = (p) => {
  const isVisible = Boolean(p.toasts.length);

  return (
    <Segment style={{ display: isVisible ? 'block' : 'none' }}>
      <Transition.Group as={List} divided={true} animation='fly up'>
        {p.toasts.map(Toast)}
      </Transition.Group>
    </Segment>
  );
};

export default Toaster;
