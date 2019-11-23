import { GluegunToolbox } from 'gluegun';

module.exports = {
  name: 'generate',
  alias: ['g'],
  run: async (toolbox: GluegunToolbox) => {
    const {
      parameters,
      template: { generate },
      print: { info, error },
      strings: { pascalCase },
    } = toolbox;

    const type = parameters.first;

    if (type !== 'event') {
      error('Invalid generate request. \nValid values: event');
      return -1;
    }

    const eventName = parameters.second;

    if (!eventName) {
      error('Event name is required. \ne.g hot g event MY_NEW_EVENT');
      return -1;
    }

    const className = pascalCase(eventName);

    await generate({
      template: 'event.ts.ejs',
      target: `${__dirname}/../../../ei/src/events/${className}.ts`,
      props: { eventName, className },
    });

    info(`Generated file at /hot/ei/src/events/${className}.ts`);
  },
};
