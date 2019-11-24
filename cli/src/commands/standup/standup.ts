import { GluegunToolbox } from 'gluegun';

module.exports = {
  name: 'standup',
  description: 'Interact with Scrum Standup meeting related stuff',
  run: async (toolbox: GluegunToolbox) => {
    toolbox.print.printHelp(toolbox);
  },
};
