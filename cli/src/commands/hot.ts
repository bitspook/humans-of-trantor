import { GluegunToolbox } from 'gluegun';

module.exports = {
  name: 'hot',
  description: 'Top level command. Doesn\'t do anything.',
  run: async (toolbox: GluegunToolbox) => {
    const { print } = toolbox;

    toolbox.config.loadConfig('hot');

    print.printHelp(toolbox);
  },
};
