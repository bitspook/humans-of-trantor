import { GluegunToolbox } from 'gluegun';

module.exports = {
  name: 'hot',
  run: async (toolbox: GluegunToolbox) => {
    const { print } = toolbox;

    print.info('Welcome to Humans Of Trantor');
  },
};
