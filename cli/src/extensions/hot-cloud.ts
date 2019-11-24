import { GluegunToolbox } from 'gluegun';
import { createPool, DatabasePoolType } from 'slonik';

let dbConnPool: DatabasePoolType | undefined;

const getDbConnPool = async (dbUrl: string) => {
  if (dbConnPool) {
    return dbConnPool;
  }

  dbConnPool = await createPool(dbUrl);

  return dbConnPool;
};

module.exports = async (toolbox: GluegunToolbox) => {
  const { print, config } = toolbox;

  toolbox.hotCloud = () => {
    if (!config.hotCloud) {
      print.error('Please add `hotCloud` config to your .hotrc');

      return -1;
    }

    return {
      db: async () => {
        if (!config.hotCloud.db || !config.hotCloud.db.connectionString) {
          print.error(
            'Please add `hotCloud.db.connectionString` config to your .hotrc',
          );

          return -1;
        }

        return getDbConnPool(config.hotCloud.db.connectionString);
      },
    };
  };
};
