import { createPool } from 'slonik';

const dbUrl = 'postgres://hot:hot@store/hot';

export default async () => createPool(dbUrl);
