# Following environment variables must be provided when calling this script:
# PGHOST
# PGUSER
# PGPASS
# PGDATABASE
echo "*:*:*:*:$PGPASS" > ~/.pgpass
chmod 600 ~/.pgpass
createdb -w
rm ~/.pgpass
