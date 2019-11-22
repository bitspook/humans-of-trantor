---
title: "Store Migrations"
date: 2019-09-20T08:06:59Z
draft: false
weight: 15
---

Store uses postgresql database. The database schema is written in PG/psql and schema changes are
maintained with [migrate utility](https://github.com/golang-migrate/migrate). `migrate` cli is
present in the store container, and can be used to perform migrations.

1. **Create a new Migration**<br />
    {{< highlight sh >}}
    docker-compose exec store ash -lc "mcreate <migration-title>"
    {{< /highlight >}}

2. **Migrate to latest**
    {{< highlight sh >}}
    docker-compose exec store migrate -path ./migrations -database postgres://hot:hot@store/hot?sslmode=disable up"
    {{< /highlight >}}

{{% notice tip %}}
You can change the version of migrate CLI by setting $MIGRATE_VERSION environment variable for `store` in docker-compose.
{{% /notice %}}
