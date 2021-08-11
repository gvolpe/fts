fts (full-text search)
======================

[![CI Status](https://github.com/gvolpe/fts/workflows/Haskell%20CI/badge.svg)](https://github.com/gvolpe/fts/actions)

Full-text search demo powered by PostgreSQL, inspired by [this article](https://blog.crunchydata.com/blog/postgres-full-text-search-a-search-engine-in-a-database).

## Overview

There are two components in this project: the dataset loader and the full-text search console application.

### Dataset loader

The loader is a nix shell script interpreted by [Ammonite](http://ammonite.io/), written in Scala, which reads a bunch of CSV files, parses its content, and it stores them in Postgres. These tasks are made easy by [fs2](https://fs2.io), [fs2-data](https://github.com/satabin/fs2-data), and [skunk](https://github.com/tpolecat/skunk).

Before proceeding, we need a PostgreSQL instance up and running. You can use the supplied [docker-compose.yml](./docker-compose.yml) file, or have your own instance running.

Once the requirements are met, we can run the script in the following way via Nix.

```shell
$ cd data
$ ./Loader.sc
```

If you wish to change the Postgres connection details, have a look at the [configuration file](data/DB.sc).

### Full-text search console app

In the console app, you can search movies by title.

```shell
$ cabal new-run fts
```

![screenshot](img/fts.png)

Hit `Ctrl + C` to exit.

## Dataset

The movies dataset is licensed under [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/), downloaded from [here](https://www.kaggle.com/stefanoleone992/imdb-extensive-dataset).
