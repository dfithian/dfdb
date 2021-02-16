# DFDB

A simple database created for the purposes of learning database architecture.

## Usage

```bash
$ stack run dfdb
Welcome to DFDB

Enter "help" to get this text
  Quit commands: :q, quit(), exit

dfdb > create table foo (string string, int int, bool bool,);
CREATE TABLE
dfdb > insert ('string', 1, true,) foo;
INSERT 1
dfdb > select (string, int,) foo where (int = 1,);
["string",1]

dfdb > create index foo_int foo (int,);
CREATE INDEX
dfdb > :q

$ stack run dfdb
Welcome to DFDB

Enter "help" to get this text
  Quit commands: :q, quit(), exit

dfdb > select (string, int,) foo where (int = 1,);
["string",1]
dfdb > drop table foo;
DROP TABLE
dfdb > select (string, int,) foo;
Table does not exist

dfdb > :q
```

## Internals

* If you want to start from scratch, `rm -rf .dfdb`

## Features

- [x] create, drop table
- [x] insert
- [x] select
- [ ] update
- [ ] nullable/non-nullable columns
- [x] primitive types
- [x] persist to disk
- [x] primary keys
- [ ] foreign keys
- [x] indexes
- [x] query planning
- [x] autocommit transactions
- [x] transactions

## Benchmarks

For info about benchmarks, see [benchmark.pdf](benchmark.pdf).
