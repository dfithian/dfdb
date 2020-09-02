# DFDB

A simple database created for the purposes of learning database architecture.

## Usage

```
$ stack run dfdb
Welcome to DFDB

Enter "help" to get this text
  Quit commands: :q, quit(), exit

dfdb > create table foo (string string, int int, bool bool,);
CREATE TABLE
dfdb > insert ('string', 1, true,) foo;
INSERT 1
dfdb > insert (1, 2, 3,) foo;
Column string(1) is not a string
dfdb > select (string, int,) foo;
["string",1]

dfdb > :q
```

## Features

- [x] create table
- [x] insert
- [x] select
- [ ] update
- [ ] nullable/non-nullable columns
- [x] primitive types
- [ ] persist to disk
- [ ] primary keys
- [ ] foreign keys
- [ ] indexes
- [ ] query planning
