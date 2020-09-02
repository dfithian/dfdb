# DFDB

A simple database created for the purposes of learning database architecture.

## Usage

```
$ stack run dfdb
Welcome to DFDB

Enter "help" to get this text
  Quit commands: :q, quit(), exit

dfdb > create table foo (a, b, c,);
CREATE TABLE
dfdb > insert ('string', 1, true,) foo;
INSERT 1
dfdb > select (b,) foo;
[1]

dfdb > :q
```
