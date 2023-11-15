# Python Bindings for beancount-parser-lima

The Python bindings are a simplification of the Rust interface, mainly because zero-copy is not possible, and all directives are returned by value.

*Python bindings are a work-in-progress*

## Performance

The main downside with Python compared with Rust is that zero-copy is not possible.  Each value returned to Python must be a Python object allocated on the heap.  It is simply not possible to return pointers to data structures owned by Rust.  The performance goal therefore is to minimize the number of such allocations.

Several mechanisms for reducing allocations are employed.

1. Strings are interned and those having the same value are mapped onto a single Python string object.

2. The lists which hold account names are also reused.

### Example

```
2023-05-01 * "EMERSON S TAPROOM"
  Assets:Bank:Current                           -25.00 NZD
  Expenses:Entertainment:Drinks-and-snacks

2023-05-02 * "EMERSON S TAPROOM"
  Assets:Bank:Current                           -12.50 NZD
  Expenses:Entertainment:Drinks-and-snacks

2023-05-03 * "BANK TRANSFER"
  Assets:Bank:Current                           100.00 NZD
  Assets:Bank:Savings

```

Only a single Python string to hold the currency `"NZD"` is allocated.  It will have a reference count of four (One for each value returned, plus one for the master value in the string table).

Similarly only a single Python string is used to hold the four instances of `"Assets"` (with a reference count of five), and a single Python string holds the value `"Bank"` (also with a reference count of five).

A single list is allocated with contents `["Assets", "Bank", "Current"]`, and this has a reference count of four (three returned values plus the master copy in the account table).  A separate list has the contents `["Assets", "Bank", "Savings"]`, using the same Python string objects as the other list for the first two values in the list.

## Development Shell

If you have a Rust toolchain and other required tools such as `maturin` installed (for which see `flake.nix`), the parser may be built and run locally as follows.

```
$ cd beancount-parser-lima-python
$ python -m venv .venv
$ maturin develop
$ source .venv/bin/activate
$ python-examples/parse.py ../beancount-parser-lima/examples/data/full.beancount
```

## Open Issues and Future Work

### Spanned

It would be desirable to be able to return spanned values, for application error reporting.
