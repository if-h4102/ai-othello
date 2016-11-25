# Contributing

## Filenames

Avoid uppercase characters. Separate words with dashes `-`.

Example: `end-of-game.pl`

Exceptions:

- Required capitalization, for example for Java files.
- GNU standard files: `README`, `LICENSE`, `CHANGELOG`, `CONTRIBUTING`, etc.

## Modules

This section describes the syntax to use modules. Modules allow to import and
export predicates.

You can check the examples in the `examples/modules` directory.

### Export

Start every file with a call to the the `module/2` predicate:

**my-module.pl**
```prolog
% module(namespace, listOfExportedPredicates)
:- module(myModule, [foo/1, bar/2]).

% Exported
foo(X) :- foo(X, X).

% Private
foo(X, Y) :- X < Y.

% Exported
bar(X, Y) :- X > Y.
```

### Import

Add calls to the `use_module/2` predicate at the beggining of your file to
import namespaces. The first argument is the relative _path_ (without the
extension) to the imported module, but you will get _namespace_.

**main.pl**
```prolog
:- module(main, []).

% Import the namespace defined in ./my-module.pl
:- use_module(my-module, []).

% Usage:
:- myModule:foo(2)
```
