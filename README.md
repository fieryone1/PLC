# RDF Query Language

An interpreter for **RQL**, a small query language for RDF graphs, written in Haskell. The lexer is built with [Alex](https://haskell-alex.readthedocs.io/), the parser with [Happy](https://haskell-happy.readthedocs.io/), and Turtle files are read with [rdf4h](https://hackage.haskell.org/package/rdf4h).

An RQL program loads one or more Turtle (`.ttl`) files, combines and queries them, and prints the result as canonical N-Triples.

## Requirements

- [Stack](https://docs.haskellstack.org/) (the project pins Stackage snapshot `lts-21.25`)
- `rdf4h-5.2.2`, which Stack fetches automatically as an extra dependency
- Alex and Happy, which are declared as build tools and installed by Stack

## Build and run

```sh
cd plc-project
stack build
stack exec -- plc-project-exe <query-file.rql>
```

Example:

```sh
stack exec -- plc-project-exe t6.rql
```

`LOAD "name"` reads `name.ttl` from the **current working directory**, so run the executable from `plc-project/`.

## Tests

`tests/expected/` holds expected output for `t6.rql` to `t10.rql`. `run_tests.ps1` runs each program and compares its output with the matching `.out` file (PowerShell, tested on Windows):

```powershell
cd plc-project
./run_tests.ps1
```

On other platforms, run each program and diff it against the expected file, for example:

```sh
stack exec -- plc-project-exe t6.rql | diff - tests/expected/t6.out
```

`t1.rql` to `t5.rql`, `test.rql` and `testAnd.rql` are additional sample programs without stored expected output.

## Language overview

A program is a sequence of statements. There are two kinds:

```
name <- Expr        -- assign an expression's result (a graph) to a name
PRINT name          -- print a graph as N-Triples
```

Identifiers are alphanumeric and start with a letter. Keywords are uppercase. Comments start with `--` and run to the end of the line.

### Expressions

| Form | Meaning |
| --- | --- |
| `LOAD "file"` | Load `file.ttl` as a graph |
| `a UNION b` | Triples in either graph |
| `a INTERSECT b` | Triples in both graphs |
| `a MINUS b` | Triples in `a` but not in `b` |
| `SELECT (s, p, o) WHERE cond` | Build triples from the variable bindings that satisfy `cond` |
| `SELECT (...) WHERE cond GROUP BY ?v` | As above, grouped by `?v`, allowing aggregates |

Set operations take graph **names**, not nested expressions. Name intermediate results first.

### Terms

- Variables: `?x`, `?price`
- URIs: `<http://example.org/alice>`
- Strings: `"Bob"`
- Unsigned integers: `21`

### Conditions

- `MATCH(s, p, o) IN graph` unifies the pattern with every triple in `graph`. Variables bind, constants must match exactly.
- Comparisons: `?x = value`, `?x != value`, and `>=`, `<=`, `>`, `<` against an integer literal. Numeric comparisons are false for non-numeric values.
- Boolean operators `AND`, `OR`, `NOT` with `NOT` binding tightest, then `AND`, then `OR`. Parentheses group.
- Two `MATCH` patterns joined by `AND` that share a variable behave as a join.

### Aggregates

`MAX(?v)`, `MIN(?v)`, `SUM(?v)` and `COUNT(?v)` may appear in the `SELECT` output list and require a `GROUP BY` clause.

### Output

`PRINT` writes N-Triples sorted lexicographically on the rendered subject, then predicate, then object. Strings sort before integers, which sort before URIs. Duplicate triples are removed.

## Examples

Union of two graphs:

```
graph1 <- LOAD "foo"
graph2 <- LOAD "bar"
unionGraph <- graph1 UNION graph2
PRINT unionGraph
```

Filter with a numeric comparison:

```
baz <- LOAD "baz"
result <- SELECT (?s, <http://example.org/ont/hasAge>, ?o)
          WHERE MATCH (?s, <http://example.org/ont/hasAge>, ?o) IN baz
          AND ?o >= 21
PRINT result
```

Join across two graphs (`?y` is shared):

```
xyzzy <- LOAD "xyzzy"
plugh <- LOAD "plugh"
result <- SELECT (?x, <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>, ?z)
          WHERE MATCH (?x, <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>, ?y) IN xyzzy
          AND MATCH (?y, <http://www.w3.org/2000/01/rdf-schema#subClassOf>, ?z) IN plugh
PRINT result
```

Negation:

```
foo <- LOAD "foo"
result <- SELECT (?x, <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>, <http://xmlns.com/foaf/0.1/Person>)
          WHERE MATCH (?x, <http://www.w3.org/1999/02/22-rdf-syntax-ns#type>, <http://xmlns.com/foaf/0.1/Person>) IN foo
          AND NOT MATCH (?x, <http://xmlns.com/foaf/0.1/knows>, <http://example.org/nmg>) IN foo
PRINT result
```

Aggregation:

```
quux <- LOAD "quux"
result <- SELECT (?x, <http://example.org/ont/price>, MAX(?p))
          WHERE MATCH (?x, <http://example.org/ont/price>, ?p) IN quux
          GROUP BY ?x
PRINT result
```

## Errors

- **Parse errors** report the offending token, or unexpected end of input.
- **Undefined names** produce `<name> is undefined`.
- **Unbound output variables** in a `SELECT` produce `Unbound variable: <name>`.

## Limitations

- Typed literals are read as integers if they parse as one, otherwise as plain strings. Blank nodes are unsupported and raise an error.
- Integers are unsigned in query text.
- There is no static type checking, and no user-defined functions.

## VS Code syntax highlighting

`rql-vscode/` contains a TextMate grammar and language configuration for `.rql` files. To install the prebuilt package:

```sh
code --install-extension rql-vscode/rql-0.1.0.vsix
```

## Further reading

`report.pdf` explains the design decisions, grammar, built-in operations, extensions and testing approach in detail.

