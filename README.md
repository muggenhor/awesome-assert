# AwesomeAssert

[![Tests](https://github.com/muggenhor/awesome-assert/actions/workflows/flake.yml/badge.svg)](https://github.com/muggenhor/awesome-assert/actions/workflows/flake.yml)

An assert implementation that aims to provide usable information while maintaining as small a performance footprint as possible when _not_ failing.

## Usage

This library is intended to be used by `#include <awesome/assert.hpp>` and use of one of these three assert macros:

```cppp
#define AWESOME_EXPECTS(<boolean-expr>)  // precondition
#define AWESOME_ASSERT(<boolean-expr>)   // invariant
#define AWESOME_ENSURES(<boolean-expr>)  // postcondition
```

* `AWESOME_ASSERT` is for conditions that the containing component is responsible for meeting.
* `AWESOME_EXPECTS` is for checking of *documented* pre-conditions of the public API of your component.
* `AWESOME_ENSURES` is for checking whether results and state adhere to *documented* post-conditions.

Generally you'll want to use `AWESOME_EXPECTS` and `AWESOME_ENSURES` at the boundary of your component's public interface.
While sticking to `AWESOME_ASSERT` for internals.

There are some limitations to the complexity of expressions you can pass to these assert macros.
These limitations stem from relying on expression decomposition to be able to produce error messages that include the values of the operands.
But we're documenting them here to prevent confusion.

```cpp
AWESOME_ASSERT(x COMP y);
AWESOME_ASSERT(x COMP y && y COMP z);
AWESOME_ASSERT(x COMP y || y COMP z);
```

Where `COMP` is one of these binary comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=` and `&` (bitwise and, for use as bitmask test operator).

Note that only a *single* instance of a logical operator (`&&` or `||`) is allowed because introspection fails beyond that.
Additionally `x`, `y` and `z` represent actual values here (or function calls that produce them), not boolean expressions.

In most cases you shouldn't experience any problems resulting from these limitations.
The most common problem is having more than two boolean expressions combined with `&&`.
In almost all cases the solution for that is to have a separate assert statement per such boolean expression.

## Requirements

### Capture and Log Context When Failing

For binary comparisons, don't just log the stringified expression, log the values of the two operands.
For unary expressions, i.e. convertible-to-bool, log the value of that expression.

This should work without having to use separate macros for every comparison operator.

### Code Locality for Non-Failing Path

The compiler-generated code (assembly) should not branch to a distant site when the asserted condition is met.
Preferably no branch at all should occur when the asserted condition is met.

### Don't Waste Cycles on Diagnostics for Non-Failing Path

No preparation of diagnostic information should happen at all when the asserted condition is met.

The largest performance impact permitted when not failing the condition is register spill-over.
Specifically the register spill-over caused by the need to retain the values of the comparison expression is permitted.

TODO: Look into compiler optimizations capable of reconstructing the values instead of storing them.

### Customizable Handling

Permit the user to override the failure handler.
It should stay `[[noreturn]]` though.
Preferrably `noexcept` as well, although that should probably be customizable.

## License

Copyright (C) 2016-2018 Giel van Schijndel

AwesomeAssert is free software: you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public License
as published by the Free Software Foundation, either version 3 of
the License, or (at your option) any later version.

AwesomeAssert is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the
GNU Lesser General Public License along with AwesomeAssert.
If not, see <http://www.gnu.org/licenses/>.
