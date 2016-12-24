# AwesomeAssert

An assert implementation that aims to provide usable information while maintaining as small a performance footprint as possible when _not_ failing.

[![Build Status](https://travis-ci.org/muggenhor/awesome-assert.svg?branch=master)](https://travis-ci.org/muggenhor/awesome-assert)

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

Copyright (C) 2016 Giel van Schijndel

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
