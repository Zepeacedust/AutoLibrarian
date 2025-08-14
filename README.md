# Autolibrarian bot usage guide

## Calculator

A message prefixed with `!calc` is interpreted as a calculation command.

The calculator can process units and do basic dimensional analysis, but its syntax is slightly nonstandard.

It follows standard order of operations, parentheses -> the unary negation -> exponents -> multiplication and division -> addition and subtraction, within a priority level all operations except exponents are left associative, or evaluated from left to right.

Exponentiation is represented with b^e, where b is the base and e the exponent.
It is the only operation that does not use arbitrary precision fractions, and as such can introduce errors.
The exponent needs to be dimensionless, that is to say have no unit, but the base can have any unit.

The unary negation is the operation -a, this does mean that -2^2 = 4, but 1 - 2^2 = -3, since that is a binary - and binds looser. 

There is no implicit multiplication, all multiplication must be written as a * b, a and b can have different units and the result will have the correct unit from them. 

Division is similar but written a/b. Since it is left associative I reccomend using parentheses.

Addition and subtraction are used in the traditional manner, and the operands must have the same units.

A unit is treated as a number in and of itself, and must be multiplied as such.
For example, 1km must be written 1*km.

The output may be in units that do not quite fit, in that case use a format specifier.
A format specifier is a calculation command formatted like `!calc [expression] to [format]`, format can be another expression that has the same unit as a result, the expression will then be scaled to match the format.

As an example `!calc 10 * km * 1 * m` returns `1.0e8 cm^2.0`, 100 thousand square centimeters is not that useful when dealing with spells that use paces, so converting it makes interpretation easier `!calc 10 * km * 10 * m to pace ^ 2`, it gives a more reasonable `11959.900463010801 pace ^ 2`. Rounding is currently left as an excercise for the user.

### List of units and their values

| Unit             | Value                  | aliases                                  |
|------------------|------------------------|------------------------------------------|
| Second           | Base time unit         | s, [s/S]ec, [s/S]econd(s)                |
| Minute           | 60 seconds             | m, [m/M]in, [m/M]inute(s)                |
| Hour             | 60 minutes             | h, [h/H]our(s)                           |
| Day              | 24 hours               | [d/D]ay(s)                               |
| Week             | 7 days                 | [w/W]eek(s)                              |
| Month            | 30.44 days             | [m/M]onth(s)                             |
| Season           | 3 months               | [s/S]eason(s)                            |
| Year             | 4 seasons              | [y/Y]ear(s)                              |
| Centimeter       | Base distance unit     | cm, [c/C]entimeter(s), [c/C]entimetre(s) |
| Meter            | 100 cm                 | m, [m/M]eter(s), [m/m]etre(s)            |
| Kilometer        | 1000km                 | km, [k/K]ilometer(s),                    |
| Inch             | 2.54cm                 | in, [i/I]nch(es)                         |
| Foot             | 12 inches              | ft, [f/F]oot, [f/F]eet                   |
| Yard             | 3 feet                 | [y/Y]ard(s)                              |
| Pace             | 3 feet                 | [p/P]ace(s)                              |
| Mile             | 5280 feet              | [m/M]ile(s)                              |
| Penny            | Base currency unit     | [p/P]enny, [p/P]ennies                   |
| Denarius         | 1 penny                | [d/D]enarius, [d/D]enarii                |
| Solidus          | 12 pennies             | [s/S]olidus, [s/S]olidi                  |
| Shilling         | 12 pennies             | [s/S]hilling(s)                          |
| Pound (currency) | 20 shillings           | [l/L]ibra                                |
| Gram             | Base weight unit       | g, [g/G]ram(s)                           |
| Kilogram         | 1000 grams             | kg, [k/K]ilogram(s), [k/K]ilo(s)         |
| Ton              | 1000 kilograms         | [t/T]on(s)                               |
| Pound (weight)   | 453.59237 grams        | lb, [p/P]ound(s)                         |
| Pawn             | Base vis unit          | [p/P]awn(s)                              |
| Rook             | 10 pawns               | [r/R]ook(s)                              |
| Queen            | 10 rooks               | [q/Q]ueen(s)                             |
| Liter            | 1000 cubic centimeters | [l/L]iter(s)                             |
| Pi               | 3.141592653589793      | [p/P]i                                   |


## Spell fetcher

A spell mentioned by name and enclosed within square brackets as so: `[[Pilum of fire]]` will have its full text posted as a reply to the message.

It findss the closest by [levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance), so most minor misspellings can be recovered, but a larger change in the structure of the name is not. 

## Spell finder tool

A message prefixed with `!find` is interpreted like a find command.
A find command has several filters relating to attributes of the spell in question. 

### Level
Aliases: "l", "L", "level", "Level"

It is a number, so all of <, >, ==, =<, => are defined with it.
General spells are treated as being of all levels.
To match levels literally, use == comparing to either "gen", or "Gen"

### Techniques and Forms
Technique aliases: "te", "Te", "technique", "Technique"

Form aliases: "fo", "Fo", "form", "Form" 

Due to technical limitations, the techniques and arts need to be compared directly to the common abbreviation strings.
So Terram is only "Te".

Since the arts do not have a natrual ordering they only have two operators, == and :, they are quite similar but not identical.

== only shows spells that have that art as the main art, while : also searches for requisites.

### Tags
Aliases: "tag", "Tag"

Tags represent the misc. attributes a spell can have, such as some specific requisites, and the property of being a ritual.

Only == and : are defined for tags, and they are identical, testing if the spell has that tag.

### Range, Duration and Target
Range aliases: "r", "R", "range", "Range"

Duration aliases: "d", "D", "duration", "Duration"

Target aliases: "t", "T", "target", "Target"

As of right now, only == and : are defined, testing if the parameter matches exactly.
It is picky with the spelling, so be warned.

### Source
Aliases: "s", "S", "source", "Source"

Matches the exact name of the book, as shown in the source entry from the spell fetcher.

### Syntax

Beyond the comparators, there are some logical operators.
Filters are separated by spaces, and are treated as a logical disjunction (they all need to be true).

For a logical or use `||`, they bind weaker than the comparators, so `te=Re fo=Te || tag:ritual` is parsed as a rego terram spell, or a ritual.

## Virtue & Flaw fetcher

If you mention any virtue enclosed by doule square brackets, like so: `{{Lame}}` then the bot will respond with that virtue.

It uses the same system as the spell fetcher to handle misspelled names, and as such will handle most minor misspelling but not entirely changed structure.
