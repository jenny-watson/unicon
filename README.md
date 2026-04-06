
# The `unicon` R Package <img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/jenny-watson/unit_conversion/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jenny-watson/unit_conversion/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

*Reliable, transparent unit conversions in a `tidyverse` environment*

Unit conversion is theoretically simple and practically complex. Despite
being a skill we all learn and apply as schoolchildren, the requirement
to convert between units of different types is a source of friction,
frustration and errors in many data analysis workflows. In the authors’
software team, a developer tasked with the implementation of a
platform-wide unit conversion system observed: *“This is the worst task
anyone could have given me. Get it right and no-one notices; get it
wrong, or try and explain how difficult it is, and you wind up looking
like a right idiot.”*

He would not be the first to [stumble
here](https://en.wikipedia.org/wiki/Gimli_Glider); nor would his errors
be the [most
expensive](https://en.wikipedia.org/wiki/Mars_Climate_Orbiter).

Inspired by his plight and the very real real-world challenges of
maintaining a living, breathing unit conversion library, the `unicon`
package was designed on the principle of maximum information density and
minimum margin for inconsistency to provide a reliable, maintainable,
`tidyverse` style interface for easy unit conversions in R data
analysis.

The package is designed to provide:

1.  A set of commonly used units, codified by their relation to one
    another and their many human-readable aliases.
2.  A mimimal set of functions for easily and transparently converting
    one unit to another.
3.  A simple, safe system for adding to and testing the conversion
    library to maximise maintainability and usefulness.

Why `unicon`? Because the authors are too lazy to type
**un**it\_**con**vert the number of times that developing this package
would otherwise require, and because this simple expediency
serendipitously lands us a consonant away from a legendary mythical
creature :unicorn:

This package was imagined and written by [Dr Jenny
Watson](https://github.com/jenny-watson) and [Dr Alasdair
Sykes](https://github.com/aj-sykes92). If you use this package in your
work, please cite it as:

Watson, J. & Sykes, A. J. (2026) `unicon`: Reliable, transparent unit
conversions in a tidyverse environment. Version 0.0.0.9000. Available at
<https://github.com/jenny-watson/unicon/>.

> Zenodo DOI badge here following release

## Installation

This package is hosted on GitHub and can be installed using the
`remotes` package:

``` r
# install.packages("remotes")
remotes::install_github("jenny-watson/unicon@*release")
```

## Usage — `unicon_full`

The core user-facing function of the `unicon` package is `unicon_full`.
Use it like this:

``` r
library(unicon)

raw_values <- c(54.21, 71.24, 55.81, 11.33, 70.59)
raw_units <- c("tonnes / ha", "tons per acre", "t/ha", "kg /Hectare", "g/m2")
unit_out <- "tonnes / ha"

unicon_full(
  value_in = raw_values,
  unit_in = raw_units,
  unit_out = unit_out,
  pull = TRUE
)
```

    ## [1]  54.21000 159.69320  55.81000   0.01133   0.70590

The function uses `unicon`’s extensive library of unit aliases to match
the highly varied raw input units to their corresponding unit IDs, and
subsequently converts them to the specified output unit.

A number of features are built into the function to help keep
conversions safe and transparent. Firstly, the user may wish to see the
full calculation thread which leads to this outcome, which can be
achieved by setting the `pull` argument to FALSE. This yields a tibble,
which we’ll `glimpse`:

``` r
unicon_full(
  value_in = raw_values,
  unit_in = raw_units,
  unit_out = unit_out,
  pull = FALSE
) %>%
  glimpse()
```

    ## Rows: 5
    ## Columns: 13
    ## $ unit_in   <chr> "tonnes / ha", "tons per acre", "t/ha", "kg /Hectare", "g/m2"
    ## $ unit_out  <chr> "tonnes / ha", "tonnes / ha", "tonnes / ha", "tonnes / ha", …
    ## $ alias_in  <chr> "tonnes/ha", "tonsperacre", "t/ha", "kg/hectare", "g/m2"
    ## $ alias_out <chr> "tonnes/ha", "tonnes/ha", "tonnes/ha", "tonnes/ha", "tonnes/…
    ## $ id_in     <chr> "t__ha", "st__acre", "t__ha", "kg__ha", "g__m_2"
    ## $ id_si     <chr> "g__ha", "g__ha", "g__ha", "g__ha", "g__ha"
    ## $ id_out    <chr> "t__ha", "t__ha", "t__ha", "t__ha", "t__ha"
    ## $ error_in  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE
    ## $ error_si  <lgl> FALSE, FALSE, FALSE, FALSE, FALSE
    ## $ error_out <lgl> FALSE, FALSE, FALSE, FALSE, FALSE
    ## $ value_in  <dbl> 54.21, 71.24, 55.81, 11.33, 70.59
    ## $ value_si  <dbl> 54210000, 159693200, 55810000, 11330, 705900
    ## $ value_out <dbl> 54.21000, 159.69320, 55.81000, 0.01133, 0.70590

This returns the following information, representing the calculation
process in rough left –\> right chronology:

- `unit_in` The user-supplied `unit_in` argument.
- `unit_out` The user-supplied `unit_out` argument.
- `alias_in` The derived alias associated to the `unit_in` argument,
  generated by stripping whitespace and converting all characters to
  lowercase.
- `alias_out` The derived alias associated to the `unit_out` argument,
  derived similarly.
- `id_in` The ID acquired for the `alias_in` variable.
- `id_si` The ID associated with the “SI” (standard reference point)
  unit for the type class of the input and output units.
- `id_out` The ID acquired for the `alias_out` variable.
- `error_in` Logical; were there errors in processing the input unit?
- `error_si` Logical; were there errors in deriving the SI unit?
- `error_out` Logical; were there errors in deriving the output unit?
- `value_in` The user-supplied `value_in` argument.
- `value_si` The values as converted to their SI (standard reference
  point) unit.
- `value_out` The values converted to the `unit_out` argument, if
  supplied. If not supplied, the standard reference point unit will be
  returned and the values will be identical to `value_si`.

To understand more about the significance of the `si` unit, read **A
brief note on design and methodology**, below.

All the core input arguments are vectorised and will attempt to
replicate if supplied as scalar values. This is helpful, as above, in
specifying a single unit of output, or in converting vectors of
consistent unit type (`unicon_full(my_values, "tonnes/ha", "g/m2")`).
It’s also possible to omit the `unit_out` argument altogether; if
omitted, the function will return values in the associated standard
reference unit and will message the user to indicate that this has been
done.

Finally, although `unicon` has a very large library of unit aliases, it
is probable that it will be passed units that it does not recognise and
important that it handles these clearly:

``` r
unicon_full(
  value_in = raw_values,
  unit_in = c("tonnes / ha", "tons per acre", "t/ha", "kg /Hectare", "nonsense per fiction"),
  unit_out = unit_out,
  pull = TRUE
)
```

    ## Warning in unicon_full(value_in = raw_values, unit_in = c("tonnes / ha", : Some units failed to convert.
    ##               Set `pull = FALSE` for detailed output.

    ## [1]  54.21000 159.69320  55.81000   0.01133        NA

As previously, setting the `pull` argument to `FALSE` will yield better
information as to the nature of the error.

``` r
unicon_full(
  value_in = raw_values,
  unit_in = c("tonnes / ha", "tons per acre", "t/ha", "kg /Hectare", "nonsense per fiction"),
  unit_out = "tonnes / ha",
  pull = FALSE
)
```

    ## Warning in unicon_full(value_in = raw_values, unit_in = c("tonnes / ha", : Some
    ## input units failed to find matches.

    ## # A tibble: 5 × 13
    ##   unit_in       unit_out alias_in alias_out id_in id_si id_out error_in error_si
    ##   <chr>         <chr>    <chr>    <chr>     <chr> <chr> <chr>  <lgl>    <lgl>   
    ## 1 tonnes / ha   tonnes … tonnes/… tonnes/ha t__ha g__ha t__ha  FALSE    FALSE   
    ## 2 tons per acre tonnes … tonsper… tonnes/ha st__… g__ha t__ha  FALSE    FALSE   
    ## 3 t/ha          tonnes … t/ha     tonnes/ha t__ha g__ha t__ha  FALSE    FALSE   
    ## 4 kg /Hectare   tonnes … kg/hect… tonnes/ha kg__… g__ha t__ha  FALSE    FALSE   
    ## 5 nonsense per… tonnes … nonsens… tonnes/ha <NA>  <NA>  t__ha  TRUE     NA      
    ## # ℹ 4 more variables: error_out <lgl>, value_in <dbl>, value_si <dbl>,
    ## #   value_out <dbl>

## Usage — `unicon_lite`

It is anticipated that a secondary use-case for the `unicon`
functionality will be in a setting where mathematical unit conversion is
required, but where the environment is tightly controlled to the point
where recognition of thousands of aliases all pointing to the same
models is surplus to requirements. The `unicon_lite` function is
therefore designed to dispense with the alias-sifting that is handled
automatically in `unicon_full`, and instead takes the unit ID as a
direct argument for conversion:

``` r
unicon_lite(
  value_in = raw_values,
  id_in = c("t__ha", "st__acre", "t__ha", "kg__ha", "g__m_2"),
  id_out = "t__ha",
  pull = TRUE
)
```

    ## [1]  54.21000 159.69320  55.81000   0.01133   0.70590

As with `unicon_full`, `unicon_lite` will return a full dataframe if
required (`pull = TRUE`) and will warn if conversions fail.

## Usage — `unicon_catrel`

> TBC

## Usage — `unicon_help`

To use the `unicon` functions, it may be useful to directly see what is
recognised by the package. The internal package data is maintained in an
optimal format for efficent computation, but the `unicon_help` function
is designed to join and return this in the simplest format for
interactive analysis:

``` r
unicon_help()
```

    ## # A tibble: 11,599 × 6
    ##    id      alias       type    category    si      model           
    ##    <chr>   <chr>       <chr>   <chr>       <chr>   <list>          
    ##  1 acre    ac          base    area        ha      <named list [2]>
    ##  2 acre    acre        base    area        ha      <named list [2]>
    ##  3 acre    acres       base    area        ha      <named list [2]>
    ##  4 celcius celcius     base    temperature celcius <named list [2]>
    ##  5 celcius c           base    temperature celcius <named list [2]>
    ##  6 celcius celsius     base    temperature celcius <named list [2]>
    ##  7 cm      cm          base    length      m       <named list [2]>
    ##  8 cm      centimetres base    length      m       <named list [2]>
    ##  9 cm      centimeters base    length      m       <named list [2]>
    ## 10 cm__day cm/day      derived speed       m__s    <named list [2]>
    ## # ℹ 11,589 more rows

Here, the core data presents:

- `id`, the unit ID
- `alias`, the alias associated to that ID
- `type` of unit, either base or derived (see **A brief note on design
  and methodology** to understand the significance of this)
- `category`, the defined category within which conversions may be
  performed
- `si`, the associated standard reference unit for that `category`
- `model`, a list column providing the unit’s conversion model for
  converting to the associated `si` unit

Together, these variables form the core operating dataset for the
`unicon` functionality.

## A brief note on design and methodology

If your use of `unicon` falls firmly into the “casual” category, you can
probably skip this section. However, we recommend reading it (or
returning to it later) if you plan to rely on `unicon`’s functions in
any kind of higher-stakes setting, or on developing/modifying your own
conversion protocols. In this section, we aim to take you through some
of the methods and design approach behind the `unicon` system.

### Standard reference units

All unit conversions are performed with respect to a single reference
unit defined per category:

``` mermaid
graph LR
  A[/Input Unit/] --> B(Standard Reference Unit)
  B --> C[/Output Unit/]
```

Within the package data, each base unit is provided with its own
human-readable ID, and defined in terms of its relationship to that
single reference unit. A JSON schema for each base unit encapsulates
this information.

The following is the schema for the unit ID `acres`, a non-standard unit
of area commonly used in agriculture:

``` json
{
  "category": "area",
  "si": "ha",
  "model": {
    "slope": 0.4047,
    "intercept": 0
  },
  "alias": [
    "ac",
    "acre",
    "acres"
  ]
} 
```

Within `unicon`, the standard reference unit for area is a hectare (ID:
`ha`), and an acre is defined as approximately 0.4 of a hectare. If
converting from acres to any other unit of area, the quantity provided
will always first be converted to hectares. Likewise, if we know how to
convert from acres to hectares, we know how to do the reverse. It is
therefore easy to convert any other units of area to acres, provided we
can first convert them to hectares; and since all units of area are
defined by their relationship to a hectare, this is a given.

This approach is fundamental, and allows the package to focus on
maintaining only one conversion relationship per unit. This minimises
the physical size of data maintained and minimises the potential for
internal inconsistencies.

### Unit aliases

The other main attribute of a unit is its `alias` list. This is a list
of the human-readable names by which the unit may be called in the real
world. For the unit with ID `acre`, its possible aliases are
`ac, acre, acres`. Together with the standard reference unit and
conversion model, these pieces of information form the core “profile” of
a unit, representing the main things we need to know about it in order
to convert to or from it, and to correctly recognise it in the real
world.

A couple of simple transforms are used to keep the alias list as focused
as possible. Firstly, aliases are lowercase and treated as
case-insensitive. Secondarily, aliases are devoid of whitespace and
whitespace is stripped from user-supplied inputs, a feature which
becomes particularly useful when dealing with derived units (see below).

Combining all of this together with the established input –\> standard
reference unit –\> output convention gives us the full process flow of
the `unicon` function:

``` mermaid
graph LR
  A1[/Input unit/]
  B1(Input unit alias)
  C1(Input unit ID)
  
  A2[/Output unit/]
  B2(Output unit alias)
  C2(Output unit ID)
  
  D(Standard Reference Unit ID)
  
  E[[Check for errors in unit recognition]]
  E1[/Warn of issues/]
  
  F[[Convert input value to SRU]]
  G[[Convert SRU value to output]]
  H[/Return outputs/]
  
  A1 --> B1 --> C1
  A2 --> B2 --> C2
  C1 --> D
  C2 --> D
  
  C1 --> E
  C2 --> E
  D --> E
  E --> E1

  D --> F --> G --> H
  C1 --> F
  C2 --> G
```

### Derived units

So far, we have only discussed base units with schemas directly defined
in the core `unicon` package data. The `unicon` system draws a clear
distinction between “base” units, like hectares and acres, and “derived”
units like kilograms per hectare or gallons per acre. This distinction
is designed, as with the single point of reference for base units, to
minimise repetition and the need for redundant and potentially
inconsistent data. It works on the principle that if we know how to
convert to and from the base units which make up a derived unit, we must
also know how to convert to or from that derived unit to another in its
category.

For example, if we know how to convert kilograms to pounds and from
hectares to acres, then by definition, we know how to convert from
kilograms per hectare to pounds per acre. We don’t need to record any
additional information in order to achieve this.

In `unicon`, a separate JSON schema exists to define derived units.
Since the heavy lifting is done by the base unit schemas, this schema
can be extremely minimal, as with the below example for the derived unit
with ID `rate` (mass per unit area):

``` json
{
  "x": "mass",
  "y": "area",
  "operator": "divide"
} 
```

In this schema, `x` and `y` define the two base unit categories which
make up the derived category, and `operator` defines the relationship
between them.

### The build process

The final tenet of the `unicon` system is to minimise the amount of
computation that is done at execution time. This ensures calculation
stays fast, and aids in transparency, since the number of steps to get
from input to output remains minimal and identical for all units. To
achieve this, the build process, which translates from raw JSON schemas
to package internal data, aims to do as much pre-processing as possible.

Firstly, all base units schemas are read and their models codified.

Secondly, these base unit models, together with the derived unit
schemas, are used to build models and aliases for derived units. This
works as follows:

Each `operator` as defined in the derived unit schemas requires its own
definition and alias list, as per this one, for division:

``` json
{
  "id": "__",
  "fun": "/",
  "alias": [
    "/",
    "per"
  ]
} 
```

For each derived unit schema, aliases are generated by cross-joining:

1.  All aliases for base unit `x`
2.  All aliases for the defined `operator`
3.  All aliases for base unit `y`

An ID for the derived unit is created by pasting together the IDs for
contributing components i.e. `paste(x, operator, y)`. For example, a
rate unit combining tonnes (ID: `t`) and hectares (ID: `ha`) via
division (ID: `__`) would receive ID `t__ha`, with aliases `t/ha`,
`tperha`, `tonnes/ha`, `tonnesperha`, and so on. It is easy to see how
one simple relationship can quickly generate a bewildering number of
aliases.

The final stage of this build process is to mathematically recombine the
contributing models, so that the model for `t__ha` and all units in the
`rate` category remain a simple single-factor slope model. Note that it
is not possible to combine models which have a none-zero intercept in
this way, and these are excluded from derived unit calculations.

The package datasets created are referenced by all `unicon` functions,
and may also be examined/queried by the user via the `unicon_help`
function, discussed above.

    ## # A tibble: 11,599 × 6
    ##    id      alias       type    category    si      model           
    ##    <chr>   <chr>       <chr>   <chr>       <chr>   <list>          
    ##  1 acre    ac          base    area        ha      <named list [2]>
    ##  2 acre    acre        base    area        ha      <named list [2]>
    ##  3 acre    acres       base    area        ha      <named list [2]>
    ##  4 celcius celcius     base    temperature celcius <named list [2]>
    ##  5 celcius c           base    temperature celcius <named list [2]>
    ##  6 celcius celsius     base    temperature celcius <named list [2]>
    ##  7 cm      cm          base    length      m       <named list [2]>
    ##  8 cm      centimetres base    length      m       <named list [2]>
    ##  9 cm      centimeters base    length      m       <named list [2]>
    ## 10 cm__day cm/day      derived speed       m__s    <named list [2]>
    ## # ℹ 11,589 more rows

## Acknowledgements

> Tom, for hex sticker

## Contribute

If you would like to contribute to this package, please file an issue,
make a pull request on GitHub, or email the authors at **TBC**.
