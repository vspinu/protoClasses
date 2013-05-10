<!-- % Contextual prototype programming in R -->
<!-- % Vitalie Spinu -->
<!-- % 2013/05/10 -->



# Contextual prototype programming in R.

Prototype programming is an object oriented (OO) programming paradigm that uses
objects rather than classes to represent behavior and structure of
objects. Inheritance is also classless, that is object inherit from other
objects.

<!-- Thus, this style of programming can also be called object-based -->
<!-- object-orientation:) -->

Class-based object orientation (Java, C++, Ruby etc.) separates the instances
(objects) from their abstract representation (classes). Thus, a developer has
first to declare classes and then instantiate objects. This has an advantage of
keeping declarations separately from dynamically created objects, but makes
designing process more tedious. Prototype-based OO completely drops classes and
thus allows for a faster development cycle.

<!-- As always, there is no free lunch, -->
<!-- and prototype-based orientation has it's disadvantages, but that is a topic for -->
<!-- a different discussion. -->

## Overview

ProtoClasses package is designed for complex applications with very pronounced
hierarchical organization of the functionality, code and data. It has
similarities with proto package and Reference Classes in R and it seamlessly
blends with R's class system.
 
There are two fundamental objects, _`protoCells`_ and _`protoContext`_, which
both inherit (in S4 sense) from common `evnProtoClass`. The latest is only of
the very little importance and won't be discussed much in what follows.

ProtoCells are the elementary objects that can instantiate other objects. They
encapsulate `methods`, `fields` and `forms`. Methods and fields are pretty much
like methods and fields in other OO systems (like Reference Classes in R). Forms
are specific to protoClasses package and are essentially unevaluated expressions
that proto objects inherit from each other. Forms are essentially an R-ish way
to implement proper dynamic dispatch. More on this later.

ProtoCells are a very handy tool to hierarchically build functionality. Often it
is very convenient to "store" this functionality in some common place instead of
letting it "float" around in the form of individual objects. This is the task of
_protoContexts_.

ProtoContexts are also proto objects that have methods, fields and forms, but in
addition they can also store protoCells. In this sense, protoContexts are like
containers for protoCells. The interpretation of protoContexts is application
specific but it is always a context of a complex task. It can be, for example, a
_model_ containing data, analysis code, results, simulation, etc. It can be, a
_workplace_, holding all your code and data relevant to some task or application
(just like R's global environment). It could be a _plot_, holding your data,
code and all the layers necessary for the plot.

## Proto Cells

All proto objects are implemented as classed environments, that is they inherit
from S4 class environment. See [implementation].

Each proto object has a short type (object name), which default to "--":


```r
library(protoClasses)
x <- protoCell()
x$type
```

```
## [1] "--"
```


All protoCells inherit from the root cell with the name "*". The long type (full
name) consists of the concatenation of all the short types of its direct and
indirect prototypes:


```r
x <- protoCell(type = "x")
y <- x$new(type = "y")
z <- y$new("funny_type")
z$type
```

```
## [1] "funny_type"
```

```r
z$Type
```

```
## [1] "funny_type.y.x.*"
```


In the above example `x` is a prototype of `y`, and `y` is a prototype of
`z` and it means in technical terms that `x` is parent environment of `y`, and
`y` is parent environment of `z`. Thus, all objects defined in `x` are visible
in `z` unless they are redefined in `y` or `z`.

The hierarchy always starts with the root (*) type that holds all the builtin
functionality necessary for the proto-system to work. The enclosure of the root
proto object is by default global environment.

### Fields

Fields represent the abstract idea of a state of the objects. In protoClasses
fields are implemented by means of accessor functions. You can initialize fields
in two ways. Simple way, by supplying the initial value, and advanced by
supplying the accessor function that will take care of the storing and retrieval
of the field.

Simple interface builds the default accessor function that simply assigns and
retrieves the value in the proto object with the appropriate conversion as given
by the class of initial value supplied. For example:


```r
x$initFields(int = 12L, str = "sfdsfdf")
x$int
```

```
## [1] 12
```

```r
x$int <- 13.3
x$int
```

```
## [1] 13
```

```r
x$str <- 13.3
x$str
```

```
## [1] "13.3"
```


In the above example `int` fields assign and retrieves an integer value from the
object; `str` is always a string.

Children of `x` automatically inherit all the fields (also methods and forms)
from their parents:


```r
y$int
```

```
## [1] 13
```

```r
z$int
```

```
## [1] 13
```

```r
y$int <- -100
z$int
```

```
## [1] -100
```

Fields can be accessed by means of the built in field `fields`:


```r
x$fields$int
```

```
## [1] 13
```


Default fields assign the object in the proto environment with the same names as
supplied during the initialization:


```r
ls(x)
```

```
## [1] "e"   "int" "str"
```


For a more complex fields you can supply accessors directly:


```r
liquid <- protoCell("liquid")

liquid$initFields(kelvin = 0, celsius = protoField(function(value) {
    if (missing(value)) {
        kelvin + 273.15
    } else {
        assign("kelvin", value - 273.15, envir = .self)
        invisible(value)
    }
}))

liquid$celsius
```

```
## [1] 273.1
```

```r
liquid$kelvin <- 100
liquid$celsius
```

```
## [1] 373.1
```

```r
liquid$celsius <- 100
liquid$kelvin
```

```
## [1] -173.1
```


The convention for field accessor is pretty simple. It is a function one
argument and it is called with no argument if it is used as a getter (liquid$celsius)
and it is called with one argument when it is used as a setter (liquid$celsius <-
value).

Note that each field accessor can access the calling object with `.self`. In the
above example `celsius` accessor assigns variable `kelvin` new value in
environment `.self`. Moreover, during the call to celsius field, the accessor
enclosure is dynamically set to be `x` (same as `.self`).

Often it is useful to access other fields from withing field accessor. This
could be done by means of object `.self` or by using `.fields` container from
within accessor function. The following are equivalent in terms of behavior:


```r
fahrenheit1 <- function(value) {
    if (missing(value)) 
        .fields$celsius * 9/5 + 32 else .fields$celsius <- (value - 32) * 5/9
}

fahrenheit2 <- function(value) {
    if (missing(value)) 
        .self$fields$celsius * 9/5 + 32 else .self$fields$celsius <- (value - 32) * 5/9
}

liquid$initFields(fahr1 = protoField(fahrenheit1), fahr2 = protoField(fahrenheit2))
```


The first version of fields uses the internal container `.field` that stores all
the accessors. Same holds for methods, forms and cells, which are stored in
specialized containers. Containers will be rigorously defined latter.

### Methods

Methods are just functions that can see all the objects from withing the proto
object. In contrast to R standard functions, which have fixed enclosure (lexical
scope), the enclosure of methods is the proto object itself. This is an instance
of dynamic scope. 

For example:


```r
liquid$initMethods(freeze = function() .self$celsius <- freezing_temp, boil = function() .self$celsius <- boiling_temp, 
    warm_up = function(by = 10) assign("kelvin", kelvin + by, .self))

liquid$celsius <- 100
liquid$warm_up(33)
liquid$celsius
```

```
## [1] 133
```


The `freeze` and `boil` methods will through an error because `freezing_temp` and
`boiling_temp` are undefined.


```r
watter <- liquid$new("watter", expr = expression(freezing_temp = 0, boiling_temp = 100))

watter$celsius <- 17
watter$warm_up(13)
watter$celsius
```

```
## [1] 30
```

```r
watter$freeze()
```

```
## Error: object 'freezing_temp' not found
```

```r
watter$celsius
```

```
## [1] 30
```


The above example ilustriate the crucial distinction between OO and functional
programming - dinamic scoping. The scope of `freeze` and `boil` methods is being
dynamically changed depending on the context. Even though these methods were
defined in object `liquid`, when called by `watter` they operate on objects
defined in `watter` and not in `liquid`. This mecanism is very powerful but it
is also complex and often very dificult to analize.

### Forms

Proto forms are specially managed expressions. They allow selective inheritance
of the code. As you will see proto forms inheritance is the ultimate example of
code reuse.


```r
x$initForms(A = form(B1 = form({
    a <- 10
}), B2 = form(C = form({
    a * 10
}))))

x$A
```

```
## e(A.B1) (from <text>#1)
##  {
##      a <- 10
##  }
## e(A.B2) .. e(A.B2.C) (from <text>#3)
##     {
##         a * 10
##     }
```


Let's write a method to test this out.


```r
x$initMethods(test = function() e(A))  ## e function evaluates forms in .self environent
x$test()
```

```
## [1] 100
```


Now replace `A.B1` form:


```r
y$setForms(A.B1 = form(a <- -10))
y$test()
```

```
## [1] -100
```



## Proto Context

### Cells

## Application Development

### S4 Inheritance 

## Examples 

## Other Programming Paradigms

### proto
### refClasses
### S4


