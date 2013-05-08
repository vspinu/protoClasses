
# Contextual prototype programming in R.

Prototype programming is an object oriented programming paradigm that uses
objects rather than classes to represent behavior and structure of
objects. Inheritance is also classless, that is object inherit from other
objects. Thus, this style of programming can also be called object-based
object-orientation:)

Class-based object orientation (like Java, C++, Ruby) separates the instances
(objects) from abstract representation of objects (classes). Thus, a developer
has first to declare classes and then instantiate objects. This has an advantage
of keeping declarations separately from dynamically created objects, but makes
designing process more tedious. Prototype-based object orientation completely
drops classes and thus allows for a faster development cycle. As always, there
is no free lunch, and prototype-based orientation has it's disadvantages, but
that is a topic for a different discussion.

## Overview

ProtoClasses package is designed for complex applications with very pronounced
hierarchical organization of the code, data and functionality. It has
similarities with proto package and Reference Classes in R and it seamlessly
blends with R's class system.
 
There are two fundamental objects, `protoCells` and `protoContext`, which both
inherit (in S4 sense) from common `evnProtoClass`. The latest is only of the
very little importance, both to programmers and users, and will not be discussed
much in what follows.

ProtoCells are the elementary objects that can instantiate other objects. They
encapsulate `methods`, `fields` and `forms`. Methods and fields are pretty much
like methods and fields in other class-based OO paradigms, including Reference
Classes in R. Forms are specific to protoClasses package and are essentially
unevaluated expressions that proto objects inherit from each other. Forms are
essentially an R-ish way to implement proper dynamic dispatch and it allows to
inherit in a rigorous fashion pieces of code from parent object to its
children. More on this later.

ProtoCells are a very handy tool to hierarchically build functionality. Often it
is very convenient to "store" this functionality in some common place instead of
letting it "float" around in the form of individual objects. This is the task of
protoContexts.

ProtoContexts are also proto objects that have methods, fields and forms, but in
addition they can also store protoCells, that is protoContexts are like
containers for protoCells. The interpretation of protoContexts is application
specific but it is always a context of a complex task. It can be, for example, a
_model_ containing data, analysis code, results, simulation, etc. It can be, a
_workplace_, holding all your code and data relevant to some task or application
(just like R's global environment, but with a much more rigorous
organization). It could be a _plot_, holding your data, code and all the layers
necessary for the plot.

## Proto Cells

### Inheritance

### Methods

### Fields

### Forms

## Proto Context

### Cells

## Application Development

### S4 Inheritance 

## Examples 

## Other Programming Paradigms

### proto
### refClasses
### S4


