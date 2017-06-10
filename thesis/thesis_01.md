# Introduction

This document describes a framework for easing the implementation of data-flow analyses within the Insieme project.
Note that throughout this document the terms framework, toolkit, and Haskell-based Analysis Toolkit (HAT) are used interchangeably.

The core contribution of this project is the implementation of a scalable, easy-to-use static program analysis framework, which can be used for rapid development and prototyping of data-flow analyses.

As this project is a joint effort between Herbert Jordan, Thomas Prokosch, and myself, a section dedicated to the attribution is provided next.
It is followed by a motivation section, the objectives, and a justification for picking Haskell as platform for the framework.
The introduction is concluded with a structural overview of the remaining document.

## Attribution

The theoretical groundwork, as well as a first prototype has been created by Herbert Jordan, who also plays a major role in the current development of the framework.
He is tasked with implementing the core components of the framework and overlooking the development process.

Thomas Prokosch helped with the initial technology assessment and prototyping components in Haskell before they are integrated into the framework.

The main tasks of the author of this thesis are:

- integration of the C++ / Haskell interface
- framework interface design
- Intermediate Representation (IR) data structure
- documentation and tutorial
- implementing utilities
- example analyses
- debugging tools

## Motivation

Program analysis, in general, is a big topic, not only in compiler theory.
Extracting certain properties automatically from an input program can be viable for multiple different use-cases (eg auto-tuning or code validation).
When talking about compilers, many of their optimisations rely on some kind of Data-Flow Analysis (DFA) to identify potential optimisation options.

The biggest nugget of motivation can be mined from the idea of having a generic Constraint-Based Analysis (CBA) framework at hand, which already provides a basic set of common analyses and can be extended rapidly as needed.
The generic DFA provided by the framework presented by this thesis handles (among others) declarations, parameter and return value passing, closures, support for composed data types, calls to unknown external functions, inter-procedural analyses, and memory locations.

By integrating it into a source-to-source compiler, and operating on a high-level intermediate representation, little boilerplate code is required to construct new analyses for various tasks.
Prototypes for research as well as for development purposes can be efficiently implemented.


## Objectives

The ultimate goal of this project is the realisation of a scalable implementation of the CBA framework developed by @herbert_phd exhibiting superior usability.
The main objective of this thesis is to help lay the foundation for this framework and its integration with Insieme.
To achieve the ultimate goal, further development will build upon this work in the future.

The main objective therefore consists of developing the core components of the framework.
These are composed of a *fixpoint solver*, a smaller set of generic analyses, and a bigger set of basic (more specific) analyses.
The intention behind each of these components and their relationship with each other is explained in [Framework Implementation].

The framework is a part of the Insieme project and therefore operators on the IR of the Insieme compiler -- namely the INSieme Parallel Intermediate REpresentation (INSPIRE).
This IR is implemented as C++ data structure in the core of the compiler.
However, as the framework is not realised using C++ (see below), a suitable representation of INSPIRE needs to be established.
This also includes a way to transfer an INSPIRE program from the Insieme compiler to this representation.
Furthermore, the framework should provide a way of relaying back analysis results to C++.

Also, it should be possible to use the framework as standalone, without the need for the Insieme compiler.

Last but not least, creating documentation of the framework and a manual for further analysis development is needed. This objective is reflected by this document.

## Picking Haskell

The first prototype of the CBA framework has been implemented using C++.
During its development drawbacks of using C++ manifested in the form of long compile times yielding a very slow development cycle.
Due to this reason, a different programming language (platform) has been chosen for the framework.

Due to the mathematical nature of program analysis, a functional programming language is preferred for the implementation.
The mathematical constructs defining analyses can be realised much simpler via the use of Algebraic Data Structures (ADTs) offered by functional languages, compared to structs, classes, and records of imperative and object-oriented languages.

[Haskell](https://www.haskell.org), popular amongst functional programming languages, features expressive syntax, a strong, static type system, and a growing community maintaining thousands of packages and libraries.
It was therefore ultimately selected for this task.
The average reader of this document may not be familiar with Haskell -- or functional programming languages in general -- and is therefore invited to pick it up and start their functional programming adventure.
@learnyouahaskell composed an excellent book going by the name of *Learn You a Haskell for Great Good!: A Beginner's Guide*, endorsed by the Haskell community, and available online.
Core concepts are explained with additional background information and useful analogies.
The playful art style and pop culture references positively influence the learning experience.
We also recommend the books *Real World Haskell* [@realworldhaskell], *Haskell Design Patterns* [@haskelldesignpatterns], and *Haskell High Performance Programming* [@highperformancehaskell].

Other functional programming languages like [OCaml](http://ocaml.org), [F#](http://fsharp.org), [Scala](http://scala-lang.org), and [Erlang](http://www.erlang.org) were considered too, yet have been ruled out during an initial technology assessment phase.
Since F\# builds on the .NET framework it was quickly removed from the list of considerations.
The overall footprint seems too large and no benefits of having the .NET framework on-board materialised.
Both Scala and Erlang seem interesting as well as promising, although on further investigation Haskell promises a more advanced type system and people have reported that its easier applicable and better designed compared to Scala [@haskellvsscala].
In contrast to Erlang, Haskell is statically typed -- something we very much prefer as it makes it easier to catch bugs early in the development process.
OCaml, in fact quite similar to Haskell, has been reconsidered multiple times during the prototyping phase.
In the end, personal preference in the syntax and design of the standard library resulted in the selection of Haskell instead of OCaml.

For the remainder of this thesis, it is assumed that the reader has a basic understanding of the Haskell programming language.
Nevertheless, details and more complex concepts are introduced when needed, along with additional references.

## Overview

[Static Code Analysis] provides a brief introduction to static code analysis, focusing on *flow-sensitive* analysis.
The concept is communicated via an example utilising a more conventional DFA framework, followed by the constraint-based approach.
This leads to a generic CBA framework, which is originally based on the one presented by @Nielson:ppa.
Additionally, customisations and adaptations towards integrating CBA capabilities into Insieme have been made by @herbert_phd to the framework, which not only seem promising in theory but also bear in mind real-world use-cases and practical applications -- something not encountered that often in the field of program analysis.

Next, covered by [Architecture] the overall architecture of Insieme's Haskell-based Analysis Toolkit (HAT) is depicted, delivering the big picture of the task at hand.
This includes presenting the Insieme compiler followed by a detailed investigation of INSPIRE @jordan2013inspire.

The implementation part, described in [Framework Implementation], reflects the documentation of the current state of the framework.
The source code can be found on [GitHub](https://github.com/insieme/insieme) embedded into the analysis module of the Insieme compiler.
Presented code snippets have been simplified for clarity in some cases.

In [Constructing a new Analysis] a new analysis is constructed showcasing the new framework.
Furthermore, the newly implemented analysis is used to evaluate the framework in [Evaluation].
Finally, [Conclusion] summarises the contributions of this work an provides an outlook on future development.
