---
title: Insieme's Haskell-based Analysis Toolkit
subtitle: Putting a HAT on Insieme
author: Alexander Hirsch
date: 30 April 2017
---

# Abstract {-}

This document provides an insight into the Haskell-based Analysis Toolkit (HAT), allowing developers and researchers to rapidly prototype and develop static program analyses.
It is therefore composed of an introduction into the topic of static program analysis, an architectural overview of the framework, the specification of a variety of essential analyses and components, a tutorial for designing new analyses showcasing the framework, followed by an evaluation of the frameworks capabilities.
The conclusion summarises the contributions of this thesis and provides an outlook on future work.

For the tutorial, and to showcase the framework, an array out-of-bounds analysis is constructed.
This analysis is also utilised for evaluating the framework, by investigating its ability of implicitly expanding a program analysis language feature support.
Furthermore we evaluate the frameworks performance in terms of execution time and memory requirements by analysing 63 622 different properties within a total of 320 example codes.

The complete code of this project is available on [GitHub](https://github.com/insieme/insieme) embedded into the high-level analysis module of the Insieme compiler.
