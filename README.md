# Insieme's Haskell-based Analysis Toolkit

> Putting a HAT on Insieme

## Abstract

This document provides an insight into the Haskell-based Analysis Toolkit (HAT), allowing developers and researchers to rapidly prototype and develop static program analyses.
It is therefore composed of an introduction into the topic of static program analysis, an architectural overview of the framework, the specification of a variety of essential analyses and components, a tutorial for designing new analyses showcasing the framework, followed by an evaluation of the frameworks capabilities.
The conclusion summarises the contributions of this thesis and provides an outlook on future work.

For the tutorial, and to showcase the framework, an array out-of-bounds analysis is constructed.
This analysis is also utilised for evaluating the framework, by investigating its ability of implicitly expanding a program analysis language feature support.
Furthermore we evaluate the frameworks performance in terms of execution time and memory requirements by analysing 63 622 different properties within a total of 320 example codes.

The complete code of this project is available on [GitHub](https://github.com/insieme/insieme) embedded into the high-level analysis module of the Insieme compiler.

## Original PDF

The original version of this thesis has been written in LaTeX and is available [here](https://W4RH4WK.github.io/files/msc.pdf).
This repository maintains a version of it written in Markdown, which is converted to HTML and PDF using [Dogx](https://github.com/W4RH4WK/Dogx).
The file `output/thesis.html` is a standalone HTML document and the recommended way of viewing.

Note that the conversion from LaTeX to Markdown may have introduced some errors as a lot of manual steps were required.
