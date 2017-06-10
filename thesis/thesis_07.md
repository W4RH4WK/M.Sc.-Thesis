# Conclusion

In this thesis a framework for implementing various static program analyses has been presented.
It utilises a Constraint-Based Analysis (CBA) framework, which is originally derived from the Data-Flow Analysis (DFA) framework presented by @Nielson:ppa and has further been customised by @herbert_phd.
This toolkit, created using Haskell, integrates seamlessly into the Insieme compiler, but can also be used as standalone.

It enables developers and researchers to rapidly develop and use specialised static program analyses, for various different kinds of applications (eg auto-tuning or identifying optimisation candidates in a compilation process).
In [Constructing a new Analysis] we have shown that crafting a new analysis only requires a small amount of boiler plate code and some moderate implementation effort.
Furthermore we have shown that the framework elevates the capabilities of newly realised analyses while maintaining decent performance.

## Future work

While the framework's current state already allows one to use it for various tasks, certain language features of the INSieme Parallel Intermediate REpresentation (INSPIRE) are not yet supported.
As stated in [Objectives], this merely builds the foundation upon which development will iterate on in future.

Regarding documentation, the core concept of the framework itself, as well as analyses implemented using it, are quite straight forward to grasp.
Yet, further documentation of certain edge cases would be helpful.
However, the main challenge encountered by new developers is understanding the different node types, constructs, and language extensions of INSPIRE.
This issue can be addressed by providing more in-depth documentation on language extensions and their underlying semantics, as well as their connections to other language extensions and the INSPIRE core language.

On the framework side, creating a catalogue of convenience functions (mainly queries) for all the different INSPIRE constructs would further ease the creation and prototype process of analysis.

Unit-testing an analysis is not that much of a hassle when using the framework together with Insieme.
For standalone purposes, however more tooling would be required to be efficient at writing tests.

And last, but certainly not least, debugging techniques and tools are needed.
At the current state of development, debugging mainly resorts to using Haskell's `Debug.Trace` and dumping the computed [Assignment], render it via [Graphviz](http://www.graphviz.org/).
A notable improvement would be to have a dedicated viewer for the [Assignment], in a similar fashion as INSPYER to INSPIRE.
