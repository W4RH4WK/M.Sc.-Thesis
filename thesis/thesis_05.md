# Constructing a new Analysis

In this chapter we construct a new analysis using the established framework.
Our goal is to implement an *array out-of-bounds* analysis.
We go through the construction, implementation, and integration into INSIEME step by step:

- Defining the goals of the new analysis.
- Investigating the relevant constructs and language extensions of the INsieme Parallel Intermediate REpresentation (INSPIRE).
- Deriving from the gathered information what components / additional analyses are required to fulfil our goal.
- Defining the property spaces of the analyses to implement.
- Defining the interface for communication between INSIEME and the Haskell-based Analysis Toolkit (HAT).
- Setting up the required boilerplate code.
- Writing a handful of test cases to allow for checking the implementation during development.
- Implementing the required analyses.
- Summarising what files have been added / modified by the process.

The implemented analysis is used to evaluate the current state of the framework.

## Defining the Goals

The goal is to create an analysis which can identify out-of-bounds accesses to arrays.
For simplicity we will only concern ourselves with accessing elements of an array beyond its upper bound.
Also covering the lower bound will be a trivial extension and is left as an exercise to the reader.

The following code fragment will illustrate the essentials:

```cpp
{ // Case 1
    int a[21];
    a[42];
}

{ // case 2
    int a[21];
    a[20];
}

{ // Case 3
    int a[21];
    int *ap = a + 10;
    ap[20];
}

{ // Case 4
    auto a = new int[21];
    a[42];
    delete[] a;
}

{ // Case 5
    int a[21];
    int b;
    a[b];
}

{ // Case 6
    int a[21];
    int b = a[42];
}
```

Case 1
  ~ Accessing an element beyond the array's upper bound. The analysis returns `IsOutOfBounds` for `a[42]`.

Case 2
  ~ Accessing an element within the array's upper bound. The analysis returns `IsNotOutOfBounds` for `a[20]`.

Case 3
  ~ Same as Case 1, yet the array is accessed indirectly via a pointer offset to the base of the array. Same result as in Case 1.

Case 4
  ~ Same as Case 1, yet the array is allocated using the `new` keyword. Same result as in Case 1.

Case 5
  ~ Accessing the array with an undefined index, the analysis returns `MaybeOutOfBounds`.

Case 6
  ~ Same as Case 1, but involves an assignment.

This marks the base for the following sections.
Note that the framework will cover more complicated cases (eg where a pointer to an array is passed to a function and the function accesses the underlying array) automatically thanks to the generic Data-Flow Analysis (DFA).

## Investigating INSPIRE

The different cases defined in the previous section are used as input code the Insieme compiler.
The analysis runs on the resulting INSPIRE program, constructed by the frontend of the compiler.
Using `insiemecc` we can feed each of the cases to the compiler and retrieve the corresponding Intermediate Representation (IR) in various formats.
`case1.cpp` is the input to the compiler, while `case1.ir`, `case1.tree`, and `case1.json` compose the output we are interested in.
The following command is used to obtain the files.

    $ insiemecc --dump-tree case1.tree --dump-json case1.json --dump-ir case1.ir case1.cpp

`case1.ir` contains the pretty-printed text representation of the corresponding INSPIRE program.
`case1.tree` can be viewed with a regular text editor to investigate the node structure of this INSPIRE program.
Furthermore `case1.json` also contains the node structure, but in a format that can be viewed using INSPYER (see [INSPYER]), which provides a more interactive viewing experience.

Let us now take a look at the first case.

```{.txt .numberLines}
// case1.cpp

int main(void) {
	int a[21];
	a[42];
}
```

```{.txt .numberLines}
// case1.ir

decl IMP_main : () -> int<4>;
// Inspire Program
int<4> function IMP_main (){
    var ref<array<int<4>,21>,f,f,plain> v1 =
      ref_decl(type_lit(ref<array<int<4>,21>,f,f,plain>));
    *ptr_subscript(ptr_from_array(v1), 42);
    return 0;
}
```

```{.txt .numberLines}
// case1.tree

(Program | ...
    (CompoundStmt |
        (DeclarationStmt |
            (Declaration |
                (GenericType | ... )
                (CallExpr |
                    (GenericType |
                        (StringValue "ref")
                        (Parents )
                        (Types |
                            (GenericType |
                                (StringValue "array")
                                (Parents )
                                (Types |
                                    (GenericType | ... )
                                    (NumericType |
                                        (Literal |
                                            (GenericType | ... )
                                            (StringValue "21"))))
                                ... )))
                    (Literal |
                        (FunctionType | ... )
                        (StringValue "ref_decl"))
                    (Declaration | ... )))
            (Variable | ... ))
        (CallExpr |
            (GenericType |  ...)
            (Literal |
                (FunctionType | ... )
                (StringValue "ref_deref"))
            (Declaration |
                (GenericType | ... )
                (CallExpr |
                    (GenericType |  ...)
                    (LambdaExpr |
                        (FunctionType | ... )
                        (LambdaReference |
                            (FunctionType | ... )
                            (StringValue "ptr_subscript"))
                        (LambdaDefinition | ... ))
                    (Declaration | ... )
                    (Declaration | ... ))))
        (ReturnStmt | ... ))...)
```

As can be seen in `case1.ir` and `case1.tree`, the array is constructed using the operator `ref_decl` and accessed via the `ptr_subscript` operator.
Note that the pointer subscript operation is *not* prohibited, however, dereferencing its result *is*.
We therefore target calls to `ref_deref` with our analysis.

Furthermore, it is important to note that the array is never handled directly, but via a reference.
Hence our analysis is concerned with the type `ref<array<'a>>`.
This also means that (at least one of the components) is very similar to the reference analysis discussed earlier in [Reference Analysis].
It follows that a look at the reference language extension is essential.
Furthermore, as the array is also handled as pointer, taking a look at the pointer language extension is helpful too.

The relevant parts can be found in the header files `insieme/core/lang/reference.h` and `insieme/core/pointer.h`.
First thing to note here is that the pointer language extension builds on top of the reference extension.

For the creation process of the array, `ref_decl` is used.
As this operator is *not* derived, we have to handle it explicitly using an `OperatorHandler`.
See the declaration in `reference.h`

    LANG_EXT_LITERAL(RefDecl, "ref_decl", "(type<ref<'a,'c,'v,'k>>) -> ref<'a,'c,'v,'k>")

This size of the array (`21`) can be inferred from its argument `type_lit(ref<array<int<4>, 21>, f, f, plain>)`.

For the access we need to investigate the pointer language extension and immediately notice the following type alias

    TYPE_ALIAS("ptr<'a,'c,'v>", "( ref<array<'a>,'c,'v>, int<8> )");

which indicates that a pointer in INSPIRE is handled as a pair of array reference and offset.
Its use can be seen when inspecting the definition of the derived operator `ptr_subscript`.

    LANG_EXT_DERIVED(PtrSubscript, R"(
        (p : ptr<'a,'c,'v>, i : int<8>) -> ref<'a,'c,'v> {
            return p.0[p.1 + i];
        }
    )")

The offset `i`, used for the `ptr_subscript` call, is simply added to the second part of the pair `p` before the actual subscript happens.

As this function requires a pointer as argument, the original array is converted to a pointer by the frontend using the derived operator `ptr_from_array`, which in turn uses the abstract operator `ref_reinterpret`.
`ref_reinterpret` is a reinterpret cast altering the actual interpretation of the referenced memory call.

    // from pointer.h
    LANG_EXT_DERIVED(PtrFromArray, R"(
        (r : ref<array<'a,'s>,'c,'v>) -> ptr<'a,'c,'v> {
            return ( ref_reinterpret(r,type_lit(array<'a,inf>)), 0l );
        }
    )")

    // from reference.h
    LANG_EXT_LITERAL(RefReinterpret, "ref_reinterpret", "(ref<'a,'c,'v,'k>, type<'b>)"
                                                        " -> ref<'b,'c,'v,'k>")

The structure of Case 2 is identical to this one, the only difference is the literal for accessing the array element.
We therefore now look at Case 3.

```
// case3.cpp

int main(void) {
	int a[21];
	int *ap = a + 10;
	ap[20];
}
```

```
// case3.ir

decl IMP_main : () -> int<4>;
// Inspire Program
int<4> function IMP_main (){
    var ref<array<int<4>,21>,f,f,plain> v1 =
      ref_decl(type_lit(ref<array<int<4>,21>,f,f,plain>));

    var ref<ptr<int<4>>,f,f,plain> v2 =
      ptr_add(ptr_from_array(v1), 10);

    *ptr_subscript(*v2, 20);

    return 0;
}
```

In this INSPIRE program, the array is accessed via the pointer $v_2$ which is already offset by $10$ elements.
This offset is created by using the derived operator `ptr_add`.
Its definition follows:

    LANG_EXT_DERIVED(PtrAdd, R"(
        (p : ptr<'a,'c,'v>, i : int<8>) -> ptr<'a,'c,'v> {
            return ( p.0, p.1 + i );
        }
    )")

As only this derived operator is used, no additional changes to the analysis are necessary compared to Case 1 and 2.

Case 4 uses a different mechanism for allocating the array.

```{.txt .numberLines}
// case4.cpp

int main(void) {
	auto a = new int[21];
	a[42];
	delete[] a;
}
```

```{.txt .numberLines}
// case4.ir

decl IMP_main : () -> int<4>;
// Inspire Program
int<4> function IMP_main (){
    var ref<ptr<int<4>>,f,f,plain> v1 =
      ptr_from_array(
        // type
        <ref<array<int<4>,21>,f,f,plain>>

        // memory location
        (ref_new(type_lit(array<int<4>,21>)))

        // initialisation
        {}
      );

    *ptr_subscript(*v1, 42);

    ref_delete(ptr_to_array(*v1));

    return 0;
}
```

The syntax from line 8 to line 15 corresponds to an InitExpr, where the type is written between `< >`, the memory location between `( )`, and the initialisation between `{ }`.
In this case the type is straight forward, `ref_new` is used to allocate the array on the heap, and the array is not initialised.

We therefore have to look at the operator `ref_new` and see that it is derived and uses `ref_alloc` which is an abstract operator.
This abstract operator needs to be handled by the analysis using an `OperatorHandler`.

    LANG_EXT_DERIVED(RefNew, R"(
        (t : type<'a>) -> ref<'a,f,f> {
            return ref_alloc(t, mem_loc_heap );
        }
    )")

    LANG_EXT_LITERAL(RefAlloc, "ref_alloc", "(type<'a>, memloc) -> ref<'a,f,f>")

Case 5 does not introduce any new constructs, thus we can continue our investigation process.

In Case 6, the call to `ref_deref` we are interested in is wrapped in a Declaration which in turn is wrapped in a DeclarationStmt to model the assignment.
Since nothing else changed, we continue with a summary.

To summarise our findings we put together a table listing the relevant operators, shortly describing each of them.

------------------------------------------------------------------------------------------------------------------------------------------------
Operator            Type         Description
------------------- ------------ ---------------------------------------------------------------------------------------------------------------
`ptr_add`           *derived*    Offsets a given pointer with a given offset.

`ptr_from_array`    *derived*    Casts a given pointer to an array.

`ptr_subscript`     *derived*    Returns a reference to a specific array element.

`ref_alloc`         *abstract*   Allocate an object of the given type at a specific memory location (stack / heap), returning a reference to it.

`ref_decl`          *abstract*   Declares an object of the given type and returning a reference to it.

`ref_deref`         *abstract*   Obtain the data stored in the memory location referenced by the given reference.

`ref_new`           *derived*    Allocates an object of the given type at the heap, returning a reference to it.

`ref_reinterpret`   *abstract*   Alters the actual interpretation of the referenced memory cell.
------------------------------------------------------------------------------------------------------------------------------------------------

## Designing the Analysis

Our design of this new analysis is based on the investigations done in the previous section.
As has already been mentioned, the type we are concerned with the most is `ref<array<'a>>`.
It follows that the operators of the reference language extensions are relevant to this analysis.

We decide to split the analysis into two parts.
One for identifying the index (ie offset) used to access the array, one for deriving the size (ie number of elements) of the array.

The first part can be accomplished fully by the reference analysis (see [Reference Analysis]).
The key to this is by investigating the corresponding `DataPath` derived by the reference analysis.

For the second part we have create a new specialised DFA, tracking the number of elements of an allocated array.
We will therefore refer to this analysis as *element count analysis*.

A call to our (yet to construct) function `outOfBounds` will trigger both analyses and compare the results.
If an out-of-bounds access is noticed (ie an access where the offset is equal or greater to the array's element count), `IsOutOfBounds` is returned.
If one (or both) of the triggered analyses could not determine an accurate enough result, `MaybeOutOfBounds` is returned.
Otherwise we can safely say that no out-of-bounds (according to our set goals) occurs returning `IsNotOutOfBounds`.

Next we define the property spaces of our two *sub-analyses*.

## Defining the Property Spaces

This is straightforward as we have already covered the reference analysis in [Reference Analysis] and `DataPath`s in [Data Path Analysis].
Using the established reference analysis on the offset, we will get back a set of `References`, where `References` are defined as follows:

```haskell
data Reference i = Reference { creationPoint :: Location
                             , dataPath      :: DP.DataPath i }
                 | NullReference
                 | UninitializedReference
  deriving (Eq,Ord,Show,Generic,NFData)
```

Where we are interested in the `DataPath` of a `Reference`.
The type parameter `i` will be a `SimpleFieldIndex` (see [Data Path]).
From the `DataPath` we can infer the index used for accessing the array.

The element count analysis provides us a `SymbolicFormulaSet` like the arithmetic analysis (see [Arithmetic Analysis]).
This set contains `SymbolicFormula`s modelling the element count of the array.
We reuse the `Lattice` and `ExtLattice` instance definitions of `SymbolicFormula`.

## Defining the Interface

In this case, the interface is trivial and can be derived from the design process.
The possible outcomes are gathered in a single ADT.

```haskell
OutOfBoundsResult = MayBeOutOfBounds
                  | IsNotOutOfBounds
                  | IsOutOfBounds
  deriving (Eq,Ord,Enum,Show,Generic,NFData)
```

We could have used the same approach as for the boolean analysis (see [Boolean Analysis]), by defining this enumeration in a dedicated header file, which is then used in Haskell and C++ to ensure that the mapping is always correct.
Yet, for simplicity, we chose not to and define an identical enumeration in C++.
The definition goes into a new header file, dedicated to the out-of-bounds analysis:

```cpp
// File:      insieme/analysis/cba/haskell/out_of_bounds_analysis.h
// Namespace: insieme::analysis::cba::haskell

enum class OutOfBoundsResult : int {
    MayBeOutOfBounds,
    IsNotOutOfBounds,
    IsOutOfBounds,
};
```

Furthermore we add the prototype of the analysis function to the same header file.

```cpp
// File:      insieme/analysis/cba/haskell/out_of_bounds_analysis.h
// Namespace: insieme::analysis::cba::haskell

OutOfBoundsResult getOutOfBounds(Context& ctxt, const CallExprAddress& expr);
```

The Haskell function representing the out-of-bounds analysis has the following signature and goes into a new file `OutOfBounds.hs`:

```haskell
    outOfBounds :: SolverState -> NodeAddress -> (OutOfBoundsResult,SolverState)
```

As the interface for C++ as well as Haskell has now been established, the next step is to write the necessary boiler plate code in the adapter.

## Setup Boilerplate

The first thing we take care of is the exporting of the Haskell `outOfBounds` function.
As our result is an enumeration we can simply pass an integer representing the corresponding enum value via the Foreign Function Interface (FFI).
The following code is added to `Adapter.hs`.

```{.haskell .numberLines}
foreign export ccall "hat_out_of_bounds"
  outOfBounds :: StablePtr Ctx.Context -> StablePtr Addr.NodeAddress -> IO CInt

outOfBounds ctx_hs expr_hs = do
    ctx  <- deRefStablePtr ctx_hs
    expr <- deRefStablePtr expr_hs
    let (result,ns) = OOB.outOfBounds (Ctx.getSolverState ctx) expr
    let ctx_c = Ctx.getCContext ctx
    ctx_nhs <- newStablePtr $ ctx { Ctx.getSolverState = ns }
    updateContext ctx_c ctx_nhs
    return $ fromIntegral $ fromEnum result
```

The export is available under the symbol `hat_out_of_bounds` which we import in C++ next.

```cpp
extern "C" {
    namespace hat = insieme::analysis::cba::haskell;
    int hat_out_of_bounds(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);
}
```

This import and the wrapper around it (next code snippet) both go into a new source file `out_of_bounds_analysis.cpp`.

```{.cpp` .numberLines}
// Namespace: insieme::analysis::cba::haskell

OutOfBoundsResult getOutOfBounds(Context& ctxt, const core::CallExprAddress& call) {
    const auto& refext = call.getNodeManager()
                             .getLangExtension<core::lang::ReferenceExtension>();

    if (!refext.isCallOfRefDeref(call)) {
        return OutOfBoundsResult::IsNotOutOfBounds;
    }

    auto call_hs = ctxt.resolveNodeAddress(call);
    int res = hat_out_of_bounds(ctxt.getHaskellContext(), call_hs);
    return static_cast<OutOfBoundsResult>(res);
}
```

As the analysis is to be only invoked on calls to the `ref_deref` operator, we import the reference language extension in lines 3 and 4 and ensure that the argument `call` is indeed a call to `ref_deref` in lines 6--8.

Note that we have to add the new Haskell module (defined by `OutOfBounds.hs`) to the list of modules in `insieme-hat.cabal`.

## Write Test Cases

Before actually implementing the analysis, setting up a few test cases is a good idea.
This eases the development process as we can immediately verify the correctness of our implementation.
Since we have already defined six cases the analysis should cover, we can morph these cases into unit tests.

Insieme uses the [Google Test Framework](https://github.com/google/googletest) (aka gtest) for most of its infrastructure testing -- therefore we also use gtest and integrate the tests into the infrastructure.

Important to note here is that we are not using the original input code for our tests, but the corresponding INSPIRE programs.
Otherwise we would have a strong coupling between the analysis module and Insieme's frontend.

Each test case for the out-of-bounds analysis goes into the file `out_of_bounds_analysis_test.cc` and will be structured as follows.

```{.cpp .numberLines}
TEST(OutOfBounds, Basic) {
    NodeManager mgr;
    IRBuilder builder(mgr);
    Context ctx;

    auto stmt = builder.parseStmt(
        "{"
        "   var ref<array<int<4>, 21>> a = ref_decl(type_lit(ref<array<int<4>, 21>>));"
        "   *ptr_subscript(ptr_from_array(a), 42);"
        "}"
    ).as<CompoundStmtPtr>();
    auto call = CompoundStmtAddress(stmt)[1].as<core::CallExprAddress>();

    ASSERT_EQ(OutOfBoundsResult::IsOutOfBounds, getOutOfBounds(ctx, call));
}
```

In line 1, `OutOfBounds` is the group this test case belongs to and `Basic` is its (unique) name.
The argument to the `parseStmt` call in line 6 is the INSPIRE program for this test.
The CallExpr of interest is extracted from it in line 12 and passed to the out-of-bounds analysis as input in line 14.
We expect the result to be `IsOutOfBounds`.

Note that the extracted CallExpr (input to the analysis) is a call to `ref_deref` as the problem of out-of-bound access occurs only upon dereferencing.

The file `out_of_bounds_analysis_test.cc` is placed inside the `test` sub-directory of the analysis module.
Insieme's build infrastructure will automatically create an executable allowing us to run all test cases of the out-of-bounds analysis in a single go.
We are also able to run only a subset of the test cases by using the `--gtest_filter` option provided by gtest.

## Implementation

The big picture of this analysis has been communicated in [Design].
Before starting with implementing the element count analysis we define ourselves the following helper functions.

```{.haskell .numberLines}
maybeToBool :: Maybe Bool -> Bool
maybeToBool = Data.Foldable.or

goesDown :: [Int] -> NodeAddress -> NodeAddress
goesDown l = foldr1 (.) $ goDown <$> reverse l
```

### Element Count Analysis

The element count analysis is a specialised DFA and therefore requires some boilerplate code:

```{.haskell .numberLines}
data ElementCountAnalysis = ElementCountAnalysis
  deriving (Typeable)

elementCountAnalysis :: DataFlowAnalysis ElementCountAnalysis
                                         (ValueTree SimpleFieldIndex
                                         (SymbolicFormulaSet BSet.Bound10))

elementCountAnalysis = (mkDataFlowAnalysis ElementCountAnalysis "EC" elementCount)

elementCount :: NodeAddress -> Solver.TypedVar (ValueTree SimpleFieldIndex
                                               (SymbolicFormulaSet BSet.Bound10))

elementCount addr = dataflowValue addr elementCountAnalysis ops
  where -- ...
```

This analysis covers the operators of the reference language extension and therefore uses `OperatorHandler`s (`ops`) to model their semantics.
The simplest case is encountered when dealing with a `ref_null` as the element count is zero.

```{.haskell .numberLines}
refNull = OperatorHandler cov dep val
  where
    cov a = isBuiltin a "ref_null"
    dep _ = []
    val a = toComposed $ BSet.singleton $ Ar.zero
```

Next, the operators `ref_cast`, `ref_reinterpret`, `ref_narrow`, and `ref_expand` simply forward the element count of their (first) argument as their semantics do not yield any modifications.

```{.haskell .numberLines}
noChange = OperatorHandler cov dep val
  where
    cov a = any (isBuiltin a) ["ref_cast", "ref_reinterpret" , "ref_narrow", "ref_expand"]
    dep _ = [Solver.toVar baseRefVar]
    val a = Solver.get a baseRefVar

    baseRefVar = elementCount $ goDown 1 $ goDown 2 addr
```

`ref_decl` and `ref_new` are used for the creation of arrays as we have observed at the beginning of this chapter.
From them we can extract the size of the array.

```{.haskell .numberLines}
creation = OperatorHandler cov dep val
  where
    cov a = any (isBuiltin a) ["ref_decl", "ref_new"] && isRefArray
    dep _ = Solver.toVar arraySize
    val a = Solver.get a arraySize

    arraySize = arithmeticValue $ goesDown [0,2,0,2,1,0] addr

isRefArray = maybeToBool $ isArrayType <$> (getReferencedType $ goDown 0 addr)
```

Note the check in line 3 as we only cover creations of arrays.
From the call to `ref_decl` or `ref_new` we invoke the arithmetic analysis on the node representing the size of the array and forward its result.
The location of the array size, here the node path `[0,2,0,2,1,0]` can be inferred from [Case1].

We also support scalars, their element count will be one.

```{.haskell .numberLines}
scalar = OperatorHandler cov dep val
  where
    cov a = any (isBuiltin a) ["ref_decl", "ref_new"] && not isRefArray
    dep _ = []
    val _ = toComposed $ BSet.singleton $ Ar.one
```

This completes our list of operator handlers.

```haskell
    ops = [refNull, noChange, creation, scalar]
```

### Getting the Array Index

The element count analysis provides us with a `SymbolicFormulaSet`.
In order to (easily) check for out-of-bounds accesses, the array index part should provide its result in a similar data structure.
From the reference analysis we get a `ReferenceSet`, where the containing `References` provide the `DataPath` from which the index can be extracted.
Therefore we now construct the parts responsible for bringing the result into the wanted data structure

```{.haskell .numberLines}
toDataPath :: Ref.Reference i -> Maybe (DP.DataPath i)
toDataPath (Ref.Reference _ dp) = Just dp
toDataPath _                    = Nothing

toPath :: DP.DataPath i -> Maybe [i]
toPath dp | DP.isInvalid dp = Nothing
toPath dp                   = Just $ DP.getPath dp

toFormula :: [SimpleFieldIndex] -> Maybe SymbolicFormula
toFormula []          = Just Ar.zero
toFormula (Index i:_) = Just $ Ar.mkConst $ fromIntegral i
toFormula _           = Nothing
```

Note that each of the functions returns a `Maybe` as there exists the possibility for the conversion to fail.
This would happen, for instance, if the received `ReferenceSet` contains a `NullReference`.

All that is left to dos, is to combine these parts using Kleisli composition, convert the results to `ArrayAccess`, and adjust the bound; yielding:

```{.haskell .numberLines}
data ArrayAccess = ArrayAccess SymbolicFormula
                 | InvalidArrayAccess
  deriving (Eq,Ord,Show,Generic,NFData)

convertArrayIndex :: Ref.ReferenceSet SimpleFieldIndex -> BSet.UnboundSet ArrayAccess
convertArrayIndex = BSet.changeBound
                  . BSet.map (maybe InvalidArrayAccess ArrayAccess)
                  . BSet.map (toDataPath >=> toPath >=> toFormula)
```

### Putting it Together

Now that we have the array size and the index used to access it in a usable format, we can put the two parts together, finally implementing `outOfBounds`.
`arraySize` and `arrayIndex` is processed as follows.

```{.haskell .numberLines}
(arrayIndex',ns) = Solver.resolve init
                 $ Ref.referenceValue
                 $ goDown 1 $ goDown 2 addr

arrayIndex :: BSet.UnboundSet ArrayAccess
arrayIndex = convertArrayIndex $ toValue arrayIndex'

(arraySize',ns') = Solver.resolve ns
                 $ elementCount
                 $ goDown 1 $ goDown 2 addr

arraySize :: SymbolicFormulaSet BSet.Unbound
arraySize = BSet.changeBound $ toValue arraySize'
```

First the `arrayIndex` is derived using the reference analysis and an initial `SolverState` (`init`).
The resulting `SolverState` (`ns`) is then used with the new element count analysis to derive `arraySize`.

Next, each element of `arrayIndex` is compared with each element of `arraySize` and checked for out-of-bounds access.

```{.haskell .numberLines}
isOutOfBound :: ArrayAccess -> SymbolicFormula -> Bool
isOutOfBound (ArrayAccess i)    s = Ar.numCompare i s /= Ar.NumLT
isOutOfBound InvalidArrayAccess _ = True

oobs = BSet.toList $ BSet.lift2 isOutOfBound arrayIndex arraySize
```

The result of the analysis is determined by whether `oobs` contains at least a single `True`.

```{.haskell .numberLines}
outOfBounds :: SolverState -> NodeAddress -> (OutOfBoundsResult,SolverState)
outOfBounds init addr = (result,ns')
  where
    result = case () of
        _ | BSet.isUniverse arrayIndex -> MayBeOutOfBounds
          | BSet.isUniverse arraySize  -> MayBeOutOfBounds
          | or oobs                    -> IsOutOfBounds
          | otherwise                  -> IsNotOutOfBounds
```

## Summary

To summarise the construction process a table is presented providing an overview of what files have been added / modified together with a short description.

----------------------------------------------------------------------------------------------------------------
File                             State      Description
-------------------------------- ---------- --------------------------------------------------------------------
`Adapter.hs`                     *modified* Now also contains Haskell export declarations for analysis function.

`OutOfBounds.hs`                 *added*    Defines the out-of-bounds analysis and its property space.

`insieme-hat.cabal`              *modified* Contains a list of all Haskell modules of \gls{hat}.

`out_of_bounds_analysis.cpp`     *added*    Wraps the imported out-of-bounds analysis function for C++.

`out_of_bounds_analysis.h`       *added*    Defines the analysis result and function prototype in C++.

`out_of_bounds_analysis_test.cc` *added*    Contains a unit test suite for the out-of-bounds analysis.
----------------------------------------------------------------------------------------------------------------

The adapter and `insieme-hat.cabal` are the only files that have been modified, all other files are simply added to the code base.
Note that it is not mandatory to have all FFI export declarations located in the adapter.
We could also have stated those directives in `OutOfBounds.hs`, yet we decided against it in order to keep all FFI export declarations (and their relevant logic) in one single (Haskell) module.
