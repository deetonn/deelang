
/*

 This file contains the typechecker for deelang.

 The typechecker is responsible for ensuring that the program is well-typed & applying local optimizations.

 Firstly, we add all builtin types to a lookup table. This lookup table will be used to ensure that
 all types are valid. We will also add all builtin functions to this lookup table.

 We will ensure this by doing multiple passes over the ast. Firstly, we will go over struct declarations
 and their contained fields, and add them into a lookup table. While doing this, we must compute
 the size of the struct, and ensure that the types of the fields are valid (doing this by looking up the types in the lookup table).
 
 Then, we will go over function declarations and their contained parameters, making sure that 
 the types of the parameters are valid (doing this by looking up the types in the lookup table).

 Finally, we will go over the function bodies, making sure that the types of the expressions are valid.
 We will also make sure that the return type of the function is valid (doing this by looking up the types in the lookup table).

 While doing this, we will also add all user-defined types and functions to the lookup table.

 While doing this, optimizations will be performed. For example, if a function is never called, it will be removed.
 Whenever an optimization is performed, we must notify the developer of said change, in case it was intentional,
 then provide the developer with a way to notify the compiler that the change was intentional.

 Possible optimizations (tree lowering) include:
    - Removing unused functions
    - Removing unused variables
    - Removing unused types
    - Removing unused fields
    - Removing unused imports
    - Removing unused structs
    - Removing unused enums
    - Removing unused constants
    - Removing unused methods
    - Removing unused traits
    - Removing unused impls
    - Removing unused type aliases

 This is all in an effort to add as little overhead as possible to the program.

 Possible optimizations other than removing dead code include:
    - Inlining functions
        - If a function is something like fn get_value() -> u32 { return 20 }, then we can
          replace all calls to get_value() with 20.
    - Evaluating constant expressions while type-checking
        - For example, (2 + 4) * 3 could be evaluated to 18 while type-checking
          This would avoid having to do this at runtime, and would also avoid having to
          store the result in a variable, which would also avoid having to allocate memory
          for the variable.
    - Algebraic simplification
        - For example, 2 * 3 could be simplified to 6 while type-checking
    - Strength Reduction
        - For example, 2 * 3 could be simplified to 2 + 2 + 2 while type-checking
          This avoid the need to do multiplication at runtime, which is slower than addition.
          We could also use bitshifting instead of mul/div. For example, "x" * 8 could be simplified
          to "x" << 3.
    - Constant Folding
        - For example, 2 + 2 could be simplified to 4 while type-checking
    - Constant Propagation
        - For example, const x = 50; x + y could be simplified to 50 + y while type-checking
          which could then be simplified to 50 + 2 while type-checking, which could then be
          simplified to 52 while type-checking.
    - Common Subexpression Elimination
        - For example: func((20 * 40) * x) can be optimized to be:
          let x = 20 * 40; func(x * 40). So we can avoid doing the addition over and over.
    - Loop Invariant Code Motion
        - For example: let x = 20 * 40; for i in 0..100 { func(x * 40); }
          can be optimized to be: let x = 20 * 40; let y = x * 40; for i in 0..100 { func(y); }
          So we can avoid doing the addition over and over.
    
    After each optimization, we must notify the developer of said change, in case it was intentional,
    then provide the developer with a way to notify the compiler that the change was intentional.

    Once standard IO has been implemented, we must make sure that we are not optimizing away
    any IO operations. For example, io::print("Hello, world!") should not be optimized away.
    However, something like: const hw = "Hello, world!"; io::print(hw); should have the "hw" variable
    inlined. This is because we can determine at compile-time that the value of "hw" will always be
    "Hello, world!", so we can just inline it. If the value of "hw" was not known at compile-time,
    then we would not be able to inline it.

 After each loop of the Ast, we must check if any changes were made. If no changes were made, then we
 can stop looping. If changes were made, then we must loop again in case there were any new optimizations
 enable by the previous ones.
    
*/