
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

use std::cell::{Cell, RefCell, Ref};

use crate::parser::ast_node::{
  FunctionDeclarationFacts,
  AssignmentFacts,
  TraitFacts, TypeAliasFacts
};

use crate::parser::ast_node::AstNode;
use crate::tokenizer::SourceLocation;

#[derive(Debug, Clone)]
pub enum Declaration {
  Variable(AssignmentFacts),
  Function(FunctionDeclarationFacts),
  Trait(TraitFacts),
  Typedef(TypeAliasFacts),

  Nothing,
}

pub struct CheckerError {
  pub message: String,
  pub location: SourceLocation,
}

type CheckerResult = Result<AstNode, CheckerError>;

impl CheckerError {
  pub fn new(message: String, 
    location: &SourceLocation) -> CheckerError {
    return CheckerError {
      message,
      location: location.clone()
    }
  }
}

pub enum CheckStatus {
  // Okay just signifies that the expression/statement is ok.
  Okay,
  Optimized(AstNode),
  Failed(CheckerError),
  Done,
}

pub struct CheckedDeclaration {
  decl: Declaration,
}

impl CheckedDeclaration {
  pub fn new(declaration: Declaration) -> CheckedDeclaration {
    Self {
      decl: declaration
    }
  }
}

pub struct TypeCheckerState {
  pub ast: Vec<AstNode>,
  pub position: Cell<usize>,

  pub declarations: RefCell<Vec<CheckedDeclaration>>
}

impl TypeCheckerState {
  pub fn new(ast: Vec<AstNode>) -> TypeCheckerState {
    TypeCheckerState { 
      ast, 
      position: Cell::new(0),
      declarations: RefCell::new(Vec::new()) }
  }

  pub fn advance(&self) -> Option<&AstNode> {
    let current_pos = self.position.get();
    if current_pos >= self.ast.len() {
      return None;
    }
    self.position.set(current_pos + 1);
    Some(&self.ast[self.position.get()])
  }

  pub fn walk_back(&self, by: usize) {
    self.position.set(self.position.get() - by);
  }

  pub fn get_declaration(&self, name: &String) -> Ref<'_, Declaration> {
    let interior = self.declarations.borrow();
    let data = Ref::map(interior, |x| {
      for item in x {
        match &item.decl {
          Declaration::Function(facts) => {
            if facts.name == *name {
              return &item.decl;
            }
          },
          Declaration::Variable(facts) => {
            if facts.name == *name {
              return &item.decl;
            }
          },
          Declaration::Typedef(facts) => {
            if facts.name == *name {
              return &item.decl;
            }
          },
          Declaration::Trait(facts) => {
            if facts.name == *name {
              return &item.decl;
            }
          },
          Declaration::Nothing => break
        }
      }
      
      &Declaration::Nothing
    });

    data
  }
}

pub fn just_walk_declarations(state: &TypeCheckerState) {
  while let Some(statement) = state.advance() {
    match statement {
      AstNode::Assignment(assignment) => {
        state.declarations.borrow_mut().push(CheckedDeclaration {
          decl: Declaration::Variable(
            assignment.clone()
          )
        })
      },
      AstNode::FunctionDeclaration(function) => {
        state.declarations.borrow_mut().push(CheckedDeclaration {
          decl: Declaration::Function(
            function.clone()
          )
        })
      },
      AstNode::TraitDeclaration(trait_decl) => {
        state.declarations.borrow_mut().push(CheckedDeclaration {
          decl: Declaration::Trait(
            trait_decl.clone()
          )
        })
      },
      AstNode::TypeAlias(alias) => {
        state.declarations.borrow_mut().push(CheckedDeclaration {
          decl: Declaration::Typedef(
            alias.clone()
          )
        })
      },
      AstNode::Struct(_) => todo!("implement struct"),
      _ => continue
    }
  }

  state.walk_back(state.position.get());
}

pub fn visit_assignment(
  state: &TypeCheckerState,
  facts: &AssignmentFacts,
) -> CheckerResult {
  // we need to make sure whatever is being assigned to
  // this exists.

  let expr_type = facts.expression.try_evaluate_type();

  if let Some(type_info) = expr_type {
    // FIXME: This .clone() is a heavy one, what the fuck is Ref<'_, ...> ??? 
    //        How do i dereference it???? So fucking baffling.
    //        The size of Declaration is 248 at the time of writing this,
    //        so cloning DOES NOT seem like a good idea.
    let type_exists = match state.get_declaration(&type_info.name).clone() {
      Declaration::Nothing => false,
      _ => true
    };
    if !type_exists {
      return Err(
        CheckerError::new(
          format!("The type \"{}\" does not exist.", type_info.name),
          &facts.expression.get_source_location(),
        )
      )
    }
  }

  // At this point, the type exists, now we just verify they
  // match.
  if let Some(actual_type) = facts.expression.try_evaluate_type() {
    if let Some(requested_type) = &facts.type_info {
      if &actual_type != requested_type {
        return Err(
          CheckerError::new(
            format!("cannot assign an entity of type \"{}\" to a variable typed \"{}\"",
            actual_type.name, requested_type.name),
            &facts.expression.get_source_location()
          )
        )
      }
    }
  }

  let actual_type = facts.expression.try_evaluate_type();

  return Ok(
    AstNode::Assignment(
      AssignmentFacts {
        name: facts.name.clone(),
        type_info: actual_type.clone(),
        constant: facts.constant,
        expression: dyn_clone::clone_box(facts.expression.as_ref()),
      }
    )
  )
}

pub fn visit_expressions(state: &TypeCheckerState) -> CheckStatus {
  while let Some(statement) = state.advance() {
    let result = match statement {
      AstNode::Assignment(facts) => {
        visit_assignment(state, facts)
      },
      _ => todo!()
    };
  }

  return CheckStatus::Done;
}

// This function will walk the AST until all types have been checked
// and all possible optimizations have been applied.
pub fn walk_ast(state: &TypeCheckerState) -> Vec<AstNode> {
  // walk declarations, so we can store them for lookups when
  // checking expressions.
  just_walk_declarations(state);

  todo!("type checker");
}