% SMTLib parser 

:- use_module(library(dcg/basics)).

% Entry point for parsing SMT-LIB-like expressions
smtlib(Tree) --> whites, expression(Tree), whites.

% Handle expressions: Boolean, arithmetic, quantifiers, assertions, libraries, theories, logics, etc.
expression(Tree) --> comparison_expr(Tree).
expression(Tree) --> boolean_expr(Tree).
expression(Tree) --> quantifier(Tree).
expression(Tree) --> assertion(Tree).
expression(Tree) --> library(Tree).
expression(Tree) --> theory(Tree).
expression(Tree) --> logic(Tree).
expression(Tree) --> arithmetic_expr(Tree).
expression(Tree) --> let_expr(Tree).
expression(Tree) --> if_expr(Tree).  % Updated to parse 'ite' expressions
expression(Tree) --> function_application(Tree).
expression(Tree) --> sort_expr(Tree).

% Fallback for unknown expressions, outputs an error message
expression(error('Unknown expression', Expr)) --> 
    ['('], whites, unknown_expr(Expr), whites, [')'], 
    { format('Error: Unknown expression "~w" encountered.~n', [Expr]) }.

% Logic expressions with expansions (unquoted atoms for snake case)
logic(logic(HumanReadable)) --> 
    ['(', 'set-logic'], whites, atom_string(LogicName), whites, [')'], 
    { expand_logic(LogicName, HumanReadable) }.

% Arithmetic Expressions in infix form
arithmetic_expr(Left + Right) --> ['('], whites, ['+'], whites, expression(Left), whites, expression(Right), whites, [')'].
arithmetic_expr(Left - Right) --> ['('], whites, ['-'], whites, expression(Left), whites, expression(Right), whites, [')'].
arithmetic_expr(Left * Right) --> ['('], whites, ['*'], whites, expression(Left), whites, expression(Right), whites, [')'].
arithmetic_expr(Left / Right) --> ['('], whites, ['/'], whites, expression(Left), whites, expression(Right), whites, [')'].
arithmetic_expr(Var) --> variable(Var).
arithmetic_expr(Const) --> constant(Const).

% Comparison Expressions
comparison_expr(Left = Right) --> ['('], whites, ['='], whites, expression(Left), whites, expression(Right), whites, [')'].
comparison_expr(Left > Right) --> ['('], whites, ['>'], whites, expression(Left), whites, expression(Right), whites, [')'].
comparison_expr(Left < Right) --> ['('], whites, ['<'], whites, expression(Left), whites, expression(Right), whites, [')'].
comparison_expr(Left >= Right) --> ['('], whites, ['>='], whites, expression(Left), whites, expression(Right), whites, [')'].
comparison_expr(Left =< Right) --> ['('], whites, ['<='], whites, expression(Left), whites, expression(Right), whites, [')'].

% Boolean Expressions using Prolog default syntax
boolean_expr(Left, Right) --> ['('], whites, ['and'], whites, expression(Left), whites, expression(Right), whites, [')'], { Left = Left, Right = Right }.
boolean_expr(Left ; Right) --> ['('], whites, ['or'], whites, expression(Left), whites, expression(Right), whites, [')'].
boolean_expr(Left => Right) --> ['('], whites, ['=>'], whites, expression(Left), whites, expression(Right), whites, [')'].
boolean_expr(not(Expr)) --> ['('], whites, ['not'], whites, expression(Expr), whites, [')'].
boolean_expr(true)  --> ['true'], whites.
boolean_expr(false) --> ['false'], whites.

% Quantifiers (forall, exists) as patterns
quantifier(forall(Vars, Expr)) --> ['('], whites, ['forall'], whites, ['('], whites, variable_list(Vars), whites, [')'], whites, expression(Expr), whites, [')'].
quantifier(exists(Vars, Expr)) --> ['('], whites, ['exists'], whites, ['('], whites, variable_list(Vars), whites, [')'], whites, expression(Expr), whites, [')'].

% Assertions (assert, check-sat, declare-const, declare-fun)
assertion(assert(Expr)) --> ['('], whites, ['assert'], whites, expression(Expr), whites, [')'].
assertion(check_sat) --> ['(', 'check-sat', ')'], whites.
assertion(declare_const(Var, Sort)) --> ['(', 'declare-const'], whites, variable(Var), whites, sort(Sort), whites, [')'].
assertion(declare_fun(Fun, ArgSorts, RetSort)) --> 
    ['(', 'declare-fun'], whites, variable(Fun), whites, ['('], whites, sort_list(ArgSorts), whites, [')'], whites, sort(RetSort), whites, [')'].

% Library expressions
library(include(File)) --> ['(', 'include'], whites, atom_string(File), whites, [')'].
library(declare_sort(Sort, Arity)) --> 
    ['(', 'declare-sort'], whites, atom_string(Sort), whites, number(Arity), whites, [')'].
library(define_fun(Fun, Args, Body, Sort)) -->
    ['(', 'define-fun'], whites, atom_string(Fun), whites, ['('], whites, variable_list(Args), whites, [')'], whites,
    expression(Body), whites, sort(Sort), whites, [')'].

% Theory expressions
theory(theory(Name)) --> ['(', 'set-logic'], whites, atom_string(Name), whites, [')'].
theory(theory(Name, Extensions)) --> ['(', 'set-logic'], whites, atom_string(Name), whites, theory_extensions(Extensions), whites, [')'].

theory_extensions([Ext | Rest]) --> whites, atom_string(Ext), whites, theory_extensions(Rest).
theory_extensions([]) --> [].

% Let Expressions
let_expr(let(Bindings, Body)) --> ['(', 'let'], whites, ['('], whites, let_bindings(Bindings), whites, [')'], whites, expression(Body), whites, [')'].

% Let bindings: multiple bindings inside 'let'
let_bindings([Var = Val | Rest]) --> ['('], whites, variable(Var), whites, expression(Val), whites, [')'], whites, let_bindings(Rest).
let_bindings([]) --> [].

% If-Then-Else (ite) Expressions, but output as if/3
if_expr(if(Cond, ThenExpr, ElseExpr)) --> 
    ['('], whites, ['ite'], whites, expression(Cond), whites, expression(ThenExpr), whites, expression(ElseExpr), whites, [')'].

% Function Applications (custom user-defined functions)
function_application(App) --> ['('], whites, variable(Fun), whites, function_args(Args), whites, [')'], 
                               { App =.. [Fun | Args] }.

% Function arguments (list of expressions)
function_args([Arg | Args]) --> expression(Arg), whites, function_args(Args).
function_args([]) --> [].

% Sorts (Int, Bool, etc.)
sort_expr(Sort) --> sort(Sort), whites.

% Helper rules for various components

% Sorts (Int, Bool, etc.)
sort(int) --> ['Int'], whites.
sort(bool) --> ['Bool'], whites.

% Variable: an identifier (atom)
variable(Var) --> atom_string(Var), whites, { atom(Var) }.

% Constant: a number
constant(Const) --> number(Const), whites, { number(Const) }.

% Fallback for unknown expressions inside parentheses
unknown_expr(Expr) --> atom_string(Expr), { format('Unknown expression found: ~w~n', [Expr]) }.

% Example query:
% ?- phrase(smtlib(Tree), "(set-logic UNKNOWN_LOGIC)").
%
% Expected output:
% Tree = error('Unknown expression', 'UNKNOWN_LOGIC').

