/* match functions by unifying with arguments 
    and infering the result
*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */

/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */

/* TODO: add statements types and their type checking */
/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

/* local variable definition
    Example:
        let v = 3 in ...
 */
typeStatement(lvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T). /* make sure we have an infered type */

/* global function definition */
typeStatement(gfLet(Name, T, Args, Code), unit):-
    atom(Name), /* make sure we have a func name */
    typeExp(Args, T), /* infer types of the params hopefully */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add func to database */

/* if statement type */
typeStatement(if(Cond, CodeT, CodeF), T):-
    typeExp(Cond, boolean), /* condition return type */
    typeExp(CodeT, T), /* infer return type of "if true" code */
    typeExp(CodeF, T), /* infer return type of "if false" code */
    bType(T). /* make sure we have an infered type */

/* while loop statement type */
typeStatement(while(Cond, Code), T):-
    typeExp(Cond, boolean), /* condition return type */
    typeExp(Code, T), /* infer return type of code */
    bType(T). /* make sure we have an infered type */

/* while loop statement type */
typeStatement(for(Init, Cond, Step, Code), T):-
    typeExp(Init, T), /* initialize statement return type */
    typeExp(Cond, boolean), /* condition return type */
    typeExp(Step, T), /* step statement return type */
    typeExp(Code, T), /* infer return type of code */
    bType(T). /* make sure we have an infered type & return it */

typeStatement(T, T).

/* Code is simply a list of statements. The type is 
    the type of the last statement (or return type?)
*/
typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(boolean).
bType(int).
bType(float).
bType(string).
bType(char).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best way to do this is in your top predicate
*/

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

fType(iplus, [int,int,int]).
fType(fplus, [float, float, float]).

fType(icompare, [int,int,int]).
fType(fcompare, [float, float, int]).
fType(scompare, [string,string,int]).

ftype(idouble, [int,int]).
ftype(fdouble, [float, float]).

ftype(charAt, [string, int, char]).
ftype(findChar, [string, char, int]).

fType(fToInt, [float,int]).
fType(iToFloat, [int,float]).
fType(print, [_X, unit]). /* simple print */

/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().
