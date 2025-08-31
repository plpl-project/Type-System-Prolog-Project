% =============================================================================
% COMPLETE AFFINE/RELEVANT INTERPRETER WITH ALL FEATURES (FIXED)
% =============================================================================

% -----------------------------------------------------------------------------
% AST DEFINITIONS
% -----------------------------------------------------------------------------

% Programs and Statements
% prog(Statements)
% var_decl(VarType, Name, Expr)
% fun_def(Name, Params, Body) where Params = [param(VarType, Name, Type), ...]
% record_def(Name, Fields) where Fields = [field(Name, Type), ...]
% expr_stmt(Expr)

% Expressions
% var(Name)
% const(Value) - numbers
% bool(Value) - true/false
% string(Value) - strings
% fun_call(Name, Args)
% add(Left, Right)
% not(Expr)
% if_then_else(Cond, Then, Else)
% record_create(RecordType, FieldValues) where FieldValues = [field_val(Name, Expr), ...]
% field_access(Expr, Field)
% list_create(Elements)

% Types
% int, string, boolean, record(Name), list(Type)

% -----------------------------------------------------------------------------
% ENHANCED PARSER
% -----------------------------------------------------------------------------

parse_program(Tokens, prog(Statements)) :-
    parse_statements(Tokens, Statements).

parse_statements([], []).
parse_statements(Tokens, [Stmt|Stmts]) :-
    parse_statement(Tokens, Stmt, Rest),
    parse_statements(Rest, Stmts).

% Parse variable declarations: affine x = 5
parse_statement([VarType, Name, '=', Value | Rest], var_decl(VarType, Name, Expr), Rest) :-
    member(VarType, [affine, relevant]),
    atom(Name),
    parse_expression([Value], Expr, []).

% Parse function definitions: fun f(affine x: int, relevant y: string) = x + 1
parse_statement([fun, FunName, '(' | Rest], fun_def(FunName, Params, Body), FinalRest) :-
    atom(FunName),
    parse_params(Rest, Params, [')', '=' | BodyTokens]),
    parse_expression(BodyTokens, Body, FinalRest).

% Parse record definitions: record Person { name: string, age: int }
parse_statement([record, RecordName, '{' | Rest], record_def(RecordName, Fields), FinalRest) :-
    atom(RecordName),
    parse_fields(Rest, Fields, ['}' | FinalRest]).

% Parse expression statements
parse_statement(Tokens, expr_stmt(Expr), Rest) :-
    parse_expression(Tokens, Expr, Rest).

% Parse function parameters
parse_params([], [], Rest) :- Rest = [].
parse_params([')' | Rest], [], [')', '=' | Rest]).
parse_params([VarType, Name, ':', Type | Rest], [param(VarType, Name, Type) | Params], FinalRest) :-
    member(VarType, [affine, relevant]),
    atom(Name),
    atom(Type),
    (   Rest = [',' | MoreRest] ->
        parse_params(MoreRest, Params, FinalRest)
    ;   Params = [],
        FinalRest = Rest
    ).

% Parse record fields
parse_fields(['}' | Rest], [], ['}' | Rest]).
parse_fields([FieldName, ':', Type | Rest], [field(FieldName, Type) | Fields], FinalRest) :-
    atom(FieldName),
    atom(Type),
    (   Rest = [',' | MoreRest] ->
        parse_fields(MoreRest, Fields, FinalRest)
    ;   Fields = [],
        FinalRest = Rest
    ).

% Parse expressions
parse_expression([Value], Expr, []) :-
    make_simple_expr(Value, Expr).

parse_expression([Left, '+', Right | Rest], add(LeftExpr, RightExpr), Rest) :-
    make_simple_expr(Left, LeftExpr),
    make_simple_expr(Right, RightExpr).

parse_expression(['!', Expr | Rest], not(ExprParsed), Rest) :-
    make_simple_expr(Expr, ExprParsed).

parse_expression([if, Cond, then, Then, else, Else | Rest], if_then_else(CondExpr, ThenExpr, ElseExpr), Rest) :-
    make_simple_expr(Cond, CondExpr),
    make_simple_expr(Then, ThenExpr),
    make_simple_expr(Else, ElseExpr).

% Function calls: f(a, b, c)
parse_expression([FunName, '(' | Rest], fun_call(FunName, Args), FinalRest) :-
    atom(FunName),
    parse_args(Rest, Args, [')' | FinalRest]).

% Record creation: Person { name = "John", age = 30 }
parse_expression([RecordType, '{' | Rest], record_create(RecordType, FieldVals), FinalRest) :-
    atom(RecordType),
    parse_field_values(Rest, FieldVals, ['}' | FinalRest]).

% Field access: person.name
parse_expression([Expr, '.', Field | Rest], field_access(ExprParsed, Field), Rest) :-
    make_simple_expr(Expr, ExprParsed),
    atom(Field).

% Lists: [1, 2, 3]
parse_expression(['[' | Rest], list_create(Elements), FinalRest) :-
    parse_list_elements(Rest, Elements, [']' | FinalRest]).

parse_expression(Tokens, Expr, Rest) :-
    Tokens = [First | RestTokens],
    make_simple_expr(First, Expr),
    Rest = RestTokens.

% Parse function arguments
parse_args([')' | Rest], [], [')' | Rest]).
parse_args([Arg | Rest], [ArgExpr | Args], FinalRest) :-
    make_simple_expr(Arg, ArgExpr),
    (   Rest = [',' | MoreRest] ->
        parse_args(MoreRest, Args, FinalRest)
    ;   Args = [],
        FinalRest = Rest
    ).

% Parse record field values
parse_field_values(['}' | Rest], [], ['}' | Rest]).
parse_field_values([Field, '=', Value | Rest], [field_val(Field, ValueExpr) | FieldVals], FinalRest) :-
    atom(Field),
    make_simple_expr(Value, ValueExpr),
    (   Rest = [',' | MoreRest] ->
        parse_field_values(MoreRest, FieldVals, FinalRest)
    ;   FieldVals = [],
        FinalRest = Rest
    ).

% Parse list elements
parse_list_elements([']' | Rest], [], [']' | Rest]).
parse_list_elements([Elem | Rest], [ElemExpr | Elements], FinalRest) :-
    make_simple_expr(Elem, ElemExpr),
    (   Rest = [',' | MoreRest] ->
        parse_list_elements(MoreRest, Elements, FinalRest)
    ;   Elements = [],
        FinalRest = Rest
    ).

% Create simple expressions
make_simple_expr(Value, const(Value)) :- number(Value).
make_simple_expr(Value, string(Value)) :- string(Value).
make_simple_expr(true, bool(true)).
make_simple_expr(false, bool(false)).
make_simple_expr(Name, var(Name)) :- atom(Name).

% -----------------------------------------------------------------------------
% ENHANCED ENVIRONMENT MANAGEMENT
% -----------------------------------------------------------------------------

empty_env([]).

% Bind variable
bind_var(Name, Type, VarType, [], [var_binding(Name, Type, VarType, unused)]).
bind_var(Name, Type, VarType, [var_binding(Name, _, _, _)|Rest], [var_binding(Name, Type, VarType, unused)|Rest]).
bind_var(Name, Type, VarType, [B|Rest], [B|NewRest]) :-
    B \= var_binding(Name, _, _, _),
    bind_var(Name, Type, VarType, Rest, NewRest).

% Bind function
bind_function(Name, Params, Body, [], [fun_binding(Name, Params, Body)]).
bind_function(Name, Params, Body, [fun_binding(Name, _, _)|Rest], [fun_binding(Name, Params, Body)|Rest]).
bind_function(Name, Params, Body, [B|Rest], [B|NewRest]) :-
    B \= fun_binding(Name, _, _),
    bind_function(Name, Params, Body, Rest, NewRest).

% Bind record type
bind_record(Name, Fields, [], [record_binding(Name, Fields)]).
bind_record(Name, Fields, [record_binding(Name, _)|Rest], [record_binding(Name, Fields)|Rest]).
bind_record(Name, Fields, [B|Rest], [B|NewRest]) :-
    B \= record_binding(Name, _),
    bind_record(Name, Fields, Rest, NewRest).

% Lookup variable
lookup_var(Name, [var_binding(Name, Type, VarType, Status)|_], Type, VarType, Status).
lookup_var(Name, [B|Rest], Type, VarType, Status) :-
    B \= var_binding(Name, _, _, _),
    lookup_var(Name, Rest, Type, VarType, Status).

% Lookup function
lookup_function(Name, [fun_binding(Name, Params, Body)|_], Params, Body).
lookup_function(Name, [B|Rest], Params, Body) :-
    B \= fun_binding(Name, _, _),
    lookup_function(Name, Rest, Params, Body).

% Lookup record
lookup_record(Name, [record_binding(Name, Fields)|_], Fields).
lookup_record(Name, [B|Rest], Fields) :-
    B \= record_binding(Name, _),
    lookup_record(Name, Rest, Fields).

% Mark variable as used
mark_used(Name, [], []) :-
    throw(error(undefined_variable(Name))).
mark_used(Name, [var_binding(Name, Type, VarType, unused)|Rest], [var_binding(Name, Type, VarType, used(1))|Rest]).
mark_used(Name, [var_binding(Name, Type, VarType, used(Count))|Rest], [var_binding(Name, Type, VarType, used(NewCount))|Rest]) :-
    NewCount is Count + 1.
mark_used(Name, [B|Rest], [B|NewRest]) :-
    B \= var_binding(Name, _, _, _),
    mark_used(Name, Rest, NewRest).

% -----------------------------------------------------------------------------
% ENHANCED TYPE CHECKER
% -----------------------------------------------------------------------------

type_check_program(prog(Statements), FinalEnv) :-
    empty_env(InitialEnv),
    type_check_statements(Statements, InitialEnv, FinalEnv),
    check_final_usage_constraints(FinalEnv).

type_check_statements([], Env, Env).
type_check_statements([Stmt|Stmts], Env, FinalEnv) :-
    type_check_statement(Stmt, Env, Env1),
    type_check_statements(Stmts, Env1, FinalEnv).

% Type check variable declaration
type_check_statement(var_decl(VarType, Name, Expr), Env, NewEnv) :-
    type_check_expression(Expr, Env, ExprType, Env1),
    bind_var(Name, ExprType, VarType, Env1, NewEnv).

% Type check function definition - FIXED: Use Body parameter
type_check_statement(fun_def(Name, Params, Body), Env, NewEnv) :-
    create_function_env(Params, Env, FunEnv),
    type_check_expression(Body, FunEnv, _ReturnType, _FinalFunEnv),
    bind_function(Name, Params, Body, Env, NewEnv).

% Type check record definition
type_check_statement(record_def(Name, Fields), Env, NewEnv) :-
    bind_record(Name, Fields, Env, NewEnv).

% Type check expression statement
type_check_statement(expr_stmt(Expr), Env, NewEnv) :-
    type_check_expression(Expr, Env, _Type, NewEnv).

% Create environment for function parameters
create_function_env([], Env, Env).
create_function_env([param(VarType, Name, Type)|Params], Env, FinalEnv) :-
    parse_type(Type, ParsedType),
    bind_var(Name, ParsedType, VarType, Env, Env1),
    create_function_env(Params, Env1, FinalEnv).

% Type check expressions
type_check_expression(var(Name), Env, Type, NewEnv) :-
    lookup_var(Name, Env, Type, VarType, Status),
    (   (VarType = affine, Status = used(_)) ->
        throw(error(affine_violation(Name)))
    ;   mark_used(Name, Env, NewEnv)
    ).

type_check_expression(const(Value), Env, int, Env) :-
    number(Value).

type_check_expression(string(_Value), Env, string, Env).

type_check_expression(bool(_Value), Env, boolean, Env).

type_check_expression(add(Left, Right), Env, int, NewEnv) :-
    type_check_expression(Left, Env, int, Env1),
    type_check_expression(Right, Env1, int, NewEnv).

type_check_expression(not(Expr), Env, boolean, NewEnv) :-
    type_check_expression(Expr, Env, boolean, NewEnv).

type_check_expression(if_then_else(Cond, Then, Else), Env, Type, NewEnv) :-
    type_check_expression(Cond, Env, boolean, Env1),
    type_check_expression(Then, Env1, Type, Env2),
    type_check_expression(Else, Env2, Type, NewEnv).

type_check_expression(fun_call(Name, Args), Env, int, NewEnv) :-
    lookup_function(Name, Env, Params, _Body),
    type_check_function_call(Args, Params, Env, NewEnv).

type_check_expression(record_create(RecordType, FieldVals), Env, record(RecordType), NewEnv) :-
    lookup_record(RecordType, Env, Fields),
    type_check_record_creation(FieldVals, Fields, Env, NewEnv).

type_check_expression(field_access(Expr, Field), Env, FieldType, NewEnv) :-
    type_check_expression(Expr, Env, record(RecordType), Env1),
    lookup_record(RecordType, Env1, Fields),
    member(field(Field, FieldType), Fields),
    NewEnv = Env1.

type_check_expression(list_create(Elements), Env, list(ElementType), NewEnv) :-
    type_check_list_elements(Elements, ElementType, Env, NewEnv).

% Helper predicates for type checking
type_check_function_call([], [], Env, Env).
type_check_function_call([Arg|Args], [param(_VarType, _Name, Type)|Params], Env, NewEnv) :-
    parse_type(Type, ParsedType),
    type_check_expression(Arg, Env, ParsedType, Env1),
    type_check_function_call(Args, Params, Env1, NewEnv).

type_check_record_creation([], [], Env, Env).
type_check_record_creation([field_val(Name, Expr)|FieldVals], Fields, Env, NewEnv) :-
    member(field(Name, Type), Fields),
    type_check_expression(Expr, Env, Type, Env1),
    select(field(Name, Type), Fields, RestFields),
    type_check_record_creation(FieldVals, RestFields, Env1, NewEnv).

type_check_list_elements([], int, Env, Env). % Default to int for empty lists
type_check_list_elements([Elem|Elems], Type, Env, NewEnv) :-
    type_check_expression(Elem, Env, Type, Env1),
    type_check_uniform_list(Elems, Type, Env1, NewEnv).

type_check_uniform_list([], _Type, Env, Env).
type_check_uniform_list([Elem|Elems], Type, Env, NewEnv) :-
    type_check_expression(Elem, Env, Type, Env1),
    type_check_uniform_list(Elems, Type, Env1, NewEnv).

% Parse type atoms to internal representation
parse_type(int, int).
parse_type(string, string).
parse_type(boolean, boolean).
parse_type(Type, record(Type)) :- 
    atom(Type),
    \+ member(Type, [int, string, boolean]).

% Check final usage constraints
check_final_usage_constraints([]).
check_final_usage_constraints([var_binding(Name, _Type, VarType, Status)|Rest]) :-
    check_usage_constraint(Name, VarType, Status),
    check_final_usage_constraints(Rest).
check_final_usage_constraints([B|Rest]) :-
    B \= var_binding(_, _, _, _),
    check_final_usage_constraints(Rest).

check_usage_constraint(Name, relevant, unused) :-
    throw(error(relevant_unused(Name))).
check_usage_constraint(_Name, relevant, used(_Count)).
check_usage_constraint(_Name, affine, unused).
check_usage_constraint(_Name, affine, used(Count)) :-
    Count =< 1.

% -----------------------------------------------------------------------------
% ENHANCED EVALUATOR
% -----------------------------------------------------------------------------

evaluate_program(prog(Statements), Result) :-
    empty_env(InitialEnv),
    evaluate_statements(Statements, InitialEnv, _FinalEnv, Result).

evaluate_statements([], Env, Env, null).
evaluate_statements([Stmt], Env, NewEnv, Result) :-
    evaluate_statement(Stmt, Env, NewEnv, Result).
evaluate_statements([Stmt|Stmts], Env, FinalEnv, Result) :-
    Stmts \= [],
    evaluate_statement(Stmt, Env, Env1, _StmtResult),
    evaluate_statements(Stmts, Env1, FinalEnv, Result).

evaluate_statement(var_decl(VarType, Name, Expr), Env, NewEnv, null) :-
    evaluate_expression(Expr, Env, Value),
    bind_var(Name, Value, VarType, Env, NewEnv).

evaluate_statement(fun_def(Name, Params, Body), Env, NewEnv, null) :-
    bind_function(Name, Params, Body, Env, NewEnv).

evaluate_statement(record_def(Name, Fields), Env, NewEnv, null) :-
    bind_record(Name, Fields, Env, NewEnv).

evaluate_statement(expr_stmt(Expr), Env, Env, Value) :-
    evaluate_expression(Expr, Env, Value).

% Evaluate expressions
evaluate_expression(var(Name), Env, Value) :-
    lookup_var(Name, Env, Value, _VarType, _Status).

evaluate_expression(const(Value), _Env, Value).
evaluate_expression(string(Value), _Env, Value).
evaluate_expression(bool(Value), _Env, Value).

evaluate_expression(add(Left, Right), Env, Result) :-
    evaluate_expression(Left, Env, LeftVal),
    evaluate_expression(Right, Env, RightVal),
    Result is LeftVal + RightVal.

evaluate_expression(not(Expr), Env, Result) :-
    evaluate_expression(Expr, Env, Value),
    (   Value = true -> Result = false
    ;   Value = false -> Result = true
    ).

evaluate_expression(if_then_else(Cond, Then, Else), Env, Result) :-
    evaluate_expression(Cond, Env, CondValue),
    (   CondValue = true ->
        evaluate_expression(Then, Env, Result)
    ;   evaluate_expression(Else, Env, Result)
    ).

evaluate_expression(fun_call(Name, Args), Env, Result) :-
    lookup_function(Name, Env, Params, Body),
    evaluate_args(Args, Env, ArgValues),
    create_call_env(Params, ArgValues, Env, CallEnv),
    evaluate_expression(Body, CallEnv, Result).

evaluate_expression(record_create(RecordType, FieldVals), Env, record_value(RecordType, Values)) :-
    evaluate_field_values(FieldVals, Env, Values).

evaluate_expression(field_access(Expr, Field), Env, Value) :-
    evaluate_expression(Expr, Env, record_value(_RecordType, FieldValues)),
    member(field_val(Field, Value), FieldValues).

evaluate_expression(list_create(Elements), Env, List) :-
    evaluate_list_elements(Elements, Env, List).

% Helper predicates for evaluation
evaluate_args([], _Env, []).
evaluate_args([Arg|Args], Env, [Value|Values]) :-
    evaluate_expression(Arg, Env, Value),
    evaluate_args(Args, Env, Values).

create_call_env([], [], Env, Env).
create_call_env([param(VarType, Name, _Type)|Params], [Value|Values], Env, FinalEnv) :-
    bind_var(Name, Value, VarType, Env, Env1),
    create_call_env(Params, Values, Env1, FinalEnv).

evaluate_field_values([], _Env, []).
evaluate_field_values([field_val(Name, Expr)|FieldVals], Env, [field_val(Name, Value)|Values]) :-
    evaluate_expression(Expr, Env, Value),
    evaluate_field_values(FieldVals, Env, Values).

evaluate_list_elements([], _Env, []).
evaluate_list_elements([Elem|Elems], Env, [Value|Values]) :-
    evaluate_expression(Elem, Env, Value),
    evaluate_list_elements(Elems, Env, Values).

% -----------------------------------------------------------------------------
% MAIN INTERFACE
% -----------------------------------------------------------------------------

run_program(Input, Result) :-
    catch(
        (   parse_program(Input, AST),
            type_check_program(AST, _FinalEnv),
            evaluate_program(AST, Value),
            Result = success(Value)
        ),
        Error,
        Result = error(Error)
    ).

% -----------------------------------------------------------------------------
% SIMPLE TESTS (Start with these)
% -----------------------------------------------------------------------------

test_simple :-
    writeln('=== SIMPLE TESTS ==='),
    
    % Test 1: Basic variable (should work like before)
    writeln('Test 1: Basic affine variable'),
    run_program([affine, x, '=', 5, x], R1),
    format('Result: ~w~n', [R1]),
    
    % Test 2: Simple addition (if parsing works)
    writeln('Test 2: Simple addition'),
    run_program([3, '+', 4], R2),
    format('Result: ~w~n', [R2]),
    
    writeln('=== END SIMPLE TESTS ===').

% -----------------------------------------------------------------------------
% COMPREHENSIVE TEST SUITE
% -----------------------------------------------------------------------------

test_all :-
    writeln('=== COMPREHENSIVE TEST SUITE ==='),
    test_variables,
    test_expressions,
    test_usage_constraints,
    writeln('=== ALL TESTS COMPLETED ===').

test_variables :-
    writeln('--- Testing Variables ---'),
    
    % Test 1: Affine variable used once
    writeln('Test 1: Affine variable used once'),
    run_program([affine, x, '=', 10, x], R1),
    format('Result: ~w~n', [R1]),
    
    % Test 2: Relevant variable used once
    writeln('Test 2: Relevant variable used once'),
    run_program([relevant, y, '=', 20, y], R2),
    format('Result: ~w~n', [R2]),
    
    writeln('').

test_expressions :-
    writeln('--- Testing Expressions ---'),
    
    % Test 3: String literals
    writeln('Test 3: String literal'),
    run_program(["hello"], R3),
    format('Result: ~w~n', [R3]),
    
    % Test 4: Boolean literals
    writeln('Test 4: Boolean literal'),
    run_program([true], R4),
    format('Result: ~w~n', [R4]),
    
    % Test 5: Addition
    writeln('Test 5: Addition'),
    run_program([5, '+', 3], R5),
    format('Result: ~w~n', [R5]),
    
    % Test 6: Negation
    writeln('Test 6: Boolean negation'),
    run_program(['!', false], R6),
    format('Result: ~w~n', [R6]),
    
    % Test 7: Conditional
    writeln('Test 7: If-then-else'),
    run_program([if, true, then, 42, else, 0], R7),
    format('Result: ~w~n', [R7]),
    
    writeln('').

test_usage_constraints :-
    writeln('--- Testing Usage Constraints ---'),
    
    % Test 8: Unused relevant variable (should fail)
    writeln('Test 8: Unused relevant variable (should fail)'),
    run_program([relevant, z, '=', 15], R8),
    format('Result: ~w~n', [R8]),
    
    % Test 9: Affine variable used twice (should fail)
    writeln('Test 9: Affine variable used twice (should fail)'),
    run_program([affine, a, '=', 5, a, '+', a], R9),
    format('Result: ~w~n', [R9]),
    
    writeln('').

% Test functions specifically
test_functions :-
    writeln('--- Testing Functions ---'),
    
    % Test 10: Simple function definition and call
    writeln('Test 10: Function definition and call'),
    FunProgram = [fun, double, '(', affine, x, ':', int, ')', '=', x, '+', x,
                  double, '(', 5, ')'],
    run_program(FunProgram, R10),
    format('Result: ~w~n', [R10]),
    
    writeln('').

% Test records specifically  
test_records :-
    writeln('--- Testing Records ---'),
    
    % Test 11: Record definition and creation
    writeln('Test 11: Record definition and creation'),
    RecordProgram = [record, 'Person', '{', name, ':', string, ',', age, ':', int, '}',
                     'Person', '{', name, '=', "John", ',', age, '=', 30, '}'],
    run_program(RecordProgram, R11),
    format('Result: ~w~n', [R11]),
    
    writeln('').

% Test lists specifically
test_lists :-
    writeln('--- Testing Lists ---'),
    
    % Test 12: List creation
    writeln('Test 12: List creation'),
    ListProgram = ['[', 1, ',', 2, ',', 3, ']'],
    run_program(ListProgram, R12),
    format('Result: ~w~n', [R12]),
    
    writeln('').

% Run specific feature tests
test_advanced_features :-
    writeln('=== TESTING ADVANCED FEATURES ==='),
    test_functions,
    test_records, 
    test_lists,
    writeln('=== ADVANCED FEATURES COMPLETED ===').
