-module(langcode).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% basic language constructs
-record(var, {name, type, src_line, const}).
-record(condition, {src_line, op}).

%% parsing constructs
-record(infix_op, {left, op, right}).

-record(cons_fixed, {int, src_line}).
-record(cons_relation, {sum, src_line}).

-include_lib("include/globals.hrl").

%% escript Entry point
main([File]) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    Tokens = clean_and_tokenize(Str),
    LogicalLines = to_logical_lines(Tokens),
    Vars = make_variables(LogicalLines),
    Conds0 = make_conditions(LogicalLines),
    Conds = resolve_bindings(Conds0, Vars),
    Vars1 = constrain_vars(Conds, Vars),
    Res = apply_fixed([], Vars1, []),
    %io:format("~p", [io_lib_pretty:print("Args: ~p~n", [{File, Bin, Tokens, VarBindings}], fun records/1 )]),
    {Tokens, Vars, Conds, Vars1, Res}.

%%====================================================================
%% Internal functions
%%====================================================================

apply_fixed(Fixed, Vars0, Done) ->
   Vars1 = [update_constraints(V, Fixed) || V <- Vars0],
   Vars = [apply_constraints(V) || V <- Vars1],
   {NewFixed, Rest} = lists:partition(fun(V) -> is_record(V, var_fixed) end, Vars),
   io:format("~p~n", [{?LINE, fixed_values_for, NewFixed}]),
   case {NewFixed, Rest} of
        {_, []} -> NewFixed ++ Done;
        {[], _} -> Rest ++ Done;
        _ -> apply_fixed(NewFixed, Rest, NewFixed ++ Done)
   end.

update_constraints(Var = #var{ name = N, const = Const }, Fixed) ->
    Var#var{const = lists:flatten([apply_fixed_vars(N, C, Fixed) || C <- Const]) }.

apply_fixed_vars(_Name, C = #cons_fixed{}, _Vars) ->
    C;
apply_fixed_vars(Name, #cons_relation{ sum = S0, src_line = Line }, Vars) ->
    S = sym:substitue(S0, Vars),
    symbolic_sum_to_constraints(Name, S, Line).

apply_constraints(V = #var{ name = N, const = Const }) ->
    FixedValues = [X || #cons_fixed{ int = X } <- Const],
    case FixedValues of
        [] -> V;
        [X] -> #var_fixed{ name = N, int = X }
    end.

constrain_vars(Conds, Vars0) ->
    [constrain_var(Conds, V) || {_, V = #var{}} <- maps:to_list(Vars0)].

constrain_var(Conds, V = #var{}) ->
    V#var{ const =  lists:flatten([condition_to_constraint(V, Cond) || Cond <- Conds])}.

condition_to_constraint(#var{ name = Name }, #condition{ op = #infix_op{ op = '=', left = L0, right = R0}, src_line = Line }) ->
    %% parse 1 + y = x + z 
    %% into 
    %% 1 + y + -x + -z = 0
    %% and see if we can derive something useful out of it
    L = symbolic_sum(L0),
    R = symbolic_sum(R0),
    %io:format("~p~n", [{?LINE, building_symsum, L, R}]),
    NL = sym:negate(L),
    S = sym:add(NL, R),
    symbolic_sum_to_constraints(Name, S, Line).

symbolic_sum_to_constraints(Name, SymSum, Line) ->
    Map = sym:variables(SymSum),
    case {Map, maps:size(Map)} of 
        {#{ Name := _ }, 1} -> 
            Fixed = sym:single_var_sum_to_int(SymSum),
            [#cons_fixed{ int = Fixed, src_line = Line }];
        {#{ Name := _ }, _} -> 
            [#cons_relation{ sum = SymSum, src_line = Line}];
        _ -> []
    end.

%% parsing constructs -> #symbolic_sum{}
%% 1 + x + y2 -> #symbolic_sum{ int = 1, vars = [x1, y2]}
symbolic_sum(I) when is_integer(I) -> 
    #symbolic_sum{ int = I, vars = [] };
symbolic_sum(#var_ref{} = V) -> 
    #symbolic_sum{ int = 0, vars = [#sym_var_times{ vars = #{ V => 1 }, times = 1}] };
symbolic_sum(#infix_op{ op = '*', left = L0, right = R0 }) ->
    L = symbolic_sum(L0), 
    R = symbolic_sum(R0),
    Res = sym:mul(L, R),
    io:format("~p~n", [{?LINE, building_symsum_mul, L, R, Res}]),
    Res;
symbolic_sum(#infix_op{ op = '+', left = L0, right = R0 }) ->
    L = symbolic_sum(L0), 
    R = symbolic_sum(R0),
    sym:add(L, R).

resolve_bindings(Conds, Vars) ->
    [resolve_binding(C, Vars) || C <- Conds].

resolve_binding(C = #condition{ op = Op0 }, Vars) ->
    Op = resolve_binding(Op0, Vars),
    C#condition{ op = Op };
resolve_binding(C = #infix_op{ left = L0, right = R0}, Vars) ->
    L = resolve_binding(L0, Vars),
    R = resolve_binding(R0, Vars),
    C#infix_op{left = L, right = R};
resolve_binding({A}, Vars) ->
    case maps:get(A, Vars, undefined) of
        undefined -> resolve_literal(A);
        #var{ name = N } -> #var_ref{ name = N }
    end.

resolve_literal(A) ->
    list_to_integer(A).

make_conditions(Lines) ->
    [#condition{ src_line = I, op = V } || {I, #infix_op{} = V} <- maps:to_list(Lines)].
make_variables(Lines) ->
    maps:from_list([{N, V#var{ src_line = I}} || {I, #var{ name = N} = V} <- maps:to_list(Lines)]).

clean_and_tokenize(Str) ->
    S2 = [I || I <- Str, I =/= $\n, I =/= $\t, I =/= $ ],
    string:tokens(S2, ";").

to_logical_lines(Tokens) ->
    {_, Lines} = 
      lists:foldl(
        fun(Token, {N, Acc}) ->
            Var = classify(Token),
            {N+1, Acc#{ N => Var }}
        end, 
        {1, #{}}, 
        Tokens),
    Lines.

classify("int:" ++ Name) ->
    #var{ type = int, name = Name};
classify(Arg1) ->
    L = split_on_operator(Arg1),
    to_operator_tree(L).

to_operator_tree(List) ->
    {Before, After} = group_by(List, '='),
    A = arith_pass(Before),
    B = arith_pass(After),
    #infix_op{ left = A, op = '=', right = B}.

group_by(A, B) -> group_by(A, B, []).

group_by([], _, _) -> false;
group_by([A|_], A, []) -> false;
group_by([A|T], A, Acc) -> {Acc, T};
group_by([H|T], A, Acc) -> group_by(T, A, Acc ++ [H]).

arith_pass([I]) -> I;
arith_pass(List) -> 
    Operators = [I || I <- List, is_atom(I)],
    case lists:usort(Operators) of
        ['+'] -> all_com_pass('+', List);
        ['*'] -> all_com_pass('*', List)
    end.

%% case when a arith expression only contains
%% communicative operators, i.e operators where ordering does not matter
all_com_pass(_, [I]) -> I;
all_com_pass(Op, List) ->
    {A, B} = group_by(List, Op),
    #infix_op{ left = all_com_pass(Op, A), op = Op, right = all_com_pass(Op, B)}.

split_on_operator([H|T] = Line) ->
    case has_operator_prefix(Line) of
        {Prefix, Rest} -> 
            [list_to_atom(Prefix) | split_on_operator(Rest)];
        false -> split_on_operator(T, [H])
    end.

split_on_operator([], Acc) ->
    [{Acc}];
split_on_operator([H|T] = Line, Acc) ->
    case has_operator_prefix(Line) of
        {Prefix, Rest} -> 
            [{Acc}, list_to_atom(Prefix) | split_on_operator(Rest)];
        false -> 
            split_on_operator(T, Acc ++ [H])
    end.

has_operator_prefix([H|T]) when 
    H == $= orelse 
    H == $+ orelse 
    H == $- orelse 
    H == $/ orelse 
    H == $* orelse
    H == $( orelse 
    H == $) -> 
    {[H], T};

has_operator_prefix(_) -> false.