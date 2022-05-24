-module(sym).

-export([add/2, substitue/2, mul/2, negate/1, single_var_sum_to_int/1]).

-include_lib("include/globals.hrl").

%% converts single var expressions
%% into the corresponding integer value
%% provided they where = 0 in a equation
%% x + 5 -> 5 
%% (x^2) - 4 -> 2
%% x + y -> false
%% etc
single_var_sum_to_int(#symbolic_sum{}) ->
    false. 

%% list of vars in sym sum
%% x + y + z -> [x,y,z]
variables(#symbolic_sum{}) ->
    ok.

add(#symbolic_sum{ int = I1, vars = V1 }, #symbolic_sum{ int = I2, vars = V2 }) ->
    V = unify_vars(V1 ++ V2),
    #symbolic_sum{ int = I1 + I2, vars = V }.

unify_vars(V) -> unify_vars(V, []).

unify_vars([], Acc) -> Acc;
unify_vars([H = #sym_var_times{ vars = V, times = X1 } |T], Acc0) ->
    %% TODO xy = yx and the system will not get that
    case lists:keytake(V, #sym_var_times.vars, Acc0) of
        false -> unify_vars(T, [H|Acc0]);
        {value, #sym_var_times{ times = X2 }, Acc} -> 
            unify_vars(T, [H#sym_var_times{ times = X1 + X2 } | Acc])
    end.

substitue(S, []) -> S;
substitue(S0 = #symbolic_sum{ vars = V1, int = Int }, [H|T]) ->
    {NewInt, NewVars} = apply_sym_sub(V1, H),
    S = S0#symbolic_sum{ int = Int + NewInt, vars = NewVars},
    substitue(S, T).

apply_sym_sub(A, B) -> apply_sym_sub(A, B, {0, []}).

apply_sym_sub([], _, Acc) -> Acc;
%% TODO handle xy5 : y = 3 -> x15 
apply_sym_sub([#sym_var_times{ vars = [#var_ref{ name = N }], times = Times} | T], 
              V = #var_fixed{ name = N, int = Int }, {IntAcc, VarAcc}) -> 
    apply_sym_sub(T, V, {IntAcc + (Int * Times), VarAcc});
apply_sym_sub([Var = #sym_var_times{ vars = [_]} | T], V, {IntAcc, VarAcc}) -> 
    apply_sym_sub(T, V, {IntAcc, [Var|VarAcc]}).

negate(S = #symbolic_sum{ int = I0, vars = V0 }) -> 
    S#symbolic_sum{ int = I0 * -1, vars = [I#sym_var_times{ times = T * -1 } || I = #sym_var_times{ times = T } <- V0 ]}.

mul(T = #symbolic_sum{ int = 0, vars = [] }, _) ->
    T;
mul(_, T = #symbolic_sum{ int = 0, vars = [] }) ->
    T;
%% TODO proper sym mul
mul(#symbolic_sum{ int = T1, vars = V1 }, #symbolic_sum{ int = T2, vars = V2 }) ->
    V = [ sym_var_times_mul(I1, I2) || I1 <- V1, I2 <- V2 ],
    #symbolic_sum{ int = T1 * T2, 
                   vars = V }.

sym_var_times_mul(#sym_var_times{}, #sym_var_times{}) ->
    ok.
