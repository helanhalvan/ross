-module(sym).

-export([add/2, substitue/2, mul/2, negate/1, single_var_sum_to_int/1, variables/1]).

-include_lib("include/globals.hrl").

%% converts single var expressions
%% into the corresponding integer value
%% provided they where = 0 in a equation
%% x + 5 -> 5 
%% (x^2) - 4 -> 2
%% x + y -> false
%% etc
single_var_sum_to_int(Sum = #symbolic_sum{ int = I, vars = FullVars }) ->
    Varlist = variables_list(Sum),
    Vars = lists:usort([V || {V, _} <- Varlist]),
    io:format("~p~n", [{?LINE, ?MODULE, Sum, Vars, Varlist}]),
    case length(Vars) of
        1 -> 
            %% This is only safe because we know there is only a single variable in this symbolic_sum
            PolyExpr = 
              [#poly_term{ pow = Pow, factor = Times} || #sym_var_times{ vars = ExprVars, times = Times } <- FullVars, 
                {_, Pow} <- maps:to_list(ExprVars) ],
            poly:solve_or_state(I*-1, PolyExpr);
        _ -> false 
    end;
single_var_sum_to_int(_) -> false.

%% list of vars in sym sum
%% x + y + z -> #{x => 1,y => 1,z => 1}
variables_list(#symbolic_sum{ vars = Vars }) ->
    [{N, Pow} || #sym_var_times{ vars = ExprVars } <- Vars, 
                                {#var_ref{ name = N }, Pow} <- maps:to_list(ExprVars) ].

variables(Sym = #symbolic_sum{}) ->
    maps:from_list(variables_list(Sym)).

add(#symbolic_sum{ int = I1, vars = V1 }, #symbolic_sum{ int = I2, vars = V2 }) ->
    V = add_unify_vars(V1 ++ V2),
    #symbolic_sum{ int = I1 + I2, vars = V }.

add_unify_vars(V) -> add_unify_vars(V, []).

%% 3x + 2x = 5x
add_unify_vars([], Acc) -> Acc;
add_unify_vars([H = #sym_var_times{ vars = V, times = X1 } |T], Acc0) ->
    %% TODO xy = yx and the system will not get that
    case lists:keytake(V, #sym_var_times.vars, Acc0) of
        false -> add_unify_vars(T, [H|Acc0]);
        {value, #sym_var_times{ times = X2 }, Acc} -> 
            add_unify_vars(T, [H#sym_var_times{ times = X1 + X2 } | Acc])
    end.

substitue(S, []) -> S;
substitue(S0 = #symbolic_sum{ vars = V1, int = Int }, [H|T]) ->
    {NewInt, NewVars} = apply_sym_sub(V1, H),
    S = S0#symbolic_sum{ int = Int + NewInt, vars = NewVars},
    substitue(S, T).

apply_sym_sub(A, B) -> apply_sym_sub(A, B, {0, []}).

apply_sym_sub([], _, Acc) -> Acc;
apply_sym_sub([ H0 = #sym_var_times{ vars = ExprVars, times = Times} | T], 
              V = #var_fixed{ name = N, int = Int }, {IntAcc, VarAcc}) -> 
    case maps:take(#var_ref{ name = N}, ExprVars) of
        error -> 
            apply_sym_sub(T, V, {IntAcc, [H0|VarAcc]});
        {Pow, NewExprVars} when map_size(NewExprVars) == 0 -> 
            apply_sym_sub(T, V, {IntAcc + (math:pow(Int, Pow)*Times), VarAcc});
        {Pow, NewExprVars} ->
            H = H0#sym_var_times{ vars = NewExprVars, times = Times * math:pow(Int, Pow)},
            apply_sym_sub(T, V, {IntAcc + (math:pow(Int, Pow)*Times), [H|VarAcc]})
    end.

negate(S = #symbolic_sum{ int = I0, vars = V0 }) -> 
    S#symbolic_sum{ int = I0 * -1, vars = [I#sym_var_times{ times = T * -1 } || I = #sym_var_times{ times = T } <- V0 ]}.

%% (1 + x) * (2 + y)
mul(T = #symbolic_sum{ int = 0, vars = [] }, _) ->
    T;
mul(_, T = #symbolic_sum{ int = 0, vars = [] }) ->
    T;
mul(#symbolic_sum{ int = T1, vars = V1 }, #symbolic_sum{ int = T2, vars = V2 }) ->
    %io:format("~p~n", [{?LINE, ?MODULE, V1, V2}]),
    V = [ sym_var_times_mul(I1, I2) || I1 <- V1, I2 <- V2 ] ++ 
        [ I2#sym_var_times{ times = Times * T1 } || I2 = #sym_var_times{ times = Times } <- V2, T1 =/= 0 ] ++
        [ I1#sym_var_times{ times = Times * T2 } || I1 = #sym_var_times{ times = Times } <- V1, T2 =/= 0 ],
    #symbolic_sum{ int = T1 * T2, 
                   vars = V }.

%% (3*xy (z^2) ) * 2xw 
%% 6*(x^2)y(z^2)w
sym_var_times_mul(#sym_var_times{ vars = L, times = Times1 },
                  #sym_var_times{ vars = R, times = Times2 }) ->
    ExprVars = var_mul(L, R),
    #sym_var_times{ times = Times1 * Times2, vars = ExprVars }.

%% xy * yz -> x * y * y * z -> x(y^2)z
var_mul(VL0, VR0) -> 
    VL = maps:to_list(VL0),
    VR = maps:to_list(VR0),
    Res = var_mul_unify(VL ++ VR, #{}),
    %%io:format("~p~n", [{?LINE, ?MODULE, VL, VR, Res}]),
    Res.

var_mul_unify([], Acc) -> Acc;
var_mul_unify([{Key, Pow}|T], Acc0) -> 
    case maps:take(Key, Acc0) of
        error -> 
            var_mul_unify(T, Acc0#{ Key => Pow });
        {AccPow, Acc1} -> 
            var_mul_unify(T, Acc1#{ Key => AccPow + Pow }) 
    end.