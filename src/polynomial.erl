-module(polynomial).

-export([solve/2]).

-include_lib("include/globals.hrl").

%% solves polynomial Int = Varlist
%% does NOT try to find all solutions
%% Varlist -> kx + kx^2 + ... 
solve(0, _Varlist) -> 0;
solve(S, [#poly_term{ factor = F, pow = P}]) -> 
    math:pow( S/F, (1/P) );
solve(S, Varlist0) ->
    case lists:all(fun(#poly_term{ pow = P}) -> P >= 1 end, Varlist0) of
        true -> 
            %% This is wrong somehow
            Varlist = [I#poly_term{ pow = P - 1} || I = #poly_term{ pow = P} <- Varlist0, P =/= 1 ],
            IntegerComponent =  sum([F || #poly_term{ pow = 1, factor = F } <- Varlist0]),
            solve(S - IntegerComponent, Varlist);
        false -> 
            {cannot_solve_for, S, Varlist0}
    end.

sum(List) ->
    lists:foldl(fun(A, B) -> A + B end, 0, List).