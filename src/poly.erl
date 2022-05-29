-module(poly).

-export([solve_or_state/2]).

-include_lib("include/globals.hrl").

%% finds single solution to polynomial if
%% it can be expressed precicely
%% otherwise returns #poly_eq 
solve_or_state(0, _Varlist) -> 0;
solve_or_state(S, [#poly_term{ factor = F, pow = P}]) -> 
    %% We might want to either return this expression rather than a concrete value
    math:pow( S/F, (1/P) );
solve_or_state(S, Varlist) ->
    #poly_eq{ int = S, terms = Varlist }.