
%% sym-solver structures
-record(symbolic_sum, {int, vars}).
-record(sym_var_times, {vars, times}). %% x*2 => #sym_var_times{ vars = #{ #var_ref{ name = "x"} => 1 }, times = 2 }

-record(var_ref, {name}).
-record(var_fixed, {name, int}).

-record(poly_term, {pow, factor}).