-define(APP, cal).

%% error
-type error() :: {error, term()}.

%% experiment
-type experiment() :: maps:map().
