-define(APP, cal).

%% error
-type error() :: {error, term()}.

%% experiment
-type exp_id() :: integer().

%% event
-type event_name() :: binary().
-type event() :: {event_name(), integer()}.
