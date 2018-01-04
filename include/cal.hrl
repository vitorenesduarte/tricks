-define(APP, cal).

%% error
-type error() :: {error, term()}.

%% event
-type event() :: {binary(), integer()}.
