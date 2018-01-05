-define(APP, cal).

%% kuberl
-define(CTX, ctx:background()).
-define(NAMESPACE, <<"default">>).

%% error
-type error() :: {error, term()}.

%% experiment
-type exp_id() :: integer().

%% event
-type event_name() :: binary().
-type event() :: {event_name(), integer()}.

%% client listener
-define(PORT, 8817).
-define(TCP_ACTIVE_OPTION, {active, once}).
-define(TCP_OPTIONS,
        [{packet, 4},
         {nodelay, true},
         {keepalive, true}]).
