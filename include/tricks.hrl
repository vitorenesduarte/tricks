-define(APP, tricks).

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

%% pod
-type tag() :: binary().
-type pod_id() :: integer().
-type pod_ip() :: list().
-type pod_data() :: {pod_id(), pod_ip()}.

%% http listener
-define(PORT, 8817).
-define(TCP_ACTIVE_OPTION, {active, once}).
-define(TCP_OPTIONS, [{packet, 4},
                      {nodelay, true},
                      {keepalive, true}]).

%% cowboy config
-define(WEB_PORT, 8080).
-define(WEB_CONFIG, [{port, ?WEB_PORT}]).
