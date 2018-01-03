-define(APP, cal).

%% error
-type error() :: {error, term()}.

%% exp
-type exp_spec() :: maps:map().
-type exp_id() :: integer().
%% exp entry
-type entry_spec() :: maps:map().
%% pod
-type pod_id() :: integer().
%% kuberl
-type kuberl_body() :: maps:map().
