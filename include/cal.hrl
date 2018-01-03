-define(APP, cal).
-define(EXP, cal_exp).

%% error
-type error() :: {error, term()}.

%% exp
-type exp_id() :: integer().
-type exp_spec() :: maps:map().
%% exp entry
-type entry_spec() :: maps:map().
%% pod
-type pod_id() :: integer().
%% kuberl
-type kuberl_body() :: maps:map().
