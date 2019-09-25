%%%-------------------------------------------------------------------
%%% @doc
%%% Repo's supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(xdb_repo_sup).

-behaviour(supervisor).

%% API
-export([
  start_link/4,
  stop/1,
  runtime_config/3
]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(Repo, OtpApp, Adapter, Opts) -> Res when
  Repo    :: xdb_repo:t(),
  OtpApp  :: atom(),
  Adapter :: xdb_adapter:t(),
  Opts    :: xdb_lib:keyword(),
  Res     :: {ok, pid()} | ignore | {error, term()}.
start_link(Repo, OtpApp, Adapter, Opts) ->
  supervisor3:start_link({local, Repo}, ?MODULE, {Repo, OtpApp, Adapter, Opts}).

-spec stop(Repo :: xdb_repo:t()) -> ok.
stop(Repo) when is_atom(Repo) ->
  case whereis(Repo) of
    undefined ->
      ok;
    Pid ->
      true = exit(Pid, normal),
      ok
  end.

-spec runtime_config(Repo, OtpApp, Opts) -> Res when
  Repo   :: xdb_repo:t(),
  OtpApp :: atom(),
  Opts   :: xdb_lib:keyword(),
  Res    :: {ok, Opts} | ignore | no_return().
runtime_config(Repo, OtpApp, Opts) ->
  case application:get_env(OtpApp, Repo) of
    {ok, Config} ->
      NewConfig = [{otp_app, OtpApp}, {repo, Repo}] ++ xdb_lib:keymerge(Config, Opts),
      repo_init(Repo, NewConfig);
    _ ->
      Text = "configuration for ~p not specified in ~p environment",
      error({badarg, xdb_lib:stringify(Text, [Repo, OtpApp])})
  end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init({Repo, OtpApp, Adapter, Opts}) ->
  case runtime_config(Repo, OtpApp, Opts) of
    {ok, Opts1} ->
      Children = [adapter_child_spec(Repo, Adapter, Opts1)],
      supervise([CS || CS <- Children, CS /= undefined], #{strategy => one_for_one});
    ignore ->
      ignore
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
repo_init(Repo, Config) ->
  case {code:ensure_loaded(Repo), erlang:function_exported(Repo, init, 1)} of
    {{module, Repo}, true} ->
      Repo:init(Config);
    _ ->
      {ok, Config}
  end.

%% @private
supervise(Children, SupFlagsMap) ->
  Strategy = maps:get(strategy, SupFlagsMap, one_for_one),
  Intensity = maps:get(intensity, SupFlagsMap, 10),
  Period = maps:get(period, SupFlagsMap, 10),
  {ok, {{Strategy, Intensity, Period}, Children}}.

%% @private
adapter_child_spec(Repo, Adapter, Opts) ->
  case {code:ensure_loaded(Adapter), erlang:function_exported(Adapter, child_spec, 2)} of
    {{module, Adapter}, true} ->
      {Timeout, Opts1} = get_restart_timeout(Opts),
      Spec = Adapter:child_spec(Repo, Opts1),
      supervisor3_child_spec(Timeout, Spec);
    _ ->
      undefined
  end.

%% @private
supervisor3_child_spec(false, Spec) -> Spec;
supervisor3_child_spec(RestartTimeout, {Id, Start, Restart, Timeout, Type, Mods}) ->
  {Id, Start, {Restart, RestartTimeout}, Timeout, Type, Mods}.

%% @private
get_restart_timeout(Opts) ->
  case lists:keytake(restart_timeout, 1, Opts) of
    {value, {restart_timeout, Value}, Opts1} -> {to_int(Value), Opts1};
    false -> {false, Opts}
  end.

%% @private
-spec to_int(binary() | list() | integer()) -> integer().
to_int(Value) when is_binary(Value) ->
  binary_to_integer(Value);
to_int(Value) when is_list(Value) ->
  list_to_integer(Value);
to_int(Value) when is_integer(Value) ->
  Value.
