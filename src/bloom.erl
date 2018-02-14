-module(bloom).
-export([add/2]).
%% Native library support
-export([load/0]).
-on_load(load/0).

-include_lib("eunit/include/eunit.hrl").

add(_, _) ->
    not_loaded(?LINE).

-spec load() -> any().
load() ->
    erlang:load_nif(filename:join(priv(), "libbloom"), none).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

priv()->
  case code:priv_dir(?MODULE) of
      {error, _} ->
          EbinDir = filename:dirname(code:which(?MODULE)),
          AppPath = filename:dirname(EbinDir),
          filename:join(AppPath, "priv");
      Path ->
          Path
  end.
