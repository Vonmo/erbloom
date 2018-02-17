-module(bloom).

%% API
-export([new/2,
         serialize/1,
         deserialize/7,
         set/2,
         check/2,
         clear/1
         ]).

%% Native library support
-export([load/0]).
-on_load(load/0).

new(_BitmapSize, _ItemsCount) ->
    not_loaded(?LINE).

serialize(_Ref) ->
    not_loaded(?LINE).

deserialize(_Bitmap, _NumBits, _NumFuns, _Sv00, _Sv01, _Sv10, _Sv11) ->
    not_loaded(?LINE).

set(_Ref, _Key) ->
    not_loaded(?LINE).

check(_Ref, _Key) ->
    not_loaded(?LINE).

clear(_Ref) ->
    not_loaded(?LINE).

load() ->
    erlang:load_nif(filename:join(priv(), "libbloom"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv()->
  case code:priv_dir(?MODULE) of
      {error, _} ->
          EbinDir = filename:dirname(code:which(?MODULE)),
          AppPath = filename:dirname(EbinDir),
          filename:join(AppPath, "priv");
      Path ->
          Path
  end.
