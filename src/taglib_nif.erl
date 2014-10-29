-module(taglib_nif).

-export([new/1,
         myfunction/1]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).

new(_FileName) ->
    ?nif_stub.

myfunction(_Ref) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

list_of(N, Elem) -> list_of(N, Elem, []).

list_of(0, _Elem, Acc) -> Acc;
list_of(N, Elem, Acc) when N > 0 -> list_of(N - 1, Elem, [Elem | Acc]).

%% basic_test() ->
%%     {ok, Ref} = new(<<"test">>),
%%     ?assertEqual(ok, myfunction(Ref)).

file_does_not_exist_test() ->
  {error, Reason} = new(<<"test">>),
  ?assertEqual("File could not be opened by taglib.", Reason).

filename_too_long_test() ->
  LongFileName = list_to_binary(list_of(1024, $a)),
  {error, Reason} = new(LongFileName),
  ?assertEqual("Filename is longer than 1023 bytes.", Reason).

-endif.
