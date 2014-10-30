-module(taglib_nif).

-export([new/1,
         new/2,
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

new_type(_FileName, _Type) ->
  ?nif_stub.

new(Filename, 'MPEG') -> new_type(Filename, 0);
new(Filename, 'OggVorbis') -> new_type(Filename, 1);
new(Filename, 'FLAC') -> new_type(Filename, 2);
new(Filename, 'MPC') -> new_type(Filename, 3);
new(Filename, 'OggFlac') -> new_type(Filename, 4);
new(Filename, 'WavPack') -> new_type(Filename, 5);
new(Filename, 'Speex') -> new_type(Filename, 6);
new(Filename, 'TrueAudio') -> new_type(Filename, 7);
new(Filename, 'MP4') -> new_type(Filename, 8);
new(Filename, 'ASF') -> new_type(Filename, 9).

myfunction(_Ref) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

list_of(N, Elem) -> list_of(N, Elem, []).

list_of(0, _Elem, Acc) -> Acc;
list_of(N, Elem, Acc) when N > 0 -> list_of(N - 1, Elem, [Elem | Acc]).

new_test() ->
  {ok, _Ref} = new(<<"noise.mp3">>).

new_type_test() ->
  {ok, _Ref} = new(<<"noise.mp3">>, 'MPEG').

file_does_not_exist_test() ->
  {error, Reason} = new(<<"test">>),
  ?assertEqual("File could not be opened by taglib.", Reason).

filename_too_long_test() ->
  LongFileName = list_to_binary(list_of(1024, $a)),
  {error, Reason} = new(LongFileName),
  ?assertEqual("Filename is longer than 1023 bytes.", Reason).

-endif.
