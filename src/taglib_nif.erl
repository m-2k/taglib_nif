-module(taglib_nif).

-export([
  new/1,
  new/2,
  file_is_valid/1,
  file_save/1,
  tag_title/1,
  tag_artist/1,
  tag_album/1,
  tag_comment/1,
  tag_genre/1,
  tag_year/1,
  tag_track/1,
  tag_set_title/2,
  tag_set_artist/2,
  tag_set_album/2,
  tag_set_comment/2,
  tag_set_genre/2,
  tag_set_year/2,
  tag_set_track/2,
  audioproperties_length/1,
  audioproperties_bitrate/1,
  audioproperties_samplerate/1,
  audioproperties_channels/1
]).

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

%% File

file_is_valid(_File) ->
  ?nif_stub.

file_save(_File) ->
  ?nif_stub.

%% Tag API

tag_title(_File) ->
  ?nif_stub.

tag_artist(_File) ->
  ?nif_stub.

tag_album(_File) ->
  ?nif_stub.

tag_comment(_File) ->
  ?nif_stub.

tag_genre(_File) ->
  ?nif_stub.

tag_year(_File) ->
  ?nif_stub.

tag_track(_File) ->
  ?nif_stub.

tag_set_title(_File, _Title) ->
  ?nif_stub.

tag_set_artist(_File, _Artist) ->
  ?nif_stub.

tag_set_album(_File, _Album) ->
  ?nif_stub.

tag_set_comment(_File, _Comment) ->
  ?nif_stub.

tag_set_genre(_File, _Genre) ->
  ?nif_stub.

tag_set_year(_File, _Year) ->
  ?nif_stub.

tag_set_track(_File, _Track) ->
  ?nif_stub.

%% Audio Properties API

audioproperties_length(_File) ->
  ?nif_stub.

audioproperties_bitrate(_File) ->
  ?nif_stub.

audioproperties_samplerate(_File) ->
  ?nif_stub.

audioproperties_channels(_File) ->
  ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

new_test() ->
  {ok, File} = new(<<"../noise.mp3">>),
  ?assert(file_is_valid(File)).

new_type_test() ->
  {ok, File} = new(<<"../noise.mp3">>, 'MPEG'),
  ?assert(file_is_valid(File)).

file_does_not_exist_test() ->
  {error, Reason} = new(<<"test">>),
  ?assertEqual("File could not be opened by taglib.", Reason).

file_is_not_valid_test() ->
  {error, Reason} = new(<<"invalid">>, 'MPEG'),
  ?assertEqual("File is not valid.", Reason).

tag_title_test() ->
  {ok, File} = new(<<"../noise.mp3">>),
  Title = tag_title(File),
  ?assertEqual(<<"NoiseTrack">>, Title).

-endif.
