TAGLIB_NIF
----------

Erlang NIF bindings for the taglib library. Currently it just reads tag values.

Requirements: install libtagc0-dev

Usage
=====

```erlang
case taglib_nif:new(<<"song.mp3">>) of
  {ok, File} -> io:format("Title: ~w", [taglib_nif:tag_title(File)];
  {error, Reason} -> io:format("Error: ~w", [Reason])
end
```

licensed under LGPL, see LICENSE
