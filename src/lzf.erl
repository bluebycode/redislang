-module(lzf).
-export([decompress/2,decode/1]).
-on_load(init/0).

-define(SHAREDLIB, "lzf_nif").
-define(nif_stub, nif_stub_error(?LINE)).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
  PrivDir = case code:priv_dir(?MODULE) of
      {error, bad_name} ->
          EbinDir = filename:dirname(code:which(?MODULE)),
          AppPath = filename:dirname(EbinDir),
          filename:join(AppPath, "priv");
      Path ->
          Path
  end,
  ok = erlang:load_nif(filename:join(PrivDir, ?SHAREDLIB), 0).

lzfpartition(B) -> lzfpartition(B, {0,0,<<>>}).

lzfpartition(<<>>, P) -> 
    P;
lzfpartition(<<_:1/binary>>, P) ->
    P;

lzfpartition(<<_:1/binary,H2:1/binary>>, P) -> 
    lzfpartition(<<H2/binary>>, P);

lzfpartition(<<$Z,$V,0,L:16/unsigned-integer,T/binary>>, _) -> 
    lzfpartition(<<>>, {L, byte_size(T), T});

lzfpartition(<<$Z,$V,1,_:16/unsigned-integer,C:16/unsigned-integer,T/binary>>, _) -> 
    lzfpartition(<<>>, {C, byte_size(T), T});

lzfpartition(<<_:1/binary,H2:1/binary,T/binary>>,P) -> 
    lzfpartition(<<H2:1/binary,T/binary>>,P).

decode(Bytes) ->
  case {UncompSize, CompSize, Compressed} = lzfpartition(Bytes) of 
    {_,_,<<Compressed:CompSize/binary>>} -> 
      case (catch decompress(UncompSize, Compressed)) of
        {'EXIT', _Err} -> <<>>;
        Response -> Response
      end;
    _ -> <<>>
  end.

decompress(_X,_Y) ->
    exit(nif_library_not_loaded).
