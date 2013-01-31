-module(bars_wm_ping).
-export([init/1, to_html/2]).
-ignore_xref([init/1, to_html/2, ping/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, nostate}.

to_html(ReqData, Context) ->
    Result = io_lib:format("Result: ~p", [bars:ping()]),
    {"<html><head><title>bars</title></head><body>" ++ Result ++ "</body></html>", ReqData, Context}.
