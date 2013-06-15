%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
%% The following Global options are supported:
%% <ul>
%%   <li>suite="foo"" - runs test/foo_tests.exs</li>
%% </ul>
%% -------------------------------------------------------------------
-module(rebar_exunit).

-export([exunit/2]).

%% ===================================================================
%% Public API
%% ===================================================================

exunit(Config, _AppFile) ->
    CodePath = code:get_path(),
    true = code:add_pathz(ebin_dir()),
    App = application:load(elixir),
    Loaded = (App == ok orelse App == {error, {already_loaded, elixir}}),
    case Loaded of
        true ->
	  application:start(elixir),
          TestExs = 
          case rebar_config:get_global(suite, undefined) of
            undefined -> rebar_utils:find_files("test", ".*\\.exs\$");
            Suite -> [Suite]
          end,
          perform_exunit(Config, TestExs);
        false ->
          rebar_log:log(info, "ExUnit not found")
    end,
    true = code:set_path(CodePath),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

ebin_dir() ->
    filename:join(rebar_utils:get_cwd(), "ebin").

perform_exunit(_Config, Files) ->
    case whereis(exunit_server) of 
      undefined -> 'Elixir.ExUnit':start([]);
      _ -> ok
    end,
    [ 'Elixir.Code':require_file(list_to_binary(File)) || File <- Files ],
    'Elixir.ExUnit':run().
