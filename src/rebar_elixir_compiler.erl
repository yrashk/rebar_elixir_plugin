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
-module(rebar_elixir_compiler).

-export([compile/2,
         post_compile/2,
         clean/2,
         pre_eunit/2]).

-export([dotex_compile/2,
         dotex_compile/3]).

%% ===================================================================
%% Public API
%% ===================================================================

%% Supported configuration variables:
%%
%% * ex_first_files - First elixir files to compile
%% * elixir_opts - Erlang list of elixir compiler options
%%                 
%%                 For example, {elixir_opts, [{ignore_module_conflict, false}]}
%%

-spec compile(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
compile(Config, _AppFile) ->
    dotex_compile(Config, "ebin").

-spec post_compile(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
post_compile(_, undefined) -> ok;
post_compile(Config, AppFile) ->
    case rebar_app_utils:is_app_src(AppFile) of
        true ->
            ActualAppFile = rebar_app_utils:app_src_to_app(AppFile),
            file:delete(ActualAppFile),
            erase({app_file, ActualAppFile}),
            rebar_otp_app:compile(Config, AppFile);
        false ->
            ok
    end.


-spec clean(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
clean(_Config, _AppFile) ->
    BeamFiles = rebar_utils:find_files("ebin", "^.*\\.beam\$"),
    rebar_file_utils:delete_each(BeamFiles),
    lists:foreach(fun(Dir) -> delete_dir(Dir, dirs(Dir)) end, dirs("ebin")),
    ok.


-spec pre_eunit(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
pre_eunit(Config, _AppFIle) ->
    dotex_compile(Config, ".eunit").

%% ===================================================================
%% .ex Compilation API
%% ===================================================================

-spec dotex_compile(Config::rebar_config:config(),
                     OutDir::file:filename()) -> 'ok'.
dotex_compile(Config, OutDir) ->
    dotex_compile(Config, OutDir, []).

dotex_compile(Config, OutDir, MoreSources) ->
    App = application:load(elixir),
    Loaded = (App == ok orelse App == {error, {already_loaded, elixir}}),
    case Loaded of
        true ->
            application:start(elixir),
            FirstExs = rebar_config:get_local(Config, ex_first_files, []),
            ExOpts = ex_opts(Config),
            %% Support the src_dirs option allowing multiple directories to
            %% contain elixir source. This might be used, for example, should
            %% eunit tests be separated from the core application source.
            SrcDirs = src_dirs(proplists:append_values(src_dirs, ExOpts)),
            RestExs  = [Source || Source <- gather_src(SrcDirs, []) ++ MoreSources,
                                  not lists:member(Source, FirstExs)],
            
            %% Make sure that ebin/ exists and is on the path
            OutDirExists = filelib:is_dir(OutDir),

            CurrPath = code:get_path(),
            
            case OutDirExists of
                true -> true = code:add_path(filename:absname(OutDir));
                false -> ok
            end,

            EbinDate =
            case OutDirExists of
                true -> 
                    {ok, Files} = file:list_dir(OutDir),
                    Dates = [ filelib:last_modified(filename:join([OutDir, F])) || F <- Files, filename:extension(F) /= ".app" ],
                    case Dates of
                        [] -> 0;
                        _ ->
                            lists:max(Dates)
                    end;
                false -> 0
            end,

            compile(FirstExs, ExOpts, OutDir, EbinDate),
            compile(RestExs, ExOpts, OutDir, EbinDate),
            
            true = code:set_path(CurrPath),
            ok;
        false ->
            rebar_log:log(info, "No Elixir compiler found~n", [])
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================
compile(Exs, ExOpts, OutDir, EbinDate) ->
    case is_newer(Exs, EbinDate) of
        true ->
            '__MAIN__-Code':compiler_options(orddict:from_list(ExOpts)),
            Files = [ list_to_binary(F) || F <- Exs],
            try 
                '__MAIN__-Elixir-ParallelCompiler':
                    files_to_path(Files,
                                  list_to_binary(OutDir), 
                                  fun(F) -> 
                                          io:format("Compiled ~s~n",[F])
                                          end),
                file:change_time(OutDir, erlang:localtime()),
                ok
            catch _:{'__MAIN__-CompileError',
                     '__exception__',
                     Reason,
                     File, Line} ->
                    case EbinDate of 
                        0 -> file:change_time(OutDir, lists:min([ file:last_modified(File) || File <- Files ]));
                        _ -> file:change_time(OutDir, EbinDate)
                    end,
                    io:format("Compile error in ~s:~w~n ~ts~n~n",[File, Line, Reason]),
                    throw({error, failed})
            end;
        false -> ok
    end.

is_newer(Files, Time) ->
    lists:any(fun(FileTime) ->
                      FileTime >= Time
              end, [ filelib:last_modified(File) || File <- Files ]).

ex_opts(Config) ->
    rebar_config:get_local(Config, ex_opts, [{ignore_module_conflict, true}]).

gather_src([], Srcs) ->
    Srcs;
gather_src([Dir|Rest], Srcs) ->
    gather_src(Rest, Srcs ++ rebar_utils:find_files(Dir, ".*\\.ex\$")).

-spec src_dirs(SrcDirs::[string()]) -> [file:filename(), ...].
src_dirs([]) ->
    ["src","lib"];
src_dirs(SrcDirs) ->
    SrcDirs.

-spec dirs(Dir::file:filename()) -> [file:filename()].
dirs(Dir) ->
    [F || F <- filelib:wildcard(filename:join([Dir, "*"])), filelib:is_dir(F)].

-spec delete_dir(Dir::file:filename(),
                 Subdirs::[string()]) -> 'ok' | {'error', atom()}.
delete_dir(Dir, []) ->
    file:del_dir(Dir);
delete_dir(Dir, Subdirs) ->
    lists:foreach(fun(D) -> delete_dir(D, dirs(D)) end, Subdirs),
    file:del_dir(Dir).
