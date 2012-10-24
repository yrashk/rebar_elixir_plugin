-module (rebar_mix_compiler).

-export([
	compile/2,
	clean/2
]).

compile(_Config, _AppFile) ->
	IsMixApp = is_mix_app(),
	compile_mix_app(IsMixApp).

clean(_Config, _AppFile) ->
	IsMixApp = is_mix_app(),
	clean_mix_app(IsMixApp).


is_mix_app() ->
	filelib:is_regular("mix.exs").

compile_mix_app(false) -> ok;
compile_mix_app(true) ->
	run_mix_command(<<"compile">>, fun compilation_error_handler/3).


clean_mix_app(false) -> ok;
clean_mix_app(true) ->
	run_mix_command(<<"clean">>).

default_error_handle_func(M, F, A) ->
	M:F(A).


compilation_error_handler(M, F, A) ->
	try
		default_error_handle_func(M, F, A)
	catch
		_:{_, '__exception__', File, Line, Reason} ->
			rebar_log:log(error, "Compile error in ~s:~w ~ts~n~n",[File, Line, Reason]),
			throw({error, failed})
	end.


run_mix_command(Command) ->
	run_mix_command(Command, fun default_error_handle_func/3).

run_mix_command(Command, ErrorHandleFunc) ->
	ok = start_mix(),
	ok = ErrorHandleFunc('Elixir-Mix-CLI', run, [Command]),
	ok = clear_mix().

start_mix() ->
	ok = loaded(mix),
	ok = 'Elixir-Mix':start().

clear_mix() ->
	'Elixir-Mix-Server':call(clear_tasks),
	ok.

loaded(mix) ->
	ensure_loaded(elixir),
	ensure_loaded(mix).

ensure_loaded(App) ->
	try
		case application:start(App) of
			ok -> ok;
			{error, {already_started, App}} -> ok
		end
	catch
		A:B ->
			rebar_log:log(error, "could not start application '~p' by reason: ~p:~p~n", [A, B]),
			false
	end.
