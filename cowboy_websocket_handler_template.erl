%%	MIT License
%%	
%%	Copyright (c) 2018 Igor Clark
%%	
%%	Permission is hereby granted, free of charge, to any person obtaining a copy
%%	of this software and associated documentation files (the "Software"), to deal
%%	in the Software without restriction, including without limitation the rights
%%	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%	copies of the Software, and to permit persons to whom the Software is
%%	furnished to do so, subject to the following conditions:
%%	
%%	The above copyright notice and this permission notice shall be included in all
%%	copies or substantial portions of the Software.
%%	
%%	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%	IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%	FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%	AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%	LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%	OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%	SOFTWARE.

-module( nest_control_ws_handler ).
-author( "Igor Clark <igor@igorclark.net>" ).

%% ------------------------------------------------------------------
%% behaviours
%% ------------------------------------------------------------------

-behaviour( cowboy_websocket ).

%% ------------------------------------------------------------------
%% application-wide macros
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% module-scope macros
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% module records
%% ------------------------------------------------------------------

-record(
	state, {
	}
).

%% ------------------------------------------------------------------
%% API function exports
%% ------------------------------------------------------------------

-export( [
] ).

%% ------------------------------------------------------------------
%% cowboy_websocket handler function/callback exports
%% ------------------------------------------------------------------

-export( [ init/2 ] ).
-export( [ websocket_init/1 ] ).
-export( [ websocket_handle/2 ] ).
-export( [ websocket_info/2 ] ).

%% ------------------------------------------------------------------
%% API function definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% cowboy_websocket handler function definitions
%% ------------------------------------------------------------------

% cowboy process init
init( Req, _Args ) ->
	State = #state{},
	{ cowboy_websocket, Req, State, #{ idle_timeout => infinity } }.

%% ------------------------------------------------------------------
%% post-HTTP-upgrade init, anything needing self() should go in here
%% ------------------------------------------------------------------

websocket_init( State ) ->
	{ ok, State }.

%% ------------------------------------------------------------------
%% receive messages from the websocket client
%% ------------------------------------------------------------------

websocket_handle( _Data, State ) ->

	case parse_ws_msg( Msg ) of

		{ _Type, _Data } = Cmd  ->
			% handle the incoming cmd
			ok;

		missing_cmd		-> ok;
		invalid_json	-> ok;
		truncated_json  -> ok;
		jsx_error		-> ok

	end,

	{ ok, State }.

%% ------------------------------------------------------------------
%% receive info messages from erlang processes
%% ------------------------------------------------------------------

websocket_info( { timeout, _Ref, Msg }, State ) ->
	{ reply, { text, Msg }, State };

websocket_info( { text, Msg }, State ) when is_binary( Msg ); is_list( Msg ) ->
	{ reply, { text, Msg }, State };

websocket_info( { text, Msg }, State ) when is_integer( Msg ) ->
	BinaryMsg = erlang:integer_to_binary( Msg ),
	{ reply, { text, BinaryMsg }, State };

websocket_info( { text, Msg }, State ) when is_float( Msg ) ->
	BinaryMsg = erlang:float_to_binary( Msg, [ { decimals, 4 } ] ),
	{ reply, { text, BinaryMsg }, State };

websocket_info( { text, Msg }, State ) when is_atom( Msg ) ->
	BinaryMsg = erlang:atom_to_binary( Msg, latin1 ),
	{ reply, { text, BinaryMsg }, State };

websocket_info( _Info, State ) ->
	lager:debug( "~p ~p got OOB info msg: ~p~n", [ ?MODULE, self(), _Info ] ),
	{ ok, State }.
 
%% ------------------------------------------------------------------
%% internal function definitions
%% ------------------------------------------------------------------

parse_ws_msg( Msg ) ->

	try jsx:decode( Msg, [ return_maps ] ) of

		#{ <<"type">> := Type, <<"data">> := Data } -> { Type, Data };

		_ -> missing_cmd

	catch

		{ error, { _, invalid_json } } ->

			lager:warning(
				"~p websocket ~p sent invalid json",
				[ ?MODULE, self() ]
			),

			invalid_json;

		{ error, { _, truncated_json } } ->

			truncated_json;

		_:Reason ->
			lager:warning(
				"~p ~p error ~p while parsing msg received from websocket: ~p~n",
				[ ?MODULE, self(), Reason, Msg ]
			),

			jsx_error

	end.
