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

-module( gen_statem_template ).
-author( "Igor Clark <igor@igorclark.net>" ).
-behaviour( gen_statem ).

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
	data, {
	}
).

%% ------------------------------------------------------------------
%% gen_statem callback exports
%% ------------------------------------------------------------------

-export( [
	start_link/0,
	init/1,
	callback_mode/0,
	terminate/3,
	code_change/4
] ).

%% ------------------------------------------------------------------
%% client API function exports
%% ------------------------------------------------------------------

-export( [
] ).

%% ------------------------------------------------------------------
%% state transition function exports
%% ------------------------------------------------------------------

-export( [
	% e.g.
	% first_state/3,
	% second_state/3,
	% third_state/3 
] ).

%% ------------------------------------------------------------------
%% gen_statem callback definitions
%% ------------------------------------------------------------------

% called by omg_control_sup:start_link/0
start_link() ->
    gen_statem:start_link( { local, ?MODULE }, ?MODULE, [], [] ).

init( [] ) ->

	error_logger:info_msg(
		"~p ~p init()",
		[ ?MODULE, self() ]
	),

	Data = #data{
	},

	FirstState = first_state, % e.g.

    { ok, FirstState, Data }.

callback_mode() ->
    state_functions.

terminate( _Reason, _StateName, _Data ) ->
    ok.

code_change( _OldVsn, StateName, Data, _Extra ) ->
    { ok, StateName, Data }.

%% ------------------------------------------------------------------
%% client API function definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% state transition function definitions
%% ------------------------------------------------------------------

%%	e.g.:
%%
%%	% handle a specific cast (async) received in first_state mode
%%	first_state( cast, Event, Data ) ->
%%		% do thing with Event
%%		{ keep_state, Data };
%%	
%%	% pass any other casts received in first_state mode to generic handle_cast below
%%	first_state( cast, Event, Data ) ->
%%		handle_cast( Event, Data );
%%	
%%	% handle a specific call (sync) received in first_state mode
%%	first_state( { call, From }, Event, Data ) ->
%%		% do thing with Event
%%		{ keep_state, Data [ { reply, From, Reply } ] }.
%%	
%%	% pass any call in second_state to generic handle_call below
%%	second_state( { call, From }, Event, Data ) ->
%%		handle_call( { call, From }, Event, Data );
%%	
%%	% pass any erlang messages in second_state to generic handle_info below
%%	second_state( info, Event, Data ) ->
%%		handle_info( Event, Data ).
%%	
%%	% handle a specific erlang message received in third_state mode & transition back
%%	third_state( info, SpecificMsg, Data ) ->
%%		io:format( "handling specific msg ~p in third_state~n", [ SpecificMsg ] ),
%%		{ next_state, first_state, Data }.
%%

%% ------------------------------------------------------------------
%% generic all-state event-handler function definitions
%% ------------------------------------------------------------------

%% handle any sync call passed in from other modes' receiver functions
handle_call( { call, From }, Event, Data ) ->
	error_logger:info_msg(
		"~p OOB handle_call(~p)",
		[ ?MODULE, Event ]
	),
	{ keep_state, Data, [ { reply, From, { invalid_state_change, Event } } ] }.

%% ------------------------------------------------------------------

%% handle any async cast passed in from other modes' receiver functions
handle_cast( Event, Data ) ->
	error_logger:info_msg(
		"~p OOB handle_cast(~p)",
		[ ?MODULE, Event ]
	),
	{ keep_state, Data }.

%% ------------------------------------------------------------------

%% handle any erlang info message received directly as OOB
handle_info( Event, Data ) ->
	error_logger:info_msg(
		"~p OOB handle_info(~p)",
		[ ?MODULE, Event ]
	),
	{ keep_state, Data }.

%% ------------------------------------------------------------------
%% internal function definitions
%% ------------------------------------------------------------------

