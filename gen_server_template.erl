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

-module( gen_server_module ).
-author( "Igor Clark <igor@igorclark.net>" ).
-behaviour( gen_server ).

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
%% gen_server function/callback exports
%% ------------------------------------------------------------------

-export( [
	start_link/0,
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
] ).

%% ------------------------------------------------------------------
%% API function definitions
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% gen_server function definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link( { local, ?MODULE }, ?MODULE, [], [] ).

init( [] ) ->
    { ok, #state{} }.

%% ------------------------------------------------------------------

handle_call( _Request, _From, State ) ->
    { reply, ignored, State }.

%% ------------------------------------------------------------------

handle_cast( _Msg, State ) ->
    { noreply, State }.

%% ------------------------------------------------------------------

handle_info( _Info, State ) ->
    { noreply, State }.

%% ------------------------------------------------------------------

terminate( _Reason, _State ) ->
    ok.

code_change( _OldVsn, State, _Extra ) ->
    { ok, State }.

%% ------------------------------------------------------------------
%% internal function definitions
%% ------------------------------------------------------------------

