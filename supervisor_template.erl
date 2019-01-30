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

-module( supervisor_module ).
-author( "Igor Clark <igor@igorclark.net>" ).
-behaviour( supervisor ).

%% ------------------------------------------------------------------
%% application-wide macros
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% module-scope macros
%% ------------------------------------------------------------------

-define( SUPERVISOR_MODULE, ?MODULE ).
-define( WORKER_MODULE, worker_module ).

%% ------------------------------------------------------------------
%% API function exports
%% ------------------------------------------------------------------

-export( [
] ).

%% ------------------------------------------------------------------
%% supervisor function/callback exports
%% ------------------------------------------------------------------

-export( [
	start_link/0,
	init/1
] ).

%% ------------------------------------------------------------------
%% API function definitions
%% ------------------------------------------------------------------

start_link() ->

	Name = { local, ?MODULE },
	{ ok, _Pid } = Link = supervisor:start_link( Name, ?MODULE, [] ),
	Link.

%% ------------------------------------------------------------------
%% supervisor function definitions
%% ------------------------------------------------------------------

init( [] ) ->

	ChildSpec = {
		?WORKER_MODULE,						% Id to register
		{ ?WORKER_MODULE, start_link, [] },	% M/F/A to spawn
		permanent,							% child restart type
		5000,								% child shutdown type
		worker,								% child type
		[ ?WORKER_MODULE ]					% list of involved modules, for hot code swaps
	},

	Children = [
		ChildSpec
	],

	RestartStrategy = {
		one_for_one,	% spawn new processes with this child spec on request
		5,				% no more than this many restarts in ...
		5				% ... this many seconds
	},

	{ ok, { RestartStrategy, Children } }.
