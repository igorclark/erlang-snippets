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

-module( gproc_pubsub ).
-author( "Igor Clark <igor@igorclark.net>" ).

-export( [
	subscribe/2,
	unsubscribe/2,
	list_subscribers/2,
	notify/3
] ).

subscribe( Channel, EventType ) ->
	gproc:ensure_reg( { p, l, { Channel, EventType } } ).

unsubscribe( Channel, EventType ) ->
	try gproc:unreg( { p, l, { Channel, EventType } } ) of
		true -> ok
	catch
		error:badarg -> not_subscribed
	end.

list_subscribers( Channel, EventType ) ->
	Pids = gproc:lookup_pids( { p, l, { Channel, EventType } } ),
	PidTuples = [ { Pid, erlang:process_info( Pid, registered_name ) } || Pid <- Pids ],
	GetPidInfo = fun
		( { Pid, { registered_name, RegisteredName } } ) when is_pid( Pid ) -> { Pid, RegisteredName };
		( { Pid, [] } ) when is_pid( Pid ) ->
			GprocInfo = gproc:info( Pid ),
			{ M, _F, _ } = proplists:get_value( current_function, GprocInfo ),
			{ Pid , M }
	end,
	[ GetPidInfo( PidTuple ) || PidTuple <- PidTuples ].


notify( Channel, EventType, Msg ) ->
	gproc:send( { p, l, { Channel, EventType } }, Msg ).
