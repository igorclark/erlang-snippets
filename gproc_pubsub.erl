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
