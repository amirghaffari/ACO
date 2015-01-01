-module(aco).
-export([run/0, run/1]).

-include ("types.hrl").

%% --------------------------- Read problem specification from file ------------------------------ %%

-spec get_int (string()) -> integer().

get_int(S) ->
    {N, _} = string:to_integer(S),
    N.
-spec read_input(string()) -> {integer(), inputs()}.

read_input(Fname) ->
    util:check_file(Fname),
    case file:read_file(Fname) of
        {ok,B} ->
	    [N|Rest] = string:tokens(binary:bin_to_list(B), " \n"),

	    % Split at space & newline. 

	    % Input format: 3N+1 integers.  N is the first item in the file, 
	    % then three sequences of N integers, containing durations, 
	    % weights and due dates respectively.

	    Num_Jobs = get_int(N),  
            Rest1 = lists:map(fun get_int/1, Rest),

            {Durations, Rest2} = lists:split(Num_Jobs, Rest1),
            {Weights, Rest3} = lists:split(Num_Jobs, Rest2),
            {Deadlines, _} = lists:split(Num_Jobs, Rest3),
            {Num_Jobs, {list_to_tuple(Durations), list_to_tuple(Weights), list_to_tuple(Deadlines)}};
        {error, Report} ->
            error (Report)
    end.  


%% ---------------- Start the master ---------------- %%

start_aco (Fname, Vertex_degree, Duplicating, Num_Ants, Iter_Global, Iter_Local, NodeFile, Params0) ->
    {Num_Jobs, Inputs} = read_input(Fname),
    #params{tau0=T0} = Params0,
    Tau0 = case T0 of
	       undefined -> fitness:tau0 (Num_Jobs, Inputs, Num_Ants);
	       _ -> util:check_positive({tau0, T0}), T0
	   end,
    Params = Params0#params{tau0=Tau0},   
    % FIX: with the EDD value for tau0, it seems that the pheromones 
    % evaporate completely and then we have problems in ant.erl
    % because many of the tau entries are 0.

    util:check_file(NodeFile),
    {ok, [Nodes]} = file:consult(NodeFile),
    lists:foreach (fun util:check_node/1, Nodes),
    case util:dups(Nodes) of
		[] -> io:format ("No duplication~n"), ok;
		D -> util:quit ("Error. Duplicate nodes in ~p: ~p.~n", [NodeFile,D])
    end,
    % This will print a message but not actually terminate the VM.

    {Best_Cost, Best_Schedule} = 
	ant_master:run(Num_Jobs, Vertex_degree, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes),
    L1 = lists:sort(Best_Schedule),
    L2 = lists:seq(1,Num_Jobs),
    if L1 =:= L2-> ok;
       true -> io:format ("WARNING: bad solution~n")
    end,
    {Num_Jobs, Best_Cost, Best_Schedule}.



% Shut down all the VMs specified in NodeFile
stop_vms (NodeFile) ->
    util:check_file(NodeFile),
    {ok, [Nodes]} = file:consult(NodeFile),
    lists:foreach (fun(Node) -> rpc:call(Node, init, stop, []) end, Nodes).

% At the moment, the colonies shut down at the end of an ACO run,
% but the VMs stay alive (this makes launching multiple runs easier).
% The stop_vms function shuts the VMs down.  We could also do this by
% leaving the main loop of the colonies running and having a message to
% tell them to shut down the VM, but this might need a bit more tidying up 
% at the end of individual runs.



%% -------------------- Argument parsing -------------------- %%

-spec usage() -> ok.
usage() -> 
    io:format ("Usage: aco [options] <input file> <vertex_degree> <num_ants> <global_iter> <local_iter> <node_file>~n"),
    io:format (" Input file should contain the number of jobs on the first line,~n"),
    io:format (" then three lines containing durations, weights, and deadlines.~n"),
    io:format (" Node file contains an Erlang list giving nodes to run colonies on.~n"),
    io:format ("~n"),
    io:format ("Options:~n"),
    io:format (" v: verbose~n"),
    io:format (" s: print schedule at end~n"),
    io:format (" R: produce output for R~n"),
    io:format (" N: set random seeds using now(). Use this if you get a crypto link error.~n"),
    io:format (" ne: don't evaporate pheromone during update.~n"),
    io:format (" a=<float>: set value of pheromone influence factor alpha (default = 1.0)~n"),
    io:format (" b=<float>: set value of heuristic influence factor beta  (default = 2.0)~n"),
    io:format (" rho=<float>: set value of pheromone evaporation rate rho (default = 0.1)~n"),
    io:format (" q0=<float>: set value of random exploration factor q0    (default = 0.9)~n"),
    io:format (" tau0=<float>: set initial pheromone strength (default calculated from mdd)~n"),
    io:format (" mdd: use MDD heuristic (this is the default)~n"),
    io:format (" edd: use EDD heuristic instead of MDD~n"),
    io:format (" au:  use AU heuristic instead of MDD~n"),

    io:format (" o1, o2, o12, o21: local search options (experimental)\n"),
    io:format ("   o1:  interchange search~n"),
    io:format ("   o2:  insertion search~n"),
    io:format ("   o12: interchange + insertion~n"),
    io:format ("   o21: insertion + interchange~n"),
    io:format ("~n"),
    io:format ("Also, aco stop <node_file> shuts down all the specified nodes.~n").
    
%-spec run2([string()]) -> ok.
run2(Args,Params) ->
    case Args of
	["v"|Rest] ->
	    run2(Rest, Params#params{verbose=true});
	["vv"|Rest] ->
	    run2(Rest, Params#params{verbose=true, vverbose=true});
	["R"|Rest] ->
	    put(r_output, true),    
	    run2(Rest, Params);
	["s"|Rest] ->
	    run2(Rest, Params#params{schedule=true});
	["ne"|Rest] ->
	    run2(Rest, Params#params{evaporate=false});
	["N"|Rest] ->
	    run2(Rest, Params#params{seed_now=true});
	["o1"|Rest] ->
	    run2(Rest, Params#params{search=o1});
	["o2"|Rest] ->
	    run2(Rest, Params#params{search=o2});
	["o12"|Rest] ->
	    run2(Rest, Params#params{search=o12});
	["o21"|Rest] ->
	    run2(Rest, Params#params{search=o21});
	[ [$a, $=   |V] |Rest] -> 
	    Alpha = util:float_of_string(V), 
	    run2(Rest, Params#params{alpha=Alpha});
	[ [$b, $=   |V] |Rest] ->
	    Beta = util:float_of_string(V), 
	    run2(Rest, Params#params{beta=Beta});
	[ [$q,$0,$= |V] |Rest] ->
	    Q0 = util:float_of_string(V), 
	    run2(Rest, Params#params{q0=Q0});
	[ [$r, $h, $o, $=   |V] |Rest] ->
	    Rho = util:float_of_string(V), 
	    run2(Rest, Params#params{rho=Rho});
	[ [$t, $a, $u, $0, $=   |V] |Rest] ->
	    Tau0 = util:float_of_string(V), 
	    run2(Rest, Params#params{tau0=Tau0});
	["mdd"|Rest] ->
	    run2(Rest, Params#params{heuristic=mdd});
	["edd"|Rest] ->
	    run2(Rest, Params#params{heuristic=edd});
	["au"|Rest] ->
	    run2(Rest, Params#params{heuristic=au});
	["mixed"|Rest] ->
	    run2(Rest, Params#params{heuristic=mixed});
	[Fname,Vertex_degree0, Duplicating0, Num_Ants0, Iter_Global0, Iter_Local0, NodeFile] ->
		Vertex_degree = util:int_of_string(Vertex_degree0),
		Duplicating = util:int_of_string(Duplicating0),
	    Num_Ants = util:int_of_string(Num_Ants0),
	    Iter_Global = util:int_of_string(Iter_Global0),
	    Iter_Local  = util:int_of_string(Iter_Local0),
	    util:check_positive ({'Num_Ants', Num_Ants}),
	    util:check_positive ({'Iter_Global', Iter_Global}),
	    util:check_positive ({'Iter_Local', Iter_Local}),
	    #params{schedule=Print_schedule} = Params,
	    {Time, {Num_Jobs, Best_Cost, Best_Schedule}} =
		timer:tc (fun() -> start_aco (Fname, Vertex_degree, Duplicating, Num_Ants, Iter_Global, Iter_Local, NodeFile, Params) end),
	    case get(r_output) of 
		true -> 
		    io:format ("~10w ~10w ~10w ~10w ~10w ~10w ~10w~n", 
			       [Num_Jobs, Vertex_degree, Num_Ants, Iter_Global, Iter_Local, Best_Cost, Time]);
		_ -> 
		    io:format("Best cost = ~w~n", [Best_Cost]),
		    case Print_schedule of
			true -> io:format("Best schedule = ~w~n", [Best_Schedule]);
			_ -> ok
		    end,
		    io:format("Time = ~w~n", [Time])
	    end;
	["stop", NodeFile] -> stop_vms (NodeFile);
	["h"] -> usage();
	[F|_] -> io:format ("Unknown option ~p~n", [F]), usage();
	[] -> usage()
		     
    end.


-spec run([atom()]) -> ok.
run(Args) ->
    Params=#params{},  % program parameters - see types.hrl
    run2 (lists:map (fun atom_to_list/1, Args), Params).
    
-spec run() -> ok.
run () -> usage().



