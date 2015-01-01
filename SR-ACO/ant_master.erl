-module(ant_master).
-compile(export_all).
-include("types.hrl").

%-spec best_solution ({solution(), pid()}, {solution(),pid()}|none) -> {solution(), pid()}.

best_solution(Solution, none) -> Solution;
best_solution(none, Solution) -> Solution;
best_solution(S1 = {{Cost1, _}, _}, S2 = {{Cost2, _}, _}) ->
    if
	Cost1 =< Cost2 -> S1;
	true -> S2
    end.
% The stuff with the pids is just so that we can see who's produced the best solution


%-spec collect_ants(non_neg_integer(), solution() | none) -> solution().

collect_colonies (0,{Best, Pid}, _Master_state, NewColonies, _ReceivedPIDs, _Duplicating) -> 
    {Best, Pid, NewColonies}; 
collect_colonies (Num_left, Current_Best, Master_state, Colonies, ReceivedPIDs, Duplicating) -> % or use lists:foldl.
    receive
	{colony_done, {New_Solution, Pid}} ->
%	    {Cost, _} = New_Solution,
%	    io:format("Solution ~p from ~p~n", [Cost, Pid]),
	    collect_colonies(Num_left-1, best_solution ({New_Solution, Pid}, Current_Best), Master_state, Colonies, ant_submaster:key_value_increment(Pid, ReceivedPIDs), Duplicating);
	{'EXIT', FailedPID, Reason} -> 
		#master_state{nodes=Nodes, num_Processes=Num_Processes} = Master_state,
		case ant_submaster:find_max_level(length(Nodes),Num_Processes) of
		{_MaxLevels=0,_NodeLength=0} -> 
			?Print(io_lib:format("Failure of a colony process with PID ~p detected by master process and reason ~p ", [FailedPID, Reason]));
		{_MaxLevels,_NodeLength} -> 
			?Print(io_lib:format("Failure of a sub-master process on the first level with PID ~p and reason ~p ", [FailedPID, Reason]))
		end,
		case recover_childs(FailedPID, Master_state, Colonies) of
		{no_updated, Colonies}->
			collect_colonies(Num_left, Current_Best, Master_state, Colonies, ReceivedPIDs, Duplicating);
		{updated, NewColonies}->
			case ant_submaster:key_search(FailedPID, ReceivedPIDs) of
				[]-> collect_colonies(Num_left-(1*Duplicating), Current_Best, Master_state, NewColonies, ReceivedPIDs, Duplicating);
				Value-> collect_colonies(Num_left-(Duplicating-Value), Current_Best, Master_state, NewColonies, ReceivedPIDs, Duplicating)
			end
		end	
	after
		?TimeOut ->
			?Print(io_lib:format("Timeout has occured in master process ~p on node ~p with Num_left ~p and received ~p ", [self(), node(), Num_left, ReceivedPIDs])),
			collect_colonies (Num_left, Current_Best, Master_state, Colonies, ReceivedPIDs, Duplicating)
    end.


loop (_Duplicating, 0, Colonies, {Best_solution, _}, _Inputs, _Params, _Master_state) -> 
    lists:foreach (fun ({Pid, _ProcessName, _ProcessIndex}) -> Pid ! {self(), stop_ants} end, Colonies),
    lists:foreach (fun (ID) -> receive ok -> ID; {'EXIT', _FailedPID, _Reason} -> ok end end, lists:seq(1,length(Colonies))), %% wait to collect all the acknowledges
    Best_solution;

loop (Duplicating, N, Colonies, Best_solution, Inputs, Params, Master_state) -> 
    lists:foreach (fun({Pid, _ProcessName, _ProcessIndex}) -> Pid ! {self(), run} end, Colonies),
    {{Cost, _} = New_Solution, Best_Pid, NewColonies} = collect_colonies (length(Colonies)*Duplicating, Best_solution, Master_state, Colonies, _ReceivedPIDs=[], Duplicating),

% Note that we're using the best solution from the previous generation.
% We could also try starting anew, relying on the pheromone matrix to guide 
% ants back to earlier best solutions if they can't do any better.

%    Improved_Solution = localsearch:vnd_loop(New_Solution, Inputs, Params),
% local search now done in colony, after every generation of ants.

    Improved_Solution = New_Solution,
    {Cost, _} = Improved_Solution,

    #params{verbose=Verbose, schedule=Print_schedule} = Params,
    case Verbose of
	true -> ?Print(io_lib:format("Iteration ~p: cost = ~p from ~p", [N, Cost, Best_Pid])),
		case Print_schedule of 
		    true -> ?Print(io_lib:format("Schedule -> ~p", [Improved_Solution]));
		    false -> ok
		end;
	false -> ok
    end,

    lists:foreach (fun({Pid, _ProcessName, _ProcessIndex}) -> 
			   if Pid =/= Best_Pid ->
				   %Pid ! {self(), {update, Improved_Solution}};
				   Pid ! {Best_Pid, {update, Improved_Solution}};
			      true -> ok
			   end
		   end, Colonies),

	#params{chaos=Chaos} = Params,
	#master_state{recovery=Recovery} = Master_state,
	if 
		Chaos==true andalso Recovery==false->
			?Print(io_lib:format("more than once on ~p", [node()])),
			lists:foreach (fun({Pid, _ProcessName, _ProcessIndex}) ->  Pid ! {run_the_chaos} end, Colonies),
			loop (Duplicating, N-1, NewColonies, {Improved_Solution, Best_Pid}, Inputs, Params#params{chaos=false}, Master_state);
		true ->
			loop (Duplicating, N-1, NewColonies, {Improved_Solution, Best_Pid}, Inputs, Params, Master_state)
	end.

run(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, Recovery, Starter) ->
	process_flag(trap_exit, true),
	#params{chaos=Chaos, printer=Printer} = Params,
	if 
		Printer==false->
			put(parent_printer, no_print);
		true->
			put(parent_printer, Params#params.parent_printer)
	end,

	if 
		Recovery -> ?Print(io_lib:format("Master process restarts at ~p on node ~p ", [time(), node()]));
		true -> 
			ok
    end,
	Master_state=#master_state{num_Jobs=Num_Jobs, num_Processes=Num_Processes, duplicating=Duplicating, num_Ants=Num_Ants, iter_Global=Iter_Global, iter_Local=Iter_Local, inputs=Inputs, params=Params, nodes=Nodes, recovery=Recovery},
	case ant_submaster:find_max_level(length(Nodes),Num_Processes) of
	{_MaxLevels=0,_NodeLength=0} -> 
		?Print(io_lib:format("Number of nodes (~p) with ~p processes per node is not enough to have any submaster node", [length(Nodes),Num_Processes])),
		GroupName="total_group",
		case s_group:new_s_group(GroupName, Nodes) of
		{ok, GroupName, _Nodes} ->
			ok;
		Error ->
			if 
				Recovery -> ok; %% previously the group has been created
				true -> 
					?Print(io_lib:format("sd_erlang: cannot create total_group for all nodes with error ~p" , [Error]))
			end
		end,
		Colonies = lists:map (fun(H) -> 
								ProcessIndex=index_of(H, Nodes),
								ProcessName=list_to_atom("colony_node_"++integer_to_list(ProcessIndex)),
								case s_group:whereis_name(GroupName, ProcessName) of
								undefined -> 
									ChildPID=spawn_link(H, ant_colony, init, [Num_Ants, Duplicating, Iter_Local, Num_Jobs, Inputs, Params, ProcessName, Recovery]),
									s_group:register_name(GroupName, ProcessName, ChildPID);
								ChildPID -> link(ChildPID)
								end,
								{ChildPID, ProcessName, ProcessIndex}
							  end, Nodes),
		ChildPIDs=lists:map (fun({ChildPID, _ProcessName, _ProcessIndex}) -> ChildPID end,Colonies),
		#params{chaos=Chaos} = Params,
		if 
			Chaos==true andalso Recovery==false ->
				Chaos_starter=util:get_local_name(chaos_starter),
				?Print(io_lib:format("master - sending ~p pids from node ~p to ~p " , [length(ChildPIDs), node(), Chaos_starter])),
				Chaos_starter! {pids, ChildPIDs};
				%Chaos_starter! {pids, []};
			true ->
				ok
		end,
		Results=loop(Duplicating, Iter_Global, Colonies, none, Inputs, Params, Master_state);
	{MaxLevels,NodeLength} ->
		?Print(io_lib:format("We have ~p levels for ~p nodes and ~p processes per node (~p nodes will be used)", [MaxLevels,length(Nodes),Num_Processes,NodeLength])),
		{UsingNodes,_Rest}=lists:split(NodeLength, Nodes),
		[Head|_Tail]=UsingNodes,
		Groups=s_group:s_groups(),
		ChildProcesses=lists:map(
		fun(ProcessIndex) ->
			ProcessName=ant_submaster:generate_submaster_name(_CurrentLevel=1, _NodeIndex=1, ProcessIndex),
			case Groups of
				undefined ->
					ChildPID=spawn_link(Head, ant_submaster, run, [Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params,UsingNodes, _CurrentLevel=1, MaxLevels, _NodeIndex=1, ProcessIndex, ProcessName, Recovery]),
					{ChildPID, ProcessName, ProcessIndex};
				{OwnSGroups,_} ->
					if 
						is_list(OwnSGroups) ->
							case length(OwnSGroups) of
								1 ->
									ok;
								_ ->
									?Print(io_lib:format("sd_erlang: (warning) master process belongs to more or less than one node ~p ~p", [length(OwnSGroups),OwnSGroups]))
							end,
							[GroupName|_Tail]=OwnSGroups,
							case s_group:whereis_name(GroupName, ProcessName) of
								undefined ->
									ChildPID=spawn_link(Head, ant_submaster, run, [Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params,UsingNodes, _CurrentLevel=1, MaxLevels, _NodeIndex=1, ProcessIndex, ProcessName, Recovery]),
									{ChildPID, ProcessName, ProcessIndex};
								ChildPID -> link(ChildPID),
									{ChildPID, ProcessName, ProcessIndex}
							end;
						true ->
							ChildPID=spawn_link(Head, ant_submaster, run, [Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params,UsingNodes, _CurrentLevel=1, MaxLevels, _NodeIndex=1, ProcessIndex, ProcessName, Recovery]),
							{ChildPID, ProcessName, ProcessIndex}
					end
			end
		end,
		lists:seq(1,Num_Processes)),
		ChildPIDs=lists:map (fun({ChildPID, _ProcessName, _ProcessIndex}) -> ChildPID end,ChildProcesses),
		#params{chaos=Chaos} = Params,
		if
			Chaos==true ->
				Chaos_starter=util:get_local_name(chaos_starter),
				?Print(io_lib:format("master - sending ~p pids from node ~p to ~p" , [length(ChildPIDs), node(), Chaos_starter])),
				Chaos_starter! {pids, ChildPIDs};
				%Chaos_starter! {pids, []};
			true ->
				ok
		end,
		Results=loop(1, Iter_Global, ChildProcesses, none, Inputs, Params, Master_state)
	end,
	Starter ! {self(), Results}.

%% Recovers a failed process
recover_childs(FailedPID, Master_state, Colonies) ->
	#master_state{
	num_Jobs=Num_Jobs,
	num_Processes=Num_Processes,
	duplicating=Duplicating,
	num_Ants=Num_Ants,
	iter_Global=Iter_Global,
	iter_Local=Iter_Local,
	inputs=Inputs,
	params=Params,
	nodes=Nodes} = Master_state,
	case get_process_name(FailedPID, Colonies) of
	not_found -> 
		?Print(io_lib:format("No recovery for colony process ~p is needed on node ~p", [FailedPID, node()])),
		{no_updated, Colonies};
	{ProcessName, ProcessIndex} ->
		case ant_submaster:find_max_level(length(Nodes),Num_Processes) of
		{_MaxLevels=0,_NodeLength=0} -> 
			Node=lists:nth(ProcessIndex, Nodes),
			NewPID=spawn_link (Node, ant_colony, init,[Num_Ants, Duplicating, Iter_Local, Num_Jobs, Inputs, Params,ProcessName, _Recovery=true]),
			util:send_pid(NewPID),
			NewChildProcesses=update_process_PID(ProcessName, NewPID, Colonies),
			?Print(io_lib:format("recovery of a colony process ~p detected by master process on node ~p by new length ~p and new PID ~p", [FailedPID, node(), length(NewChildProcesses), NewPID]));
		{MaxLevels,NodeLength} -> 
			{UsingNodes,_Rest}=lists:split(NodeLength, Nodes),
			[Head|_Tail]=UsingNodes,
			NewPID=spawn_link(Head, ant_submaster, run, [Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params,UsingNodes, _CurrentLevel=1, MaxLevels, _NodeIndex=1, ProcessIndex, ProcessName, _Recovery=true]),
			util:send_pid(NewPID),
			NewChildProcesses=update_process_PID(ProcessName, NewPID, Colonies),
			?Print(io_lib:format("recovery of a sub-master process ~p detected by master process on node ~p by new length ~p and new PID ~p", [FailedPID, node(), length(NewChildProcesses), NewPID]))
		end,
		{updated, NewChildProcesses}
	end.

%% gets a process identity and return its registred name
get_process_name(_ProcessPID,_Colonies=[]) ->
	not_found;
get_process_name(ProcessPID, [{ProcessPID, ProcessName, ProcessIndex}|_Tail]) ->
	{ProcessName,ProcessIndex};
get_process_name(ProcessPID, [{_Process, _ProcessName, _ProcessIndex}|Tail]) ->
	get_process_name(ProcessPID,Tail).

%% updates the pid of a failed process with the new pid
update_process_PID(ProcessName, NewPID, Colonies) ->
	update_process_PID(ProcessName, NewPID, Colonies, _ACC=[]).

update_process_PID(_ProcessName, _NewPID, [], _ACC)->
		throw(cannot_find_process_name_to_update);

update_process_PID(ProcessName, NewPID, [{_Process, ProcessName, ProcessIndex}|Tail], ACC)->
		ACC++[{NewPID, ProcessName, ProcessIndex}]++Tail;

update_process_PID(ProcessName, NewPID, [{ProcessID, ProcessName2, ProcessIndex}|Tail], ACC)->
		update_process_PID(ProcessName, NewPID, Tail, ACC++[{ProcessID, ProcessName2, ProcessIndex}]).

%% returns the index of an element of a list
index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).
