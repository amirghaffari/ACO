%% Creates a multi-level tree of sub-master nodes to collect the results from Colonies. 

%% Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
%% RELEASE project (http://www.release-project.eu/)

-module(ant_submaster).
-compile(export_all).
-include("types.hrl").


%% compares two solutions and returns the better one
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

%% collecting the collonies results from child nodes
collect_childs (0,{Best, Pid}, _SubMaster_state, NewChildProcesses, _ReceivedPIDs, _Duplicating) -> 
    {Best, Pid, NewChildProcesses}; 
collect_childs (Num_left, Current_Best, SubMaster_state, ChildProcesses, ReceivedPIDs, Duplicating) -> % or use lists:foldl.
    receive
    {afterFailure,NewChildProcesses, FailedPID} ->
		case key_search(FailedPID, ReceivedPIDs) of
			[]->   	collect_childs(Num_left-(1*Duplicating), Current_Best, SubMaster_state, NewChildProcesses, ReceivedPIDs, Duplicating);
			Value-> collect_childs(Num_left-(Duplicating-Value), Current_Best, SubMaster_state, NewChildProcesses, ReceivedPIDs, Duplicating)
		end;
	{colony_done, {New_Solution, Pid}} ->
		collect_childs(Num_left-1, best_solution ({New_Solution, Pid}, Current_Best), SubMaster_state, ChildProcesses, key_value_increment(Pid, ReceivedPIDs), Duplicating);
	{'EXIT', FailedPID, Reason} -> 
		#submaster_state{currentLevel=CurrentLevel, maxLevel=MaxLevel} = SubMaster_state,
		if
			CurrentLevel==MaxLevel-1 ->
				?Print(io_lib:format("Failure of a colony process with PID ~p and reason ~p ", [FailedPID, Reason]));
			true ->
				%?Print(io_lib:format("Failure of a sub-master process with PID ~p and reason ~p ", [FailedPID, Reason]))
				?Print(io_lib:format("Failure of a supervisor process with PID ~p and reason ~p ", [FailedPID, Reason]))
		end,
		case recover_childs(FailedPID, SubMaster_state, ChildProcesses) of
			{no_updated, ChildProcesses}->
				collect_childs(Num_left, Current_Best, SubMaster_state, ChildProcesses, ReceivedPIDs, Duplicating);
			{updated, NewChildProcesses}->
				case key_search(FailedPID, ReceivedPIDs) of
					[]->   	collect_childs(Num_left-(1*Duplicating), Current_Best, SubMaster_state, NewChildProcesses, ReceivedPIDs, Duplicating);
					Value-> collect_childs(Num_left-(Duplicating-Value), Current_Best, SubMaster_state, NewChildProcesses, ReceivedPIDs, Duplicating)
				end;
			{updated, NewChildProcesses, NewSupervisorPid}->
				Num_failed=find_num_failed(ChildProcesses, NewChildProcesses, ReceivedPIDs, Duplicating),
				collect_childs(Num_left-Num_failed, Current_Best, SubMaster_state#submaster_state{supervisor=NewSupervisorPid}, NewChildProcesses, ReceivedPIDs, Duplicating)
		end
	after
		?TimeOut ->
		#submaster_state{currentLevel=CurrentLevel, maxLevel=MaxLevel} = SubMaster_state,
		if
			CurrentLevel==MaxLevel-1 ->
				?Print(io_lib:format("Timeout has occurred on a last level sub-master process ~p on node ~p ", [self(), node()])),
				collect_childs (Num_left, Current_Best, SubMaster_state, ChildProcesses, ReceivedPIDs, Duplicating);
			true ->
				?Print(io_lib:format("Timeout has occurred on a sub-master process ~p on node ~p ", [self(), node()])),
				collect_childs (Num_left, Current_Best, SubMaster_state, ChildProcesses, ReceivedPIDs, Duplicating)
		end
    end.

find_num_failed(ChildProcesses, NewChildProcesses, ReceivedPIDs, Duplicating)->
	ChildPIDs=lists:map (fun({ChildPID, _ProcessName, _ProcessIndex}) -> ChildPID end,ChildProcesses),
	NewChildPIDs=lists:map (fun({ChildPID, _ProcessName, _ProcessIndex}) -> ChildPID end,NewChildProcesses),
	find_num_failed(ChildPIDs, NewChildPIDs, ReceivedPIDs, Duplicating, 0).

find_num_failed([], _NewChildPIDs, _ReceivedPIDs, _Duplicating, Acc) ->
	Acc;

find_num_failed([Head|Tail], NewChildPIDs, ReceivedPIDs, Duplicating, Acc) ->
	case lists:member(Head, NewChildPIDs) of
		false ->
			case key_search(Head, ReceivedPIDs) of
				[]->   	find_num_failed(Tail, NewChildPIDs, ReceivedPIDs, Acc+Duplicating);
				Val ->  find_num_failed(Tail, NewChildPIDs, ReceivedPIDs, Acc+(Duplicating-Val))
			end;
		true->
			find_num_failed(Tail, NewChildPIDs, ReceivedPIDs, Duplicating, Acc)
	end.

%% return the value of a key in a list of {key,value} tuples
key_search(_Key, []) ->
	[];
key_search(Key, [{Key, Val}|_Tail]) ->
	Val;
key_search(Key, [{_Key2, _Val}|Tail]) ->
	key_search(Key, Tail).

key_value_increment(Key, List) ->
	key_value_increment(Key, List, _Acc=[]).

key_value_increment(Key, [], Acc) ->
	Acc++[{Key,1}];

key_value_increment(Key, [{Key, Val}|Tail], Acc) ->	
	Acc++[{Key, Val+1}]++Tail;

key_value_increment(Key, [{Key2, Val}|Tail], Acc) ->	
	key_value_increment(Key, Tail, Acc++[{Key2, Val}]).

%% processing and passing all the messages from parent to childs and vice versa
loop(ChildProcesses, Best_solution, Duplicating, SubMaster_state) ->
    receive 
	{Parent, run} -> 
		lists:foreach (fun({Pid, _ProcessName, _ProcessIndex}) -> Pid ! {self(), run} end, ChildProcesses),
		{New_Solution, Best_Pid, NewChildProcesses} = collect_childs (length(ChildProcesses)*Duplicating, Best_solution, SubMaster_state, ChildProcesses, _ReceivedPID=[], Duplicating),		
	    Parent ! {colony_done, {New_Solution, Best_Pid}},
	    loop(NewChildProcesses, {New_Solution, Best_Pid}, Duplicating, SubMaster_state);

	{Best_Pid, {update, Global_Best_Solution}} -> 
		lists:foreach (fun({Pid, _ProcessName, _ProcessIndex}) -> 
			if Pid =/= Best_Pid ->
				Pid ! {Best_Pid, {update, Global_Best_Solution}};
				true -> ok
			end
		end, ChildProcesses),
		loop(ChildProcesses, {Global_Best_Solution,Best_Pid},Duplicating, SubMaster_state);

	{Parent, stop_ants} ->  % called by master at end of main loop
		case is_pid(SubMaster_state#submaster_state.supervisor) of
			false -> 
				ok; %% this sub-master does not have a supervisor process
			_-> 
				SubMaster_state#submaster_state.supervisor!{self(), stop_ants}
		end,
	    lists:foreach (fun ({Pid, _ProcessName, _ProcessIndex}) -> Pid ! {self(), stop_ants} end, ChildProcesses),
	    ?Print(io_lib:format("waiting for stop on node ~p " , [node()])),
	    lists:foreach (fun (ID) -> receive ok -> ID; {'EXIT', _FailedPID, _Reason} -> ok end end, lists:seq(1,length(ChildProcesses))),
	    Parent ! ok;

	{run_the_chaos} ->
		if 
			SubMaster_state#submaster_state.processIndex==1 ->
				Chaos_starter=util:get_local_name(chaos_starter),
				Chaos_starter!{run_the_chaos};
			true->
				ok
		end,
		lists:foreach (fun({Pid, _ProcessName, _ProcessIndex}) ->  Pid ! {run_the_chaos} end, ChildProcesses),
		loop(ChildProcesses, Best_solution, Duplicating, SubMaster_state)
    end.

%% creates appropriate processes on child nodes
run(Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params, Nodes, CurrentLevel, MaxLevel, NodeIndex,ProcessIndex, ProcessName, Recovery) ->
	process_flag(trap_exit, true),
	#params{chaos=Chaos, printer=Printer} = Params,
	if 
		Printer==false->
			put(parent_printer, no_print);
		true->
			put(parent_printer, Params#params.parent_printer)
	end,
	SubMaster_state=#submaster_state{num_Jobs=Num_Jobs, num_Processes=Num_Processes, duplicating=Duplicating, num_Ants=Num_Ants, iter_Global=Iter_Global, iter_Local=Iter_Local, inputs=Inputs, params=Params, nodes=Nodes, currentLevel=CurrentLevel, maxLevel=MaxLevel, nodeIndex=NodeIndex, processIndex=ProcessIndex, processName=ProcessName},
	if 
		Chaos==false andalso Printer==false ->
			Chaos_starter=no_print;
		true ->
			if
				ProcessIndex==1 andalso not Recovery->
					Chaos_starter=spawn(start_chaos, run, [get(parent_printer), Params#params.chaos, Params#params.printer]);
				true ->
					Chaos_starter=util:get_local_name(chaos_starter) %% chaos monkey is run on this node
			end
	end,

    if
		CurrentLevel==MaxLevel-1 ->
			ChildNodes=get_childs(Num_Processes,CurrentLevel,MaxLevel,Nodes,NodeIndex),
			QuotaOfEachProcess=round(length(ChildNodes)/Num_Processes),
			Start=(ProcessIndex-1)*QuotaOfEachProcess+1,
			MyChildNodes=lists:sublist(ChildNodes, Start, Num_Processes),
			GroupName="group_"++ProcessName,
			add_s_group_last_level(GroupName, [node()]++MyChildNodes),
			s_group:register_name(GroupName, ProcessName, self()),
			Colonies = lists:map (fun(H) -> 
									ProcessGlobalIndex=ant_master:index_of(H, Nodes),
									ChildProcessName=list_to_atom("colony_node_"++integer_to_list(ProcessGlobalIndex)),
									case s_group:whereis_name(GroupName, ChildProcessName) of
									undefined -> 
										ChildPID=spawn_link(H, ant_colony, init, [Num_Ants, Duplicating, Iter_Local, Num_Jobs, Inputs, Params#params{parent_printer=Chaos_starter}, ChildProcessName, Recovery]),
										s_group:register_name(GroupName, ChildProcessName, ChildPID);
									ChildPID -> link(ChildPID)
									end,
									{ChildPID, ChildProcessName, ProcessGlobalIndex}
								  end, MyChildNodes),
			ChildPIDs=lists:map (fun({ChildPID, _ProcessName, _ProcessIndex}) -> ChildPID end,Colonies),	
			if 
				Chaos==true andalso Recovery==false->
					?Print(io_lib:format("submaster - sending ~p pids of colonies from node ~p to ~p " , [length(ChildPIDs), node(), Chaos_starter])),
					Chaos_starter! {pids, ChildPIDs};
					%Chaos_starter! {pids, []};
				true ->
					ok
			end,
			loop(Colonies, none, Duplicating, SubMaster_state#submaster_state{chaos_starter=Chaos_starter});
		true ->
			ChildNodes=get_childs(Num_Processes,CurrentLevel,MaxLevel,Nodes,NodeIndex),
			ChildNode=lists:nth(ProcessIndex, ChildNodes),
			GroupName="group_"++atom_to_list(node()),
			add_s_group(GroupName, ProcessIndex, [node()]++[ChildNode]),
			s_group:register_name(GroupName, ProcessName, self()),
			SupervisorName=list_to_atom("sup_"++atom_to_list(ProcessName)),
			case s_group:whereis_name(GroupName, SupervisorName) of
			undefined -> 
				SupervisorPid=spawn_link(ChildNode,sup_submaster,run,[Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params#params{parent_printer=Chaos_starter}, Nodes, CurrentLevel+1, MaxLevel, Num_Processes*(NodeIndex-1)+ProcessIndex, Recovery, {ProcessName, self()}, GroupName]),
				s_group:register_name(GroupName, SupervisorName, SupervisorPid);
			SupervisorPid -> 
				link(SupervisorPid),
				SupervisorPid! {after_recovery,self()}
			end,

			receive
				{sup_submaster,ChildProcesses} ->
					ok
			end,


			ChildPIDs=lists:map (fun({ChildPID, _ProcessName, _ProcessIndex}) -> ChildPID end,ChildProcesses),
			if 
				Chaos==true andalso Recovery==false ->
					?Print(io_lib:format("submaster - sending ~p pids of colonies from node ~p to ~p " , [length(ChildPIDs), node(), Chaos_starter])),
					Chaos_starter! {pids, ChildPIDs++[SupervisorPid]};
					%Chaos_starter! {pids, []};
				true ->
					ok
			end,
			loop (ChildProcesses, none, _Duplicating=1, SubMaster_state#submaster_state{supervisor=SupervisorPid,chaos_starter=Chaos_starter})
	end.

add_s_group_last_level(GroupName, Nodes) ->
	case exist_group(GroupName) of
		true -> 
			ok;
		false ->
			case s_group:new_s_group(GroupName, Nodes) of
				{ok, GroupName, _Nodes} ->
					{ok, GroupName, _Nodes};
				Error ->
					?Print(io_lib:format("sd_erlang: an error occurs (~p) when trying to create group ~p on node ~p " , [Error, GroupName, node()]))
			end
	end.

add_s_group(GroupName, ProcessIndex, Nodes) ->
	if
		ProcessIndex==1 ->
			case exist_group(GroupName) of
				true -> 
					ok;
				false ->
					case s_group:new_s_group(GroupName, Nodes) of
						{ok, GroupName, _Nodes} ->
							{ok, GroupName, _Nodes};
						Error ->
							?Print(io_lib:format("sd_erlang: an error occurs (~p) when trying to create group ~p on node ~p for process index ~p " , [Error, GroupName, node(), ProcessIndex]))
					end
			end;
		true ->
			%% wait untill sgroup gets created
			case exist_group_with_wait(GroupName) of
				true-> 
					[_Head|Tail]=Nodes,
					s_group:add_nodes(GroupName, Tail);
					%?Print(io_lib:format("sd_erlang:add nodes ~p to group ~p for process index ~p on node ~p " , [Tail, GroupName, ProcessIndex, node()]));
				_->
					?Print(io_lib:format("sd_erlang: group name ~p is not in my groups on node ~p and process index ~p " , [GroupName, node(), ProcessIndex]))
			end
	end.

exist_group(GroupName) ->
	case s_group:s_groups() of
	undefined ->
		false;
	{OwnSGroups,_} ->
		if 
			is_list(OwnSGroups) ->
				case lists:member(GroupName, OwnSGroups) of
					true->
						true;
					_->
					false
				end;
			true ->
				false
		end
	end.

exist_group_with_wait(GroupName) ->
	exist_group_with_wait(GroupName, 10). %% waits 10 milliseconds for s_group to be created

exist_group_with_wait(_GroupName, 0) ->
	false;
exist_group_with_wait(GroupName, Times) ->
	case exist_group(GroupName) of
		true->
			true;
		_->
			timer:sleep(1),
			exist_group_with_wait(GroupName, Times-1)
	end.
	
%% auxiliary functions to create a tree of submaster nodes

%% calculates the number of levels in the tree
%% each node has "Num_Processes" processes
%% each process supervise one node in lower level, except the last level that each process supervises "Num_Processes" number of nodes

%% calculates the tree level based on the total number of nodes and node degree (degree of vertices)
find_max_level(Num_Nodes,Num_Processes) ->
	S=speculate_level(Num_Nodes,Num_Processes),
	find_max_level(Num_Nodes,Num_Processes,S).

find_max_level(Num_Nodes,Num_Processes,Speculated_level) ->
	Result=calc_formula(Num_Processes,Speculated_level),
	if
		Result ==0 -> {_Level=0,_NumNodes=0};
		Result =< Num_Nodes -> {Speculated_level,Result};
		true -> find_max_level(Num_Nodes,Num_Processes,Speculated_level-1) 
	end.

%% finds the largest possible power for Num_Processes to be less than Num_Nodes
speculate_level(Num_Nodes,Num_Processes)->
	speculate_level(Num_Nodes,Num_Processes,0).

speculate_level(Num_Nodes,Num_Processes,Acc)->
	Result=math:pow(Num_Processes,Acc),
	if
		 Result<Num_Nodes ->
			 speculate_level(Num_Nodes,Num_Processes,Acc+1);
		true ->
			round(Acc-1)
	end.

%% calculates 1+P^1+P^2+...+P^(N-2)+P^N
calc_formula(Num_Processes,Level) when Level>=2 ->
	calc_formula(Num_Processes,Level,_Acc=0,_Current_level=0);

%% No submaster can be allocated
calc_formula(_Num_Processes,_Level) ->
	0.

calc_formula(Num_Processes,Last_Level,Acc,Current_level) when Current_level=<Last_Level ->
	Num_Nodes=math:pow(Num_Processes,Current_level),
	case Current_level+2 of
		Last_Level ->
			calc_formula(Num_Processes,Last_Level,Acc+Num_Nodes,Current_level+2);
		_-> 
			calc_formula(Num_Processes,Last_Level,Acc+Num_Nodes,Current_level+1)
	end;

calc_formula(_Num_Processes,_Last_Level,Acc,_Current_level) ->
	round(Acc).

%% returns number of nodes for a specific level
nodes_in_level(Num_Processes, Level) ->
	round(math:pow(Num_Processes,Level-1)).

%% returns all the child nodes of a specific node. Node is specified by its level and its index in the level
%% How to test: Nodes=lists:seq(1, 277). list_to_tuple(ant_submaster:get_childs(4,3,4,Nodes,1)).
get_childs(Num_Processes,CurrentLevel,MaxLevel,Nodes,IndexInLevel) -> %when CurrentLevel<MaxLevel ->
	Num_Node_in_Current_Level=nodes_in_level(Num_Processes,CurrentLevel),
	if
		Num_Node_in_Current_Level<IndexInLevel ->
			throw(index_in_level_is_more_than_num_nodes_in_level);
		true -> ok
	end,
	if
		CurrentLevel>=MaxLevel -> 
			Num_Node_in_Next_Level=0,
			throw(current_level_must_be_less_than_max_level);
		CurrentLevel==MaxLevel-1 ->
			Num_Node_in_Next_Level=nodes_in_level(Num_Processes,CurrentLevel+2);			
		true->
			Num_Node_in_Next_Level=nodes_in_level(Num_Processes,CurrentLevel+1)
	end,
	Childs_Per_Node=Num_Node_in_Next_Level/Num_Node_in_Current_Level,
	Index_For_Next_Line=Childs_Per_Node*(IndexInLevel-1)+1,
	After_Me_This_Level=Num_Node_in_Current_Level-IndexInLevel,
	Child_index=level_index(Num_Processes,CurrentLevel)+IndexInLevel+After_Me_This_Level+Index_For_Next_Line,lists:sublist(Nodes, round(Child_index), round(Childs_Per_Node)).

%% returns the index of the first node for a specific level
level_index(Num_Processes,Level) ->
	level_index(Num_Processes,Level,0,0).

level_index(Num_Processes,Level,Acc,CurrentLevel) when CurrentLevel<Level-1 ->
	R=math:pow(Num_Processes,CurrentLevel),
	level_index(Num_Processes,Level,Acc+R,CurrentLevel+1);

level_index(_Num_Processes,_Level,Acc,_CurrentLevel)->
Acc.

generate_submaster_name(Level, NodeIndex, ProcessIndex) ->
	Name=integer_to_list(Level)++integer_to_list(NodeIndex)++integer_to_list(ProcessIndex),
	list_to_atom(Name).

%% Recovers a failed process
recover_childs(FailedPID, SubMaster_state, ChildProcesses) ->
	#submaster_state{
	num_Jobs=Num_Jobs, 
	num_Processes=Num_Processes, 
	processName=ProcessName,
	duplicating=Duplicating, 
	num_Ants=Num_Ants, 
	iter_Global=Iter_Global, 
	iter_Local=Iter_Local, 
	inputs=Inputs, 
	params=Params, 
	nodes=Nodes, 
	currentLevel=CurrentLevel, 
	maxLevel=MaxLevel, 
	nodeIndex=NodeIndex, 
	processIndex=ProcessIndex,
	supervisor=SupervisorPid,
	chaos_starter=Chaos_starter} = SubMaster_state,
    if
		CurrentLevel==MaxLevel-1 ->
			case ant_master:get_process_name(FailedPID, ChildProcesses) of
			not_found -> 
				?Print(io_lib:format("No recovery for colony process ~p is needed on node ~p ", [FailedPID, node()])),
				{no_updated, ChildProcesses};
			{ChildProcessName, ProcessGlobalIndex} ->
				Node=lists:nth(ProcessGlobalIndex, Nodes),
				NewPID=spawn_link(Node, ant_colony, init, [Num_Ants, Duplicating, Iter_Local, Num_Jobs, Inputs, Params, ChildProcessName, _Recovery=true]),
				util:send_pid(NewPID),
				GroupName="group_"++ProcessName,
				s_group:register_name(GroupName, ChildProcessName, NewPID),
				NewChildProcesses=ant_master:update_process_PID(ChildProcessName, NewPID, ChildProcesses),
				?Print(io_lib:format("recovery of a colony process ~p on node ~p by new length ~p and new PID ~p ", [FailedPID, node(), length(NewChildProcesses), NewPID])),
				{updated, NewChildProcesses}
			end;
		true ->
			if
				SupervisorPid==FailedPID ->
					?Print(io_lib:format("Failure of a supervisor process with pid ~p on node ~p", [FailedPID, node()])),
					ChildNodes=get_childs(Num_Processes,CurrentLevel,MaxLevel,Nodes,NodeIndex),
					ChildNode=lists:nth(ProcessIndex, ChildNodes),
					GroupName="group_"++atom_to_list(node()),
					NewSupervisorPid=spawn_link(ChildNode,sup_submaster,run,[Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params#params{parent_printer=Chaos_starter}, Nodes, CurrentLevel+1, MaxLevel, Num_Processes*(NodeIndex-1)+ProcessIndex, _Recovery=true, {ProcessName, self()}, GroupName]),
					SupervisorName=list_to_atom("sup_"++atom_to_list(ProcessName)),
					s_group:register_name(GroupName, SupervisorName, NewSupervisorPid),
					receive
						{sup_submaster,ChildProcesses} ->
							ok
					end,
					{updated, ChildProcesses, NewSupervisorPid};
				true ->
					case ant_master:get_process_name(FailedPID, ChildProcesses) of
					not_found -> 
						?Print(io_lib:format("No recovery for sub-master process ~p is needed on node ~p ", [FailedPID, node()])),
						{no_updated, ChildProcesses};
					{ChildProcessName, NextProcessIndex} ->
						ChildNodes=get_childs(Num_Processes,CurrentLevel,MaxLevel,Nodes,NodeIndex),
						ChildNode=lists:nth(ProcessIndex, ChildNodes),
						NewPID=spawn_link(ChildNode,ant_submaster,run,[Num_Jobs, Num_Processes, Duplicating, Num_Ants, Iter_Global, Iter_Local, Inputs, Params,Nodes, CurrentLevel+1, MaxLevel,Num_Processes*(NodeIndex-1)+ProcessIndex,NextProcessIndex, ChildProcessName, _Recovery=true]),
						util:send_pid(NewPID),
						GroupName="group_"++atom_to_list(node()),
						s_group:register_name(GroupName, ChildProcessName, NewPID),
						NewChildProcesses=ant_master:update_process_PID(ChildProcessName, NewPID, ChildProcesses),
						?Print(io_lib:format("recovery of a sub-master process ~p on node ~p by new length ~p and new PID ~p ", [FailedPID, node(), length(NewChildProcesses), NewPID])),
						{updated, NewChildProcesses}
					end
			end
	end.
