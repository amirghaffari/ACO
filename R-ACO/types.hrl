-type intlist   () :: [integer()].
-type intvec    () :: tuple().  % We really want a tuple of ints here
-type floatvec  () :: tuple().  % We really want a tuple of floats here: I don't think the type grammar can express this.

-type inputs    () :: {Durations::intvec(), Weights::intvec(), Deadlines::intvec()}.
-type cost      () :: integer().
-type index     () :: pos_integer().
-type schedule  () :: [pos_integer()].
-type solution  () :: {cost(), schedule()}.
-type numjobs   () :: pos_integer().
-type heuristic () :: edd | mdd | au | mixed.
-type idxf      () :: {index(), float()}.   
%     ^^^^ 
% Indexed float. Quite often we're working with "vectors" where we're
% only interested in the entries corresponding to unscheduled jobs.
% We represent these by lists of indexed floats.  These also seem to
% be a better functional representation than tuples. 

-record(params, {
	  alpha     = 1.0,    % Importance of pheromone information
	  beta      = 2.0,    % Importance of heuristic information
	  q0        = 0.9,    % Amount of non-determinism in job selection
	  rho       = 0.1,    % Pheromone evaporation rate
	  heuristic = mdd,    % Heuristic strategy: mdd, edd, or au
	  verbose   = false, 
	  vverbose  = false, 
	  schedule  = false,  % print schedule?
	  evaporate = true,   % Evaporate pheromone during update?
	  search    = none,   % Local search strategy
	  seed_now  = false,  % use now() for random seeds?
	  tau0      = undefined,      % Initial value for pheromone trails - calculated after inputs are read
	  chaos     = false	  % runs chaos monkey to benchmark reliability
	 }
       ).

-type params() :: #params{}.

-record(master_state, {
	  num_Jobs,
	  num_Processes,
	  duplicating,
	  num_Ants,
	  iter_Global,
	  iter_Local,
	  inputs,
	  params,
	  nodes,
	  colonies
	 }
       ).

-record(submaster_state, {
	  num_Jobs,
	  num_Processes,
	  duplicating,
	  num_Ants,
	  iter_Global,
	  iter_Local,
	  inputs,
	  params,
	  nodes,
	  currentLevel,
	  maxLevel,
	  nodeIndex,
	  processIndex,
	  status=0,
	  supervisor,
	  processName
	 }
       ).

-record(colony_state, {
	  num_Jobs,
	  inputs,
	  params
	 }
       ).

-type colony_state() :: #colony_state{}.

-define(TimeOut, 10000).


