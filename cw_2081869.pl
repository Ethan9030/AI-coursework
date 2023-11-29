% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), get_agent_position(A,P),
    get_agent_energy(1,Energy),
    (condition_go_true(Task) -> ( 
                            heruistic(Task,P,D),
                            solve_task_astar(Task, [(D:[P])], [], Path),  
                            length(Path,Cost),
                            ((Cost > Energy) -> (solve_task(find(c(X)),_), 
                                               agent_topup_energy(1,c(X)),  
                                               solve_task(Task,_))
                            ;((Energy - Cost) < 20) -> (solve_task(find(c(X)),_),  % not sure this is the right solution
                                               agent_topup_energy(1,c(X)),  
                                               solve_task(Task,_))  
                            ;otherwise -> agent_do_moves(A,Path)))
    ;condition_find_true(Task) -> (solve_task_bfs(Task,[[P]],[],Path),
                    agent_do_moves(A,Path), 
                    length(Path,Cost))).
    
% Calculate the path required to achieve a Task using A* search
solve_task_astar(Task,Queue,Visited,Path) :-
    Queue = [Next | Rest],
    Next = (_:[Pos|RPath]),
    (achieved(Task,Pos) -> reverse([Pos|RPath],[_|Path])
    ;otherwise -> (findall(
        (NF:[NP,Pos|RPath]),
        (map_adjacent(Pos,NP,empty),
        length(Queue,X), NG is X,
        heruistic(Task,NP,NH),
        NF is NG + NH,
        \+ member(NP,Visited), \+ member((_:[NP|_]),Rest)),
        Newfound
        ),
    append(Rest, Newfound, NewQueue),
    sort(NewQueue,SortedQueue),
    solve_task_astar(Task, SortedQueue, [Pos|Visited], Path))).

% Calculate the path required to achieve a Task
solve_task_bfs(Task,Queue,Visited,Path) :-
    Queue = [Next | Rest],
    Next = [Pos | RPath],
    (achieved(Task,Pos) -> reverse([Pos|RPath],[_|Path])
    ;otherwise     -> (findall([NP,Pos|RPath],
                               (map_adjacent(Pos,NP,empty),\+ member(NP,Visited), \+ member([NP|_],Rest)),
                               Newfound),
                      append(Rest,Newfound,NewQueue),
                      solve_task_bfs(Task,NewQueue,[Pos|Visited],Path))).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task=go(Pos).

% heruistic function to calculate the Distance from the Task
heruistic(Task,NP,NF) :-
    Task = go(Pos), map_distance(Pos,NP,NF).

% Two helper functions to help switch between target is known or unknown
condition_find_true(Task) :-
    Task = find(_).


condition_go_true(Task) :-
    Task = go(SubTask),
    ground(SubTask).
