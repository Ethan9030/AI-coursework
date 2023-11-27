% Accomplish a given Task and return the Cost
solve_task(Task,Cost) :-
    my_agent(A), get_agent_position(A,P),
    solve_task_astar(Task, [[38,38,0,P|RPath]], [], Path),
    %solve_task_bfs(Task,[[P]],[],Path),   
    agent_do_moves(A,Path), 
    length(Path,Cost).

% Calculate the path required to achieve a Task using breadth-first search
solve_task_bfs(Task,Queue,Visited,Path) :-
    Queue = [Next | Rest],
    Next = [Pos | RPath],
    (achieved(Task,Pos) -> reverse([Pos|RPath],[_|Path])
    ;otherwise     -> (findall([NP,Pos|RPath],
                               (map_adjacent(Pos,NP,empty),\+ member(NP,Visited), \+ member([NP|_],Rest)),
                               Newfound),
                      append(Rest,Newfound,NewQueue),
                      solve_task_bfs(Task,NewQueue,[Pos|Visited],Path))).
    

% Calculate the path required to achieve a Task using A* search
solve_task_astar(Task,Queue,Visited,Path) :-
    Queue = [Next | Rest],
    Next = [_,_,G,Pos| RPath],
    (achieved(Task,Pos) -> reverse([Pos|RPath],Path)
    ;otherwise -> (findall(
        [NF,NH,NG,NP|RPath],
        (map_adjacent(Pos,NP,empty),
        NG is G + 1,
        manhattan_distance(NP,Pos,NH),
        NF is NG + NH,
        \+ member(NP,Visited), \+ member([_,_,_,NP|_],Rest)),
        Newfound
        ),
    append(Rest, Newfound, NewQueue),
    sort(NewQueue,SortedQueue),
    solve_task_astar(Task, SortedQueue, [Pos | Visited], Path))).

% True if the Task is achieved with the agent at Pos
achieved(Task,Pos) :- 
    Task=find(Obj), map_adjacent(Pos,_,Obj)
    ;
    Task=go(Pos).

% Manhattan distance between two positions
manhattan_distance(p(X1,Y1),p(X2,Y2),Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).


% (ground(Task) -> solve_task_astar(Task, [[38,38,0,P|RPath]], [], Path),   %something is wrong here?
%                      agent_do_moves(A,Path), 
%                      length(Path,Cost))
%     ;otherwise -> solve_task_bfs(Task,[[P]],[],Path),
%                   agent_do_moves(A,Path), 
%                   length(Path,Cost).
