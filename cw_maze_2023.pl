% Solve the maze, aiming to get all the agents to p(N,N)
solve_maze :-
    solve_maze([]).
    
solve_maze(Visited) :- 
    my_agents(Agents),
    find_moves(Agents,Moves,Visited,NewVisited),  
    agents_do_moves(Agents,Moves),
    traverse_agents(Agents),
    solve_maze(NewVisited).

%%%%%%%%%%%%%%%% USEFUL PREDICATES %%%%%%%%%%%%%%%%%%
% Find a possible move for each agent
find_moves([],[],Visited,Visited).
find_moves([A|As],[M|Moves],Visited,NewVisited) :-
    findall(P,(agent_adjacent(A,P,empty), \+ member(P,Visited)),PosMoves),  
    get_agent_position(A,Pos),
    length(PosMoves,X), L is X,
    ((L = 1) -> [M] = PosMoves,                            
                find_moves(As,Moves,[Pos|Visited],NewVisited)
    % ;(L = 0) -> M = [],
    %             find_moves(As,Moves,Visited,NewVisited)                    
    ;otherwise -> include(not_member(Visited), PosMoves, NextMoves),
                  random_member(M,NextMoves),     % Add something here to find M that is not in Visited
                  find_moves(As,Moves,Visited,NewVisited)).

% Traverse agents to see if they reach p(N,N)
traverse_agents([]).
traverse_agents([A|Rest]) :-    
    get_agent_position(A,P), 
    ailp_grid_size(N),
    P = p(N,N) -> leave_maze(A)
    ;
    traverse_agents(Rest).

% Write to use with include predicate     
 not_member(X, List) :- 
    \+ member(X, List).