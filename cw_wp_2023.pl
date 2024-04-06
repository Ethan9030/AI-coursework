% True if link L appears on A's wikipedia page
actor_has_link(L,A) :- 
    actor(A), wp(A,WT), wt_link(WT,L).

% Attempt to solve by 
eliminate(As,A,K,List) :- 
    As=[A], !
    ;
    solve_task(find(o(K)),_), 
    link(L1),  
    my_agent(N),
    agent_ask_oracle(N,o(K),L1,L),  
    include(actor_has_link(L),As,ViableAs),  
    delete(List,K,ResultList),   
    traverse_cost(ResultList,[],SortedQueue),   
    find(SortedQueue,K1),
    eliminate(ViableAs,A,K1,ResultList).   

% Deduce the identity of the secret actor A
find_identity(A) :- 
    ailp_grid_size(N),
    findall(T, (between(1,N,X), between(1,N,Y), lookup_pos(p(X,Y),o(T))), List),
    findall(A,actor(A),As), 
    eliminate(As,A,_,List).

% Return a sorted Queue which contains Cost and the Oracle number
traverse_cost([],SortedQueue,SortedQueue).
traverse_cost([H|T],Queue,SortedQueue) :-
    get_agent_position(1, P),
    solve_task_bfs(find(o(H)), [[P]], [], Path),     
    length(Path, X),
    Cost is X,
    NewQueue = [[Cost,[H]]|Queue],
    sort(NewQueue,TempQueue),
    traverse_cost(T,TempQueue,SortedQueue).


% Find the Oracle number with lowest Cost
find([[_,[N]]|_],N).