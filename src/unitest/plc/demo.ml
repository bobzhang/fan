



%plc{
%:nqueens(+N,?Board)
nqueens(N,Board) :-  range(1,N,L),  permutation(L,Board),  safe(Board).
%:range(+Start,+Stop,?Result)
range(M,N,[M|L]) :- M < N, M1 is M+1, range(M1,N,L).
range(N,N,[N]).
%:permutation(+List,?Result)
permutation([],[]).
permutation([A|M],N) :- permutation(M,N1), insert(A,N1,N).
%:insert(+Element,+List,?Result)
%:insert(?Element,+List,+Result)
%:insert(+Element,?List,+Result)
insert(A,L,[A|L]).
insert(A,[B|L],[B|L1]) :- insert(A,L,L1).
%:safe(+Board)
safe([_]).
safe([Q|Qs]) :- nodiag(Q,Qs,1), safe(Qs).
%:nodiag(+Queen,+Board,+Dist)
nodiag(_,[],_).
nodiag(Q1,[Q2|Qs],D) :-  noattack(Q1,Q2,D),  D1 is D+1,  nodiag(Q1,Qs,D1).
%:noattack(+Queen1,+Queen2,+Dist)
noattack(Q1,Q2,D) :-  Q2-Q1 =\= D,  Q1-Q2 =\= D. };
nqueens_co (fun b -> print_endline (string_of_plval b)) (Int 10);
(*


safe([_]).
safe([Q|Qs]) :- nodiag(Q,Qs,1), safe(Qs).

%:nodiag(+Queen,+Board,+Dist)
nodiag(_,[],_).
nodiag(Q1,[Q2|Qs],D) :-
  noattack(Q1,Q2,D),
  D1 is D+1,
  nodiag(Q1,Qs,D1).

%:noattack(+Queen1,+Queen2,+Dist)
noattack(Q1,Q2,D) :-
  Q2-Q1 =\= D,
  Q1-Q2 =\= D. *)

