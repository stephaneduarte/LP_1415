%Projeto LP

%Puzzle 8
%81186 - Stephane Duarte
%81858 - Joao Oliveira

imprimeum(C) :- imprimeum(C, 0).
imprimeum([], _) :- !.
imprimeum([V|C], Pos) :- escvariavel(V), 
						Pos1 is Pos + 1, 
						escespaco(Pos1), 
						imprimeum(C, Pos1).

imprimedois(C1,C2) :- write('Transformacao desejada:'), nl, nl,
						escvariavelC(C1,0), write('    '), 
						escvariavelC(C2,0), nl,
						escvariavelC1(C1), write(' -> '), 
						escvariavelC1(C2), nl,
						escvariavelC2(C1), write('    '), 
						escvariavelC2(C2), nl.
						
escvariavelC(_,3) :- !.
escvariavelC([V|C], Cont) :- escvariavel(V), 
						write(' '), 
						Cont1 is Cont + 1, 
						escvariavelC(C, Cont1).

escvariavelC1([_,_,_|C]) :- escvariavelC(C, 0), !.
escvariavelC2([_,_,_|C]) :- escvariavelC1(C), !.

escespaco(Pos) :- Pos mod 3 =\= 0, 
						write(' ').
escespaco(Pos) :- Pos mod 3 =:= 0, 
						nl.

escvariavel(0) :- write(' ').
escvariavel(V) :- write(V).

escmovimento([]).
escmovimento([Mov|Movs]) :- write('mova a peca '), 
						escmovimento1(Mov), 
						escmovimento(Movs).
escmovimento1([P|X]) :- X == [c], 
						write(P), 
						write(' para cima.'), nl.
escmovimento1([P|X]) :- X == [b], 
						write(P), 
						write(' para baixo.'), nl.
escmovimento1([P|X]) :- X == [e], 
						write(P), 
						write(' para a esquerda.'), nl.
escmovimento1([P|X]) :- X == [d], 
						write(P), 
						write(' para a direita.'), nl.

zero([0|_], Pos, Pos) :- !.
zero([_|C], Cont, Pos) :- Cont2 is Cont + 1, 
						zero(C, Cont2, Pos).

mov_legal(C1, M, P, C2) :- zero(C1, 0, Pos), 
						mov_legal(C1, M, P, C2, Pos).
mov_legal(C1, c, P, C4, Pos) :- Pos < 6, 
						Pos1 is Pos + 3, 
						peca(Pos1, P, C1), 
						mudanca(P,10,C1,C2), 
						mudanca(0,P,C2,C3), 
						mudanca(10,0,C3,C4).
mov_legal(C1, b, P, C4, Pos) :- Pos > 2, 
						Pos1 is Pos - 3, 
						peca(Pos1, P, C1), 
						mudanca(P,10,C1,C2), 
						mudanca(0,P,C2,C3), 
						mudanca(10,0,C3,C4).
mov_legal(C1, e, P, C4, Pos) :- Pos2 is Pos - 1, 
						Pos2 mod 3 =:= 0, 
						Pos1 is Pos + 1, 
						peca(Pos1, P, C1), 
						mudanca(P,10,C1,C2), 
						mudanca(0,P,C2,C3), 
						mudanca(10,0,C3,C4).
mov_legal(C1, e, P, C4, Pos) :- Pos mod 3 =:= 0, 
						Pos1 is Pos + 1, 
						peca(Pos1, P, C1), 
						mudanca(P,10,C1,C2), 
						mudanca(0,P,C2,C3), 
						mudanca(10,0,C3,C4).
mov_legal(C1, d, P, C4, Pos) :- Pos mod 3 =\= 0, 
						Pos1 is Pos - 1, 
						peca(Pos1, P, C1), 
						mudanca(P,10,C1,C2), 
						mudanca(0,P,C2,C3), 
						mudanca(10,0,C3,C4).


resolve_manual(C1,C2) :- imprimedois(C1,C2), 
						resolve_manual1(C1,C2).
resolve_manual1(C2,C2) :- write('Parabens!'), !.
resolve_manual1(C1,C2) :- write('Qual o seu movimento?'), nl, 
						read(X), 
						mov_legal(C1, X, _, C3), 
						imprimeum(C3), 
						resolve_manual1(C3,C2).
resolve_manual1(C1,C2) :- write('Movimento ilegal.'), nl, 
						resolve_manual1(C1,C2).


resolve_cego(C1,C2) :- imprimedois(C1,C2), 
						resolve_cego(C1,C2,[C1],[]).
resolve_cego(C2,C2,_,Movs) :- inverte(Movs,Movs2), 
						escmovimento(Movs2),
						!.
resolve_cego(C1,C2,Tabs,Movs) :- mov_legal(C1, X, P, C3), 
						\+ verificarjogada(C3, Tabs), 
						adicionar(C3, Tabs, Tabs2), 
						adicionar([P,X],Movs,Movs2), 
						resolve_cego(C3,C2,Tabs2,Movs2).

resolve_info_h(C1,C2) :- resolve_info(C2,[C1],[]).
resolve_info(_,[],_) :- write('Nao ha solucao').
resolve_info(C2,Ab,Fe) :- escolhe(C2, Ab, Fe, T),
						\+ iguais(C2, T), !,
						adicionar(T, Fe, Fe2),
						retirar(T, Ab, Ab2),
						expande(T, Ab2, Fe2, Ab3),
						resolve_info(C2, Ab3, Fe2).
resolve_info(C2,Ab,Fe) :- escolhe(C2, Ab, Fe, T),
						adicionar(T, Fe, Fe2),
						inverte(Fe2,Fe3),
						escreve_mov(Fe3), !.

transformacao_possivel(C1,C2) :- contador(C1, Cont1),
						contador(C2, Cont2),
						Mod1 is Cont1 mod 2,
						Mod2 is Cont2 mod 2, !,
						Mod1 =:= Mod2.

contador(C, Cont) :- contador(C, [], 0, Cont).
contador([], _, Cont, Cont).
contador([Elem|C], Ant, Cont1, Cont) :- Elem =\= 0,
						maiores(Elem,Ant,0,Cont2),
						Cont3 is Cont1 + Cont2,
						adicionar(Elem,Ant,Ant2),
						contador(C,Ant2,Cont3,Cont).
contador([Elem|C], Ant, Cont1, Cont) :- contador(C, Ant, Cont1, Cont).

maiores(_,[],Cont1,Cont1).
maiores(Elem,[A|L],Cont1,Cont) :- A > Elem,
						Cont2 is Cont1 + 1,
						maiores(Elem,L,Cont2,Cont).
maiores(Elem,[A|L],Cont1,Cont) :- maiores(Elem,L,Cont1,Cont).
						
iguais(A,A).

escreve_mov(Fe2) :- escreve_mov(Fe2,[]).
escreve_mov([X,Y|Fe],Movs) :- mov_legal(X,M,P,Y),
						adicionar([P,M],Movs,Movs2),
						escreve_mov([Y|Fe],Movs2).
escreve_mov(_,Movs) :- inverte(Movs,Movs2),
						escmovimento(Movs2).

						
calc_f(C1,C2,G,F) :- calc_h(C1,C2,H), 
						F is H + G, !.
						
calc_h(C1,C2,H) :- calc_h(C1,C2,0,H).
calc_h([],[],Cont,Cont).
calc_h([P1|R1],[P2|R2],Cont,H) :- P1 =\= P2, 
						P1 =\= 0, 
						Cont1 is Cont+1, 
						calc_h(R1,R2,Cont1,H).
calc_h([_|R1],[_|R2],Cont,H) :- calc_h(R1,R2,Cont,H).

escolhe(C2, Ab, Fe, T) :- calc_elem(Fe,Cont),
						Cont1 is Cont + 8,
						escolhe(C2,Ab,Cont,Cont1,_,T).
escolhe(_, [], _, _, T, T) :- !.
escolhe(C2, [T1|Ab], G, FMax, T2, T) :- calc_f(T1,C2,G,F),
						F < FMax,
						escolhe(C2, Ab, G, F, T1, T),!.
escolhe(C2, [T1|Ab], G, FMax, T2, T) :- escolhe(C2, Ab, G, FMax, T2, T).

encontra(T, L) :- findall(C2, mov_legal(T, _, _, C2), L).

expande(T, Ab, Fe, Ab2) :- findall(C2, mov_legal(T, _, _, C2), L),
						verificatabs(L, Ab, Fe, Ab2).
							
verificatabs([], Ab2, _, Ab2).
verificatabs([T|L], Ab, Fe, Ab3) :- \+ verificarjogada(T, Ab),
						\+ verificarjogada(T, Fe),
						adicionar(T,Ab,Ab2), !,
						verificatabs(L,Ab2,Fe,Ab3).
verificatabs([T|L], Ab, Fe, Ab3) :- verificatabs(L, Ab, Fe, Ab3).

calc_elem(Tab,Nr) :- calc_elem(Tab,0,Nr).
calc_elem([],Cont,Cont).
calc_elem([_|L], Cont, Nr) :- Cont1 is Cont + 1, 
						calc_elem(L,Cont1,Nr).

adicionar(A,L,[A|L]).

retirar(A,[A|L],L).
retirar(A,[B|L1],[B|L2]) :- retirar(A,L1,L2).

verificarjogada(Jogada,[Jogada|_]).
verificarjogada(Jogada,[_|L]) :- verificarjogada(Jogada,L).

junta([],L,L).
junta([P|R],L1,[P|L2]) :- junta(R,L1,L2).

inverte([],[]).
inverte([P|R],I) :- inverte(R,I1), 
						junta(I1,[P],I).

%NAO SABEMOS COMOOOOOOOOOOOOOOOOOOOOOOOOO

peca(0, Peca, [Peca|_]).
peca(N, Peca, [_|C]) :- peca(M, Peca, C), 
						N is M + 1.

mudanca(_, _, [], []).
mudanca(A, B, [A|C1], [B|C2]) :- mudanca(A,B,C1,C2).
mudanca(A, B, [C|C1], [C|C2]) :- A =\= C, 
						mudanca(A,B,C1,C2).