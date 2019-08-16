/*Projeto LP*/

/*Puzzle 8*/

/*Grupo - 36*/
/*81186 - Stephane Duarte*/
/*81858 - Joao Oliveira*/


/*--------------------------------------------------------------------------------------------------------------*/
/*----------------------------------------------FUNCOES PRINCIPAIS----------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------------*/


/* Funcao: resolve_manual */
/* Funcao que executa movimentos introduzidos pelo utilizador ate atingir a configuracao desejada. */
resolve_manual(C1,C2) :- transformacao_possivel(C1,C2), /* Verifica se a transformacao de C1 em C2 e' possivel. */
						imprimedois(C1,C2), /* Imprime as configuracoes da transformacao desejada. */
						resolve_manual1(C1,C2). 
/* Quando se atingir a transformacao desejada, o jogo acaba. */
resolve_manual1(C2,C2) :- writeln('Parabens!'), !. /* Escreve uma mensagem de Parabens. */
/* Enquanto nao se atinge a transformacao desejada: */
resolve_manual1(C1,C2) :- write('Qual o seu movimento?'), nl, /* Pede um movimento ao utilizador. */
						read(X), /* Recebe o movimento introduzido pelo utilizador. */
						mov_legal(C1, X, _, C3), /* Verifica se o movimento introduzido e' valida. */
						imprimeum(C3), /* Imprime a configuracao apos o movimento introduzido. */
						resolve_manual1(C3,C2). 
/* Quando a jogada introduzida e' invalida: */						
resolve_manual1(C1,C2) :- write('Movimento ilegal'), nl, /* Avisa o utilizador de que o movimento e' ilegal. */
						resolve_manual1(C1,C2).


/* Funcao: resolve_cego */
/* Funcao que executa movimentos segundo uma ordem ate atingir a configuracao desejada. */
resolve_cego(C1,C2) :- transformacao_possivel(C1,C2), /* Verifica se a transformacao de C1 em C2 e' possivel. */
						imprimedois(C1,C2), /* Imprime as configuracoes da transformacao desejada. */
						resolve_cego(C1,C2,[C1],[]). 
/* Quando se atingir a transformacao desejada, o jogo acaba. */
resolve_cego(C2,C2,_,Movs) :- inverte(Movs,Movs2), /* Inverte a lista dos movimentos. */
						escmovimento(Movs2), !. /* Escreve a lista de movimentos a efetuar. */
/* Enquanto nao atingir a transformacao desejada: */					
resolve_cego(C1,C2,Tabs,Movs) :- mov_legal(C1, X, P, C3), /* Encontra um movimento possivel. */
						\+ verificarjogada(C3, Tabs), /* Verifica se a configuracao apos o movimento ja existiu. */
						/* (Desta forma nao entrara' num ciclo infinito) */ 
						adicionar(C3, Tabs, Tabs2), /* Adiciona a configuracao a lista de configuracoes. */
						adicionar([P,X],Movs,Movs2), /* Adiciona o movimento a lista de movimentos. */
						resolve_cego(C3,C2,Tabs2,Movs2).


/* Funcao: resolve_info_h */
/* Funcao que executa movimentos segundo o Algoritmo A* ate atingir a configuracao desejada. */
resolve_info_h(C1,C2) :- transformacao_possivel(C1,C2),/* Verifica se a transformacao de C1 em C2 e' possivel. */
						imprimedois(C1,C2), /* Imprime as configuracoes da transformacao desejada. */
						calc(C1,C2,0,F,H), /* Calcula o f da configuracao inicial. */
						resolve_info(C2,[[C1, F, 0, H,[]]],[]).
resolve_info_h(C1,C2) :- \+ transformacao_possivel(C1,C2), write('Nao ha solucao').
resolve_info(C2, Ab, Fe) :- no_menor(Ab, T), /* Procura o no com menor f. */
						retirar(T,Ab,Ab2), /* Remove o no com menor f da lista de abertos. */
						adicionar(T,Fe,Fe2), /* Adiciona o no 'a lista de fechados. */
						\+ tiguais(T,C2), !, /* Se a configuracao presente no no' for a desejada, o jogo acaba. */
						expande(T, C2, Fe, Ab2, Ab3), /* Expande o no com menor f. */
						resolve_info(C2, Ab3, Fe2). /* Repete o processo. */
/* Quando o jogo acaba: */
resolve_info(_, Ab, _) :- no_menor(Ab, T), /* Procura novamente o no com menor f. */
						movimentos(T,Movs), /* Retira os movimentos do no'.*/
						inverte(Movs,Movs1), /* Inverte a lista de movimentos. */
						escmovimento(Movs1). /* Escreve a lista de movimentos a efetuar. */


/*--------------------------------------------------------------------------------------------------------------*/
/*-------------------------------------------------FUNCAO EXTRA-------------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------------*/


/* Funcao: transformacao_possivel */
/* Funcao que verifica se é possivel transformar uma configuração noutra. */
transformacao_possivel(C1,C2) :- contador(C1, Cont1), /* Conta para cada elemento da lista que representa a configuracao inicial */
													  /* quantos sao superiores a ele, estando atrás na lista. */
						contador(C2, Cont2), /* Calcula a soma de todos os elementos da lista que representa a configuracao desejada */
											 /* que sao superiores a eles, estando atrás deles (na lista). */
						Mod1 is Cont1 mod 2, 
						Mod2 is Cont2 mod 2, !,
						Mod1 =:= Mod2. /* Verifica se o resto da divisao por 2 do contador da conf. inicial é igual ao do contador da conf. desejada. */


/*--------------------------------------------------------------------------------------------------------------*/
/*--------------------------------------------FUNCOES COMPLEMENTARES--------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------------*/


/* Funcao: imprimedois */
/* Funcao que imprime duas listas na forma de configuracoes, lado a lado. */
imprimedois(C1,C2) :- write('Transformacao desejada:'),nl,
						escvariavelC(C1,0), write('    '), /* Escreve os primeiros 3 valores de uma lista e espaços. */ 
						escvariavelC(C2,0), nl, /* Escreve os primeiros 3 valores de uma lista e passa para a linha seguinte. */
						escvariavelC1(C1), write(' -> '), /* Escreve os 3 valores do meio de uma lista, espaços e uma seta. */ 
						escvariavelC1(C2), nl, /* Escreve 3 valores do meio de uma lista e passa para a linha seguinte. */
						escvariavelC2(C1), write('    '), /* Escreve os ultimos 3 valores de uma lista e espaços. */ 
						escvariavelC2(C2), nl. /* Escreve os ultimos 3 valores de uma listas e passa para a linha seguinte. */

/* Funcao: imprimeum */
/* Funcao que imprime uma lista na forma de configuracao. */
imprimeum(C) :- nl, imprimeum(C, 0).
imprimeum([], _) :- nl, !.
imprimeum([V|C], Pos) :- escvariavel(V), /* Escreve um valor da lista. */
						Pos1 is Pos + 1, /* Incrementa um contador. */
						escespaco(Pos1), /* Quando o contador e' divisivel por 3 passa para a linha seguinte. */
						imprimeum(C, Pos1). 

/* Funcao: mov_legal */
/* Funcao que devolve movimentos possiveis numa configuracao. */
mov_legal(C1, M, P, C2) :- zero(C1, 0, Pos), /* Procura a possicao do zero. */
						mov_legal(C1, M, P, C2, Pos).
/* Movimento: cima */
mov_legal(C1, c, P, C4, Pos) :- Pos < 6, /* Quando o zero esta nas duas linhas de cima: */
						Pos1 is Pos + 3, /* A posicao do zero sera a abaixo dele. */
						peca(Pos1, P, C1), /* Procura a peca acima do zero. */
						mudanca(P,10,C1,C2), /* Coloca o valor 10 no lugar da peca. */
						mudanca(0,P,C2,C3), /* Coloca a peca no lugar do zero. */
						mudanca(10,0,C3,C4). /* Coloca o zero no lugar do 10. */
/* Movimento: baixo */
mov_legal(C1, b, P, C4, Pos) :- Pos > 2, /* Quando o zero esta nas duas linhas de baixo: */
						Pos1 is Pos - 3, /* A posicao do zero sera a acima dele. */
						peca(Pos1, P, C1), /* Procura a peca abaixo do zero. */
						mudanca(P,10,C1,C2), /* Coloca o valor 10 no lugar da peca. */
						mudanca(0,P,C2,C3), /* Coloca a peca no lugar do zero. */
						mudanca(10,0,C3,C4). /* Coloca o zero no lugar do 10. */
/* Movimento: esquerda */
mov_legal(C1, e, P, C4, Pos) :- Pos2 is Pos - 1, 
						Pos2 mod 3 =:= 0, /* Quando o zero esta na coluna central: */
						Pos1 is Pos + 1, /* A posicao do zero sera a 'a direita dele. */
						peca(Pos1, P, C1), /* Procura a peca 'a direita do zero. */
						mudanca(P,10,C1,C2), /* Coloca o valor 10 no lugar da peca. */
						mudanca(0,P,C2,C3), /* Coloca a peca no lugar do zero. */
						mudanca(10,0,C3,C4). /* Coloca o zero no lugar do 10. */
/* Movimento: esquerda */
mov_legal(C1, e, P, C4, Pos) :- Pos mod 3 =:= 0, /* Quando o zero esta na coluna da esquerda: */
						Pos1 is Pos + 1, /* A posicao do zero sera a 'a direita dele. */
						peca(Pos1, P, C1), /* Procura a peca 'a direita do zero. */ 
						mudanca(P,10,C1,C2), /* Coloca o valor 10 no lugar da peca. */
						mudanca(0,P,C2,C3), /* Coloca a peca no lugar do zero. */
						mudanca(10,0,C3,C4). /* Coloca o zero no lugar do 10. */
/* Movimento: direita */
mov_legal(C1, d, P, C4, Pos) :- Pos mod 3 =\= 0, /* Quando o zero esta na coluna central ou na da direita: */
						Pos1 is Pos - 1, /* A posicao do zero sera a 'a esquerda dele. */
						peca(Pos1, P, C1), /* Procura a peca 'a esquerda do zero. */
						mudanca(P,10,C1,C2), /* Coloca o valor 10 no lugar da peca. */
						mudanca(0,P,C2,C3), /* Coloca a peca no lugar do zero. */
						mudanca(10,0,C3,C4). /* Coloca o zero no lugar do 10. */

/* Funcao: escmovimento */
/* Funcao que escreve os movimentos a efetuar. */
escmovimento([]).
escmovimento([Mov|Movs]) :- write('mova a peca '), 
						escmovimento1(Mov),
						ultimo(Movs), /* Quando a lista estiver vazia, escreve um ponto final. */
						escmovimento(Movs).
escmovimento1([P|X]) :- X == [c], 
						write(P), 
						write(' para cima').
escmovimento1([P|X]) :- X == [b], 
						write(P), 
						write(' para baixo').
escmovimento1([P|X]) :- X == [e], 
						write(P), 
						write(' para a esquerda').
escmovimento1([P|X]) :- X == [d], 
						write(P), 
						write(' para a direita').

/* Funcao: calc */
/* Funcao que calculo o f de uma configuracao. */
calc(C1,C2,G,F,H) :- calc_h(C1,C2,H), /* Calcula o h. */
						F is H + G, !.						

/* Funcao: expande */
/* Funcao que expande uma configuracao. */
expande([],_, _, Ab,Ab).
expande([C,_,G,_,M], C2, Fe, Ab, Ab2) :- findall([C3,M2,P], mov_legal(C, M2, P, C3), L), /* Encontra todos os movimentos posiveis. */
						constroi_nos(L, C2, G, M, X), /*Constroi nos com os movimentos encontrados. */
						novos_nos(X, Y, Ab, Fe), /* Verifica se sao nos novos. */
						adicionar2(Y, Ab, Ab3), /* Adiciona as configuracoes novas 'a lista de abertos. */
						expande([],C2,Fe,Ab3,Ab2). /* Termina o processo. */


/*--------------------------------------------------------------------------------------------------------------*/
/*----------------------------------------------FUNCOES AUXILIARES----------------------------------------------*/
/*--------------------------------------------------------------------------------------------------------------*/


/* Funcao: escvariavelC */
/* Funcao que imprime os 3 primeiro valores de uma lista. */						
escvariavelC(_,3) :- !. /* Quando o contador e' igual a 3 termina. */
escvariavelC([V|C], Cont) :- escvariavel(V), /* Escreve um valor. */
						Cont1 is Cont + 1, /* Incrementa um contador. */
						escvariavelC(C, Cont1).

/* Funcoes: escvariavelC1, escvariavelC2 */
/* Funcoes que imprimem outros 3 valores de uma lista. */
escvariavelC1([_,_,_|C]) :- escvariavelC(C, 0), !.
escvariavelC2([_,_,_|C]) :- escvariavelC1(C), !.

/* Funcao: escespaco */
/* Funcao que muda de linha quando o argumento recebido e' divisivel por 3. */
escespaco(Pos) :- Pos mod 3 =\= 0, !.
escespaco(Pos) :- Pos mod 3 =:= 0, 
						nl.

/* Funcao: escvariavel */
/* Funcao que imprime valores de uma lista. */
escvariavel(0) :- write('   '). /* Se o valor da lista for zero, imprime 3 espaços. */
escvariavel(V) :- write(' '), write(V), write(' '). /* Se o valor nao for zero, imprime o valor com um espaço antes e depois. */

/* Funcao: zero */
/* Funcao que procura a posicao de um zero numa lista. */
zero([0|_], Pos, Pos) :- !. 
zero([_|C], Cont, Pos) :- Cont2 is Cont + 1, /* Incrementa a posicao ate encontrar o zero. */
						zero(C, Cont2, Pos).

/* Funcao: peca */
/* Funcao que devolve o elemento correspondente a uma posicao numa lista. */
peca(C, Peca, L) :- peca(Peca, L, 0, C).
peca(Peca, [Peca|_], Cont, Cont).
peca(Peca, [_|L], Cont, C) :- Cont1 is Cont + 1,
						peca(Peca, L, Cont1, C).

/* Funcao: mudanca */
/* Funcao que troca a posicao de dois elementos de uma lista. */
mudanca(_, _, [], []).
mudanca(A, B, [A|C1], [B|C2]) :- mudanca(A,B,C1,C2).
mudanca(A, B, [C|C1], [C|C2]) :- A =\= C, 
						mudanca(A,B,C1,C2).

/* Funcao: junta */
/* Funcao que junta duas listas. */
junta([],L,L).
junta([P|R],L1,[P|L2]) :- junta(R,L1,L2).

/* Funcao: inverte */
/* Funcao que inverte uma lista. */
inverte([],[]).
inverte([P|R],I) :- inverte(R,I1), 
						junta(I1,[P],I).

/* Funcao: ultimo */
/* Funcao que escreve um ponto final se receber uma lista vazia e muda de linha se a lista nao for vazia. */						
ultimo([]) :- writeln('.'),!.
ultimo(_) :- nl.

/* Funcao: adiciona */
/* Funcao que adiciona um elemento ao fim de uma lista. */
adicionar(A,L,[A|L]).

/* Funcao: retirar */
/* Funcao que remove um elemento de uma lista. */
retirar(A,[A|L],L).
retirar(A,[B|L1],[B|L2]) :- retirar(A,L1,L2).

/* Funcao: verificarjogada */
/* Funcao que verifica se um elemento esta numa lista. */
verificarjogada(Jogada,[Jogada|_]).
verificarjogada(Jogada,[_|L]) :- verificarjogada(Jogada,L).

/* Funcao: maiores */
/* Funcao que conta quantos elementos atras de um elemento sao maiores que esse elemento. */
maiores(_,[],Cont1,Cont1).
maiores(Elem,[A|L],Cont1,Cont) :- A > Elem, 
						Cont2 is Cont1 + 1, /* Incrementa o contador se o elemento for maior. */
						maiores(Elem,L,Cont2,Cont).
maiores(Elem,[_|L],Cont1,Cont) :- maiores(Elem,L,Cont1,Cont).


/* Funcao: contador */
/* Funcao que calcula a soma de todos os elementos da lista que sao superiores a eles, estando atrás deles (na lista). */
contador(C, Cont) :- contador(C, [], 0, Cont).
contador([], _, Cont, Cont).
contador([Elem|C], Ant, Cont1, Cont) :- Elem =\= 0, 
						maiores(Elem,Ant,0,Cont2), /* Conta os maiores para cada elemento. */
						Cont3 is Cont1 + Cont2, 
						adicionar(Elem,Ant,Ant2), /* Adiciona o elemento 'a lista dos anteriores. */
						contador(C,Ant2,Cont3,Cont).
contador([_|C], Ant, Cont1, Cont) :- contador(C, Ant, Cont1, Cont).

/* Funcao: calc_h */
/* Funcao que calcula o h para futuramente calcular o f. */			
calc_h(C1,C2,H) :- calc_h(C1,C2,0,H).
calc_h([],[],Cont,Cont).
calc_h([P1|R1],[P2|R2],Cont,H) :- P1 =\= P2, 
						P1 =\= 0, 
						Cont1 is Cont+1, /* Se os elementos das mesmas posicoes de duas listas nao forem iguais */
						      			 /* e o da primeira nao for zero, incrementa o contador. */
						calc_h(R1,R2,Cont1,H).
calc_h([_|R1],[_|R2],Cont,H) :- calc_h(R1,R2,Cont,H).

/* Funcao: no_menor */
/* Funcao que devolve o no com menor f. */	
no_menor(Ab, T) :- no_menor(Ab, _, T), !.
no_menor([_|[]], T, T).						
no_menor([A,B|Ab],T3, T) :- comparaf(A,B,T2), /* Compara o f de dois nos. */
						no_menor([T2|Ab],T2,T).


/* Funcao: comparaf */
/* Funcao que compara o f de dois nos, devolvendo o no com menor f. */	
comparaf(A, B, T) :- comparaf(A, B, _, T).
comparaf([],[],T,T).
comparaf([_,X,_,_,_],[E,Y,F,G,H],_,T) :- X > Y,
						comparaf([],[],[E,Y,F,G,H],T), !.
comparaf([A,X,B,C,D],[_,_,_,_,_],_,T) :- comparaf([],[],[A,X,B,C,D],T), !.

/* Funcao: adicionar2 */
/* Funcao que adiciona um no a uma lista. */	
adicionar2([],Ab,Ab).
adicionar2([A|L],Ab,Ab2) :-	adicionar2(L,[A|Ab],Ab2).

/* Funcao: verificat */
/* Funcao que verifica se a configuracao de um no esta numa lista. */	
verificat([C,_,_,_,_],[[C,_,_,_,_]|_]) :- !.
verificat([C,_,_,_,_],[_|L]) :- verificat([C,_,_,_,_],L).

/* Funcao: constroi_nos */
/* Funcao que cria novos nos. */	
constroi_nos(L, C2, G, M, X) :- constroi_nos(L, C2, G, M, [], X).
constroi_nos([], _, _, _, Nos, Nos).
constroi_nos([[C,M,P]|R], C2, G, Mov, Nos, X) :- G2 is G + 1, /* Incrementa o G. */
						calc(C, C2, G2, F, H), /* Calcula o f da configuracao do no. */
						constroi_nos(R, C2, G, Mov, [[C, F, G2, H, [[P,M]|Mov]]|Nos], X). 

/* Funcao: novos_nos */
/* Funcao que verifica se um no ja existe nalguma das listas, adicionando-o se ainda nao existir. */	
novos_nos(X, Y, Ab, Fe) :- novos_nos(X, [], Ab, Fe, Y).
novos_nos([],Y,_,_,Y).				
novos_nos([A|X],Y,Ab,Fe,Y2) :- \+ verificat(A,Ab), /* Verifica se esta numa lista. */
						\+ verificat(A,Fe), /* Verifica se esta numa outra lista. */
						adicionar(A,Y,Y1), !, /* Adiciona o no a uma lista. */
						novos_nos(X,Y1,Ab,Fe,Y2).
novos_nos([_|X],Y,Ab,Fe,Y2) :- novos_nos(X,Y,Ab,Fe,Y2).

/* Funcao: tiguais */
/* Funcao que verifica se uma configuracao de um no e' igual a uma outra configuracao. */	
tiguais([C,_,_,_,_],C).

/* Funcao: movimentos */
/* Funcao que devolve a lista movimentos de um no. */	
movimentos([_,_,_,_,M],M).
