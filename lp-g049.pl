%______________PROJECTO DE LÓGICA PARA A PROGRAMAÇÃO_____________
%_____GRUPO 49   nº 62587 Vasco Alexandre da Silva Ramos ________
%----------------------------------------------------------------
%----------------------------------------------------------------
%--------------------------aproxima.pl---------------------------
%**********aproxima(localização 1, destino, partida)*************
%****Devolve 'true' caso a "localização 1" se aproxime do *******
%**** "destino" por qualquer coordenada tendo como origem a *****
%**** "partida". ************************************************
%----------------------------------------------------------------
%----------------------------------------------------------------

aproxima(LOC1, DEST, LOC2) :-
	gps(LOC1, P1x, P1y),
	gps(DEST, P2x, P2y),
	gps(LOC2, P3x, P3y),
	aproxima(abs(P1x), abs(P1y), abs(P2x), abs(P2y), abs(P3x), abs(P3y)).
	
aproxima(Xx, Xy, Yx, Yy, Zx, Zy) :-
	(abs(Yx - Xx) < abs(Yx - Zx)) -> true;
	(abs(Yy - Xy) < abs(Yy - Zy)) -> true.
	

%----------------------------------------------------------------
%----------------------------------------------------------------
%-------------------optimiza_itinerario.pl-----------------------
%*optimiza_itinerario(localização 1, localização 2, itinerario)**
%**** Tem 3 procedimentos auxiliares tendo em conta o que se ****
%*** quer optimizar. "optimiza_itinerario_d" para a distacia, ***
%**** "optimiza_itinerario_c" para os custos e ******************
%**** "optimiza_itinerario_t" para o tempo **********************
%----------------------------------------------------------------
%----------------------------------------------------------------

optimiza_itinerario_d(X, Y, ITN) :-
	optimiza_itinerario_d(X, Y, ITN, 0).	
optimiza_itinerario_d(X, Y, ITN, ACC) :-
	melhora_itinerario(X, Y, ITN, ACC + 5, -1, -1) -> 
	melhora_itinerario(X, Y, ITN, ACC + 5, -1, -1); 
	optimiza_itinerario_d(X, Y, ITN, ACC + 5).
	
optimiza_itinerario_t(X, Y, ITN) :-
	optimiza_itinerario_t(X, Y, ITN, 0).	
optimiza_itinerario_t(X, Y, ITN, ACC) :-
	melhora_itinerario(X, Y, ITN, -1, -1, ACC + 2) -> 
	melhora_itinerario(X, Y, ITN, -1, -1, ACC + 2); 
	optimiza_itinerario_t(X, Y, ITN, ACC + 2).
	
optimiza_itinerario_c(X, Y, ITN) :-
	optimiza_itinerario_c(X, Y, ITN, 0).	
optimiza_itinerario_c(X, Y, ITN, ACC) :-
	melhora_itinerario(X, Y, ITN, -1, ACC + 0.5, -1) -> 
	melhora_itinerario(X, Y, ITN, -1, ACC + 0.5, -1); 
	optimiza_itinerario_c(X, Y, ITN, ACC + 0.5).
	
	
%----------------------------------------------------------------
%----------------------------------------------------------------
%-------------------itinerario_optimo.pl-------------------------
%******** itinerario_optimo(loc 1, loc 2, itn, crit) ************
%***** Tem como procedimentos auxiliares os contidos em *********
%***** "optimiza_itinerario.pl" e chama-os de acordo com o ******
%***** critério apresentado. ************************************
%----------------------------------------------------------------
%----------------------------------------------------------------

itinerario_optimo(X, Y, ITN, CRIT) :-
	(CRIT == 'distancia') -> optimiza_itinerario_d(X, Y, ITN);
	(CRIT == 'custo') -> optimiza_itinerario_c(X, Y, ITN);
	(CRIT == 'tempo') -> optimiza_itinerario_t(X, Y, ITN).
	
%----------------------------------------------------------------
%----------------------------------------------------------------
%---------------------------melhora.pl---------------------------
%************** melhora(limite, valor a verificar) **************
%**** Tem 3 procedimentos auxiliares, de acordo, com o valor ****
%**** a melhorar. "melhora_d" para a distancia, "melhora_p"  ****
%**** para os custos e "melhora_t" para o tempo. Ele verifica ***
%**** se o nosso "valor a verificar" está dentro dos limites ****
%----------------------------------------------------------------
%----------------------------------------------------------------

melhora_d(LIM_D, D) :-
	((LIM_D \== -1) -> (D =< LIM_D) ; false).

melhora_p(LIM_P, P):-
	((LIM_P \== -1) -> (P =< LIM_P); false).

melhora_t(LIM_T, T)	:-
	((LIM_T \== -1) -> (T =< LIM_T); false).
	
%----------------------------------------------------------------
%----------------------------------------------------------------
%---------------------melhora_itinerario.pl----------------------
%** melhora_itinerario(loc 1, loc 2, itn, L_dist, L_por, L_tem) *
%**** Tem como procedimentos auxiliares os contidos em melhor ***
%** e o objectivo é apresentar um itinerario que esteja dentro **
%*** dos limites dados pelo utilizador. *************************
%----------------------------------------------------------------
%----------------------------------------------------------------

melhora_itinerario(X, Y, ITN, LIM_D, LIM_P, LIM_T) :-
	((LIM_D > 0), (LIM_P > 0), (LIM_T > 0)) -> (itinerario(X, Y, ITN, DIST, PORT, TEMPO), 
												melhora_d(LIM_D, DIST), 
												melhora_p(LIM_P, PORT), 
												melhora_t(LIM_T, TEMPO));
	((LIM_D > 0), (LIM_P > 0)) -> (itinerario(X, Y, ITN, DIST, PORT, TEMPO), 
									melhora_d(LIM_D, DIST), 
									melhora_p(LIM_P, PORT));
	((LIM_D > 0), (LIM_T > 0)) -> (itinerario(X, Y, ITN, DIST, PORT, TEMPO), 
									melhora_d(LIM_D, DIST), 
									melhora_t(LIM_T, TEMPO));
	((LIM_P > 0), (LIM_T > 0)) -> (itinerario(X, Y, ITN, DIST, PORT, TEMPO),
									melhora_p(LIM_P, PORT), 
									melhora_t(LIM_T, TEMPO));
	(LIM_D > 0) -> (itinerario(X, Y, ITN, DIST, PORT, TEMPO), 
						melhora_d(LIM_D, DIST));
	(LIM_P > 0) -> (itinerario(X, Y, ITN, DIST, PORT, TEMPO), 
						melhora_p(LIM_P, PORT));
	(LIM_T > 0) -> (itinerario(X, Y, ITN, DIST, PORT, TEMPO), 
						melhora_t(LIM_T, TEMPO));
	itinerario(X, Y, ITN, DIST, PORT, TEMPO).
	
%-----------------------------------------------------------------
%-----------------------------------------------------------------
%--------------------------itinerario.pl--------------------------
%******* itinerario(loc 1, loc 2 , itn, dist, port, tempo) *******
%**** Usa como auxiliares os ficheiros "nao_se_repete.pl" para ***
%* verificar repetições, o ficheiro "aproxima.pl" para verificar *
%** as aproximações e o ficheiro "nao_membro.pl" para garantir ***
%** que não existem elementos repetidos. *************************
%-----------------------------------------------------------------
%-----------------------------------------------------------------

itinerario(X, Y, LIST, DIST, PORT, TEMPO):- 
	itinerario(X, Y, LIST, [], DIST, 0, PORT, 0, TEMPO, 0).
itinerario(A, B, [A, B], LIST, DIST, DIST_ACC, PORT, PORT_ACC, TEMPO, TEMPO_ACC) :- 
	(liga(A, B,_ , D, V, PORT1) ; liga(B, A,_ , D, V, PORT1)),
	DIST is DIST_ACC + D,
	PORT is PORT_ACC + PORT1,
	TEMPO is ((TEMPO_ACC + (D / V)) * 60),
	nao_se_repete(B, LIST).
itinerario(X, Y, [X | [P | R]], LIST, DIST, DIST_ACC, PORT, PORT_ACC, TEMPO, TEMPO_ACC) :-
	(liga(X, P, _, D, V, PORT1) ; liga(P, X, _, D, V, PORT1)),  
	aproxima(P, Y, X),
	nao_membro(P, LIST),
	itinerario(P, Y, [P | R], [X | [P | LIST]], DIST, DIST_ACC + D, PORT, PORT_ACC + PORT1, TEMPO, TEMPO_ACC + (D / V)).
	
%-----------------------------------------------------------------
%-----------------------------------------------------------------
%--------------------------nao_membro.pl--------------------------
%******************* nao_membro(valor, lista) ********************
%***** Devolve 'true' se valor não pertence á lista e 'falso' ****
%***** caso contrário. *******************************************
%-----------------------------------------------------------------
%-----------------------------------------------------------------

nao_membro(_, []).
nao_membro(X, [P | R]) :- nao_membro(X, R), (X \== P).


%-------------------------------------------------------------------
%-------------------------------------------------------------------
%--------------------------nao_se_repete----------------------------
%****************** nao_se_repete(valor, lista) ********************
%********** Verifica se o "valor" não se repete na lista ***********
%-------------------------------------------------------------------
%-------------------------------------------------------------------

nao_se_repete(X, L) :- nao_se_repete(X, L , 0).
nao_se_repete(_, [], ACC) :- (ACC < 2).
nao_se_repete(X, [X | R], ACC) :- nao_se_repete(X, R, (ACC + 1)).
nao_se_repete(X, [Y | R], ACC) :- (X\==Y), nao_se_repete(X, R, ACC).

%-------------------------------------------------------------------