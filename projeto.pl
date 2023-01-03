% Yassir Yassin - ist1100611  
% --------------------------- 

% base de conhecimento
:- [dados],
   [keywords].


%      ____  __ 
%     |___ \/_ |
%       __) || |
%      |__ < | |
%      ___) || |
%     |____(_)_|
          

%   eventosSemSalas(EventosSemSala) e verdade se EventosSemSala e uma lista, ordenada e
%   sem elementos repetidos, de IDs de eventos sem sala;

eventosSemSalas(EventosSemSala) :-
    findall(ID,
            evento(ID, _, _, _, semSala),
            EventosSemSala_uns),
    sort(EventosSemSala_uns, EventosSemSala).


%  eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) e verdade se
%  EventosSemSala e uma lista, ordenada e sem elementos repetidos, de IDs de eventos sem sala
%  que decorrem em DiaDaSemana (doravante segunda-feira, terca-feira, quarta-feira,
%  quinta-feira, sexta-feira, sabado);

eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala) :-
    findall(ID,
          horario(ID,
                  DiaDaSemana,
                  _,
                  _,
                  _,
                  _),
            IDs),
    findall(ID, evento(ID, _, _, _, semSala),
        ID_semSala),
    intersection(IDs, ID_semSala, EventosSemSala_uns),
    sort(EventosSemSala_uns, EventosSemSala).

%  eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) e verdade se ListaPeriodos
%  e uma lista de periodos (pi,i_{1,2,3,4}) e EventosSemSala e uma lista, ordenada e sem elemen-
%  tos repetidos, de IDs de eventos sem sala nos periodos de ListaPeriodos.

eventosSemSalasPeriodo([], []).
eventosSemSalasPeriodo(ListaPeriodos, EventosSemSala) :-
    findall(ID,
            ( horario(ID,
                      _,
                      _,
                      _,
                      _,
                      Periodo),
              (   Periodo=p1_2,
                  (   member(p1, ListaPeriodos)
                  ;   member(p2, ListaPeriodos)
                  )
              ;   Periodo=p3_4,
                  (   member(p3, ListaPeriodos)
                  ;   member(p4, ListaPeriodos)
                  )
              ;   member(Periodo, ListaPeriodos)
              ),
              evento(ID, _, _, _, semSala)
            ),
            EventosSemSala_uns),
    sort(EventosSemSala_uns, EventosSemSala).



%    ____   ___  
%   |___ \ |__ \ 
%     __) |   ) |
%    |__ <   / / 
%    ___) | / /_ 
%   |____(_)____|
             
             

%  organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) e verdade se
%  EventosNoPeriodo e a lista, ordenada e sem elementos repetidos, de IDs dos eventos
%  de ListaEventos que ocorrem no periodo Periodo para pi,i_{1,2,3,4}.

organizaEventos([], _, []).
organizaEventos([Evento|Tail], Periodo, EventosNoPeriodo) :-
    horario(Evento, _, _, _, _, Horario),
    (   Periodo=p1, (Horario=p1; Horario=p1_2)
    ;   Periodo=p2, (Horario=p2; Horario=p1_2)
    ;   Periodo=p3, (Horario=p3; Horario=p3_4)
    ;   Periodo=p4, (Horario=p4; Horario=p3_4)
    ), !,
    organizaEventos(Tail, Periodo, EventosNoPeriodoRestantes),
    EventosNoPeriodo_Uns=[Evento|EventosNoPeriodoRestantes],
    sort(EventosNoPeriodo_Uns, EventosNoPeriodo).
organizaEventos([_|Tail], Periodo, EventosNoPeriodo) :-
    organizaEventos(Tail, Periodo, EventosNoPeriodo).



%  eventosMenoresQue(Duracao, ListaEventosMenoresQue) e verdade se
%  ListaEventosMenoresQue e a lista ordenada e sem elementos repetidos dos identifica-
%  dores dos eventos que tem duracao menor ou igual a Duracao.

eventosMenoresQue(Duracao, ListaEventosMenoresQue) :-
    findall(ID,
            ( horario(ID,
                      _,
                      _,
                      _,
                      DuracaoEvento,
                      _),
              DuracaoEvento=<Duracao
            ),
            EventosMenoresQue),
    sort(EventosMenoresQue, ListaEventosMenoresQue).

    
%  eventosMenoresQueBool(ID, Duracao) e verdade se o evento identificado por ID tiver dura-
%  cao igual ou menor a Duracao.

eventosMenoresQueBool(ID, Duracao) :-
    horario(ID,
            _,
            _,
            _,
            DuracaoEvento,
            _),
    DuracaoEvento=<Duracao.


%  procuraDisciplinas(Curso, ListaDisciplinas) e verdade se ListaDisciplinas e a lista
%  ordenada alfabeticamente do nome das disciplinas do curso Curso.

procuraDisciplinas(Curso, ListaDisciplinas) :-
    findall(NomeDisciplina,
            ( turno(ID, Curso, _, _),
              evento(ID, NomeDisciplina, _, _, _)
            ),
            Disciplinas),
    sort(Disciplinas, ListaDisciplinas).


%   organizaDisciplinas(ListaDisciplinas, Curso, Semestres) e verdade se Semestres
%   e uma lista com duas listas. A lista na primeira posicao contem as disciplinas de
%   ListaDisciplinas do curso Curso que ocorrem no primeiro semestre; idem para a lista na
%   segunda posicao, que contem as que ocorrem no segundo semestre. Ambas as listas devem
%   estar ordenadas alfabeticamente e nao devem ter elementos repetidos.

organizaDisciplinas(ListaDisciplinas, Curso, Semestres) :-
    organizaDisciplinas(ListaDisciplinas, Curso, [], [], Semestres).

organizaDisciplinas([], _, FirstSemester, SecondSemester, [SortedFirstSemester, SortedSecondSemester]) :-
    sort(FirstSemester, SortedFirstSemester),
    sort(SecondSemester, SortedSecondSemester).
organizaDisciplinas([Disciplina|Tail], Curso, FirstSemester, SecondSemester, Semestres) :-
    (evento(ID, Disciplina, _, _, _), turno(ID, Curso, _, _),
     horario(ID, _, _, _, _, Periodo),
     (member(Periodo, [p1, p2, p1_2]) ->
      organizaDisciplinas(Tail, Curso, [Disciplina|FirstSemester], SecondSemester, Semestres);
      (member(Periodo, [p3, p4, p3_4]) ->
       organizaDisciplinas(Tail, Curso, FirstSemester, [Disciplina|SecondSemester], Semestres)
      )
     )).



%  horasCurso(Periodo, Curso, Ano, TotalHoras) e verdade se TotalHoras for o numero
%  de horas total dos eventos associadas ao curso Curso, no ano Ano e periodo Periodo =
%  pi,i_{1,2,3,4}. Mais uma vez: nao esquecer as disciplinas semestrais.

horasCurso(Periodo, Curso, Ano, TotalHoras) :-
    (   Periodo=p1
        ;   Periodo=p2
        ),
        findall(ID,
                (   evento(ID, _, _, _, _),
                    turno(ID, Curso, Ano, _),
                    horario(ID, _, _, _, _, Periodo)
                ),
                ListaIDS),
        findall(ID,
                (   evento(ID, _, _, _, _),
                    turno(ID, Curso, Ano, _),
                    horario(ID, _, _, _, _, p1_2)
                ),
                ListaIDS2),
        append(ListaIDS, ListaIDS2, ListaIDS3),
        sort(ListaIDS3, IDsorted),
        findall(Horas,
                (   member(ID, IDsorted),
                    horario(ID, _, _, _, Horas, _)
                ),
                ListaHoras),
        sum_list(ListaHoras, TotalHoras).

horasCurso(Periodo, Curso, Ano, TotalHoras) :-
     (   Periodo=p3
        ;   Periodo=p4
        ),
        findall(ID,
            (  evento(ID, _, _, _, _),
                turno(ID, Curso, Ano, _),
                horario(ID, _, _, _, _, Periodo)
            ),
                ListaIDS),
        findall(ID,
            (  evento(ID, _, _, _, _),
               turno(ID, Curso, Ano, _),
               horario(ID, _, _, _, _, p3_4)
                ),
            ListaIDS2),
        append(ListaIDS, ListaIDS2, ListaIDS3),
        sort(ListaIDS3, IDsorted),
        findall(Horas,
            (   member(ID, IDsorted),
                horario(ID, _, _, _, Horas, _)
            ),
             ListaHoras),
        sum_list(ListaHoras, TotalHoras).



%evolucaoHorasCurso(Curso, Evolucao) e verdade se Evolucao for uma lista de tuplos na
%forma (Ano, Periodo, NumHoras), em que NumHoras e o total de horas associadas ao curso
%Curso, no ano Ano e periodo Periodo (pi,i_{1,2,3,4}). Evolucao devera estar ordenada por ano
%(crescente) e periodo.

evolucaoHorasCurso(Curso, Evolucao) :-
    findall(Ano, turno( _, _, Ano, _), Anos),
    sort(Anos, AnosOrdenados),
    evolucaoHorasCurso(Curso, AnosOrdenados, Evolucao).

evolucaoHorasCurso(_, [], []).
evolucaoHorasCurso(Curso, [Ano|RestoAnos], Evolucao) :-
    findall((Ano, Periodo, NumHoras),
            ( member(Periodo, [p1, p2, p3, p4]),
              horasCurso(Periodo, Curso, Ano, NumHoras)
            ),
            Tuplos),
    append(Tuplos, RestoEvolucao, Evolucao),
    evolucaoHorasCurso(Curso, RestoAnos, RestoEvolucao).


%        ____   ____  
%       |___ \ |___ \ 
%        __) |  __) |
%       |__ <  |__  <  
%       ___) | ___) |
%       |____(_)____/ 


%ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas)
%e verdade se Horas for o numero de horas sobrepostas (lembrar que 0.5 representa 30 minu-
%tos) entre o evento que tem inicio em HoraInicioEvento e fim em HoraFimEvento, e o slot
%que tem inicio em HoraInicioDada e fim em HoraFimDada. Se nao existirem sobreposicoes o
%predicado deve falhar (false).

ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraInicioDada < HoraFimEvento,
    HoraFimDada > HoraInicioEvento,
    !,
    ( HoraInicioDada >= HoraInicioEvento, HoraFimDada =< HoraFimEvento ->
      Horas is HoraFimDada - HoraInicioDada
    ; HoraInicioDada < HoraInicioEvento, HoraFimDada > HoraFimEvento ->
      Horas is HoraFimEvento - HoraInicioEvento
    ; HoraInicioDada < HoraInicioEvento, HoraFimDada =< HoraFimEvento ->
      Horas is HoraFimDada - HoraInicioEvento
    ; HoraInicioDada >= HoraInicioEvento, HoraFimDada > HoraFimEvento ->
      Horas is HoraFimEvento - HoraInicioDada
    ).


%numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras)
%e verdade se SomaHoras for o numero de horas ocupadas nas salas do tipo TipoSala, no
%intervalo de tempo definido entre HoraInicio e HoraFim, no dia da semana DiaSemana, e no
%periodo Periodo = pi,i_{1,2,3,4}. Nao te esquecas das disciplinas semestrais (p1_2 se Periodo for
%p1 ou p2, p3_4 se Periodo for 3 ou 4).

numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-
    (Periodo = p1 ; Periodo = p2),
    findall(Horas, (salas(TipoSala, ListaSalas), member(Sala, ListaSalas), evento(ID, _, _, _, Sala), (horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, Periodo) ; horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p1_2)), ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)), ListaHoras),!,
    sum_list(ListaHoras, SomaHoras).

numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-
    (Periodo = p3 ; Periodo = p4),
    findall(Horas, (salas(TipoSala, ListaSalas), member(Sala, ListaSalas), evento(ID, _, _, _, Sala), (horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, Periodo) ; horario(ID, DiaSemana, HoraInicioEvento, HoraFimEvento, _, p3_4)), ocupaSlot(HoraInicio, HoraFim, HoraInicioEvento, HoraFimEvento, Horas)), ListaHoras),!,
    sum_list(ListaHoras, SomaHoras).


%ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) e verdade se Max for o numero de ho-   
%ras possiveis de ser ocupadas por salas do tipo TipoSala (ver acima), no intervalo de tempo
%definido entre HoraInicio e HoraFim. Em termos praticos, assume-se que Max e o inter-
%valo tempo dado (HoraFim - HoraInicio), multiplicado pelo numero de salas em jogo do tipo
%TipoSala.

ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) :-
    salas(TipoSala, Salas),
    length(Salas, NumSalas),
    Max is (HoraFim - HoraInicio) * NumSalas.


%percentagem(SomaHoras, Max, Percentagem) e verdade se Percentagem for a divisao de
%SomaHoras por Max, multiplicada por 100.

percentagem(SomaHoras, Max, Percentagem) :-
    Percentagem is (SomaHoras / Max) * 100.



%ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) e verdade se
%Resultados for uma lista ordenada de tuplos do tipo casosCriticos(DiaSemana, TipoSala,
%Percentagem) em que DiaSemana, TipoSala e Percentagem sao, respectivamente, um dia
%da semana, um tipo de sala e a sua percentagem de ocupacao, no intervalo de tempo entre
%HoraInicio e HoraFim, e supondo que a percentagem de ocupacao relativa a esses elementos
%esta acima de um dado valor critico (Threshold).

ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) :-
    findall(casosCriticos(DiaSemana, TipoSala, PercentagemArredondada),
    ( horario(_, DiaSemana, _, _, _, Periodo),
    salas(TipoSala, _),
    numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras),
    ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max),
    percentagem(SomaHoras, Max, Percentagem),
    Percentagem > Threshold,
    ceiling(Percentagem, PercentagemArredondada)
    ), CasosCriticos),
    sort(CasosCriticos, Resultados).

        


%         ____  _  _   
%        |___ \| || |  
%          __) | || |_ 
%         |__ <|__   _|
%         ___) |  | |  
%        |____(_) |_| 

/*A mesa contem 8 lugares e e representado por [[X1,X2,X3],[X4,X5],[X6,X7,X8]], uma lista com tres listas,
em que a primeira contem as pessoas de um lado da mesa (X1, X2 e X3), a segunda as
pessoas a cabeceira (X4 e X5) e a terceira as pessoas do outro lado da mesa (X6, X7 e X8)*/

% Preenche a mesa com as pessoas da lista de pessoas
fill_table([X1,X2,X3,X4,X5,X6,X7,X8], [[X1,X2,X3],[X4,X5],[X6,X7,X8]]).

ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) :-
    permutation(ListaPessoas, Permutation),
    fill_table(Permutation, OcupacaoMesa),
    verificaRestricoes(OcupacaoMesa, ListaRestricoes).




% Verifica se a mesa satisfaz as restricoes
verificaRestricoes(OcupacaoMesa, ListaRestricoes) :-
    forall(member(Restricao, ListaRestricoes), verificaRestricao(OcupacaoMesa, Restricao)).

verificaRestricao([[_,_,_], [X,_], [_,_,_]], cab1(X)).
verificaRestricao([[_,_,_],  [_,X], [_,_,_]], cab2(X)).
verificaRestricao([[_,_,_], [X,_], [Y,_,_]], honra(X,Y)).
verificaRestricao([[_,_,Y], [_,X], [_,_,_]], honra(X,Y)).

verificaRestricao([[X,Y,_], [_,_], [_,_,_]], lado(X,Y)). 
verificaRestricao([[_,X,Y], [_,_], [_,_,_]], lado(X,Y)). 
verificaRestricao([[_,Y,X], [_,_], [_,_,_]], lado(X,Y)). 
verificaRestricao([[Y,X,_], [_,_], [_,_,_]], lado(X,Y)). 
verificaRestricao([[_,_,_], [X,Y], [_,_,_]], lado(X,Y)). 
verificaRestricao([[_,_,_], [Y,X], [_,_,_]], lado(X,Y)). 
verificaRestricao([[_,_,_], [_,_], [X,Y,_]], lado(X,Y)). 
verificaRestricao([[_,_,_], [_,_], [_,X,Y]], lado(X,Y)). 
verificaRestricao([[_,_,_], [_,_], [_,Y,X]], lado(X,Y)).
verificaRestricao([[_,_,_], [_,_], [Y,X,_]], lado(X,Y)).

verificaRestricao(OcupacaoMesa, naoLado(X,Y)) :-
    \+ verificaRestricao(OcupacaoMesa, lado(X,Y)).    

verificaRestricao([[X,_,_], [_,_], [Y,_,_]], frente(X,Y)).
verificaRestricao([[_,X,_], [_,_], [_,Y,_]], frente(X,Y)). 
verificaRestricao([[_,_,X], [_,_], [_,_,Y]], frente(X,Y)). 
verificaRestricao([[Y,_,_], [_,_], [X,_,_]], frente(X,Y)). 
verificaRestricao([[_,Y,_], [_,_], [_,X,_]], frente(X,Y)). 
verificaRestricao([[_,_,Y], [_,_], [_,_,X]], frente(X,Y)). 

verificaRestricao(OcupacaoMesa, naoFrente(X,Y)) :-
    \+ verificaRestricao(OcupacaoMesa, frente(X,Y)).









 