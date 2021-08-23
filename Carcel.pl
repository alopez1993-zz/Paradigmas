% Práctica Cárcel. Paradigma Lógico
% ///////////////////////////////////////////////////////////////////

% guardia(Nombre)
guardia(bennett).
guardia(mendez).
guardia(george).

% prisionero(Nombre, Crimen)
prisionero(piper, narcotráfico([metanfetaminas])).
prisionero(alex, narcotráfico([heroína])).
prisionero(alex, homicidio(george)).
prisionero(red, homicidio(rusoMafioso)).
prisionero(suzanne, robo(450000)).
prisionero(suzanne, robo(250000)).
prisionero(suzanne, robo(2500)).
prisionero(dayanara, narcotráfico[heroína, opio])).
prisionero(dayanara, narcotráfico([metanfetaminas])).

%1) No es inversible para su primer parámetro, ya que recién intentará ligarlo
% dentro del not, el cual es un predicado no inversible.

% controla(Controlador, Controlado)
controla(piper, alex).
controla(bennett, dayanara).
controla(Guardia, Otro):- 
    prisionero(Otro,_), guardia(Guardia)
    not(controla(Otro, Guardia)).

% 2) conflictoDeIntereses/2: relaciona a dos personas distintas (ya sean guardias o prisioneros) si no se 
% controlan mutuamente y existe algún tercero al cual ambos controlan.

conflictoDeIntereses(Persona1, Persona2):-
    controla(Persona1, Persona3),
    controla(Persona2, Persona3),
    not(controla(Persona1, Persona2)),
    not(controla(Persona2, Persona1)).

% 3) peligroso/1: Se cumple para un preso que sólo cometió crímenes graves.
% Un robo nunca es grave.
% Un homicidio siempre es grave.
% Un delito de narcotráfico es grave cuando incluye al menos 5 drogas a la vez, o incluye metanfetaminas.
peligroso(Prisionero):-
    prisionero(Prisionero,_),
    forall(prisionero(Prisionero, Crimen), grave(Crimen)).

grave(homicidio(_)).
grave(narcotráfico(Drogas)):-
    length(Drogas, Cantidad), Cantidad >= 5.
grave(narcotráfico(Drogas)):-
    member(metanfetaminas, Drogas).


% 4) ladronDeGuanteBlanco/1: Aplica a un prisionero si sólo cometió robos y todos fueron por más de $100.000.
ladronDeGuanteBlanco(Prisionero):-
    prisionero(Prisionero,_),
    forall(prisionero(Prisionero, Crimen), roboBlanco(Crimen)).

roboBlanco(robo(Cantidad)):-
    robo(Cantidad), Cantidad > 100000.

%condena/2: Relaciona a un prisionero con la cantidad de años de condena que debe cumplir. 
%Esto se calcula como la suma de los años que le aporte cada crimen cometido, que se obtienen de 
%la siguiente forma:
%La cantidad de dinero robado dividido 10.000.
%7 años por cada homicidio cometido, más 2 años extra si la víctima era un guardia.
%2 años por cada droga que haya traficado.

condena(Prisionero, Anios):-
    prisionero(Prisionero, _),
    findall(CantidadRobada, prisionero(Prisionero, robo(CantidadRobada), CantidadesRobadas),
    sumlist(CantidadesRobadas, RobadaTotal).
    findall(7, (prisionero(Prisionero, homicidio(Victima), not(guardia(Victima))), Homicidios),
    findall(9, (prisionero(Prisionero, homicidio(Victima), guardia(Victima)), HomicidiosAGuardias)
    sumlist(Homicidios, HomicidiosTotal),
    sumlist(Homicidios, HomicidiosAGuardiasTotal),
    prisionero(Prisionero, narcotráfico(Drogas)),
    length(Drogas, DrogasTotal),
    Anios is (RobadaTotal/10000) + HomicidiosTotal + HomicidiosAGuardiasTotal + (DrogasTotal*2).

% capoDiTutiLiCapi/1: Se dice que un preso es el capo de todos los capos cuando nadie lo controla, pero todas 
% las personas de la cárcel (guardias o prisioneros) son controlados por él, o por alguien a quien él 
% controla (directa o indirectamente).

capoDiTutiLiCapi(Prisionero):-
    not(controla(Prisionero1, Prisionero)),
    forall(prisionero(Otro,_), (controla(Prisionero, Otro))),
    forall(not(controla(Prisionero, Otro)), (controla(Prisionero, Otro)))







