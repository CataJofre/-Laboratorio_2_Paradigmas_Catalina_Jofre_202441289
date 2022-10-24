:- module(tda_pixhex_20244128_JofreLeon,[pixhex/5, imageIsHexmap/1]).
/*-----------------------------------------------------TDA PIXHEX-------------------------------------------*/
% Este archivo corresponde al TDA pixhex, se encuentran todas los predicados necesarios para su creacion, 
% tambien los predicados organizados segun estructura.

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/
% El TDA PIXHEX representa la unidad inicial de una imagen tipo HEXMAP  cuyos colores se representan con un
%  unico valor escrito en forma hexadecimal, ademas de incluir la posicion de de cada pixel,representado
%  con las letras "x" e "y", tal como un plano cartesiano y la profundidad de estos.
% Representacion:
%  pixhex-d <- x (int) X y (int) X hex(String) X depth
/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*--------------------------------------PREDICADO PIXHEX ------------------------------------------------------*/
% Dominio: 3 numeros de tipo entero y un elemento de tipo string
% Recorrido: una lista que contiene los tres numeros del dominio y el string
% Descripcion: PREDICADO constructora del TDA pixhex, que guarda las posiciones, el hex y la profundidad

pixhex(X, Y, Hex, Profundidad, [X, Y, Hex, Profundidad]):-
    integer(X), integer(Y), string(Hex), integer(Profundidad),
    X >= 0, Y >= 0,  Profundidad >= 0.

/*-----------------------------------------------------PREDICADOS DE PERTENENCIA---------------------------*/
/*--------------------------------------PREDICADO IMAGE-IS-HEXMAP------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido: Boolean
% Descripcion: Si es una imagen del tipo hex xmap retorna true (muestra la imagen), sino retorna false

imageIsHexmap([_,_, [[_,_, Hex,_]|_]]) :-
   string(Hex)-> 
    writeln('#t');
    writeln('#f').
/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/
/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

