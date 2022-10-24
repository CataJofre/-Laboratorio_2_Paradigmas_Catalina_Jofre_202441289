:- module(tda_pixbit_20244128_JofreLeon,[pixbit/5, imageIsBitmap/1]).


/*-----------------------------------------------------REPRESENTACION--------------------------------------*/
% El TDA PIXBIT representa la formacion de pixeles que pueden tomar valores entre 1 y 0,ademas de incluir la
% posicion de cada pixel, representado con las letras "x" e "y", tal como un plano cartesiano y la
% profundidad de estos, ademas de ser la unidad fundamental de una imagen llamada bitmap-d. 
% Representacion:
% pixbit-d <- x (int) X y (int) X bit ([0|1]) X depth (int))
/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*--------------------------------------PREDICADO PIXBIT------------------------------------------------------*/
% Dominio: 4 numeros de tipo entero
% Recorrido: una lista que contiene los cuatro numeros del dominio
% Descripcion: PREDICADO constructora del TDA pixbit, que guarda las posiciones, el bit y la profundidad

pixbit(X, Y, Bit, Profundidad, [X, Y, Bit, Profundidad]):-
    integer(X), integer(Y), integer(Bit), integer(Profundidad),
    X >= 0, Y >= 0, Bit>= 0, Bit =< 1, Profundidad >= 0.

/*-----------------------------------------------------PREDICADOS DE PERTENENCIA---------------------------*/
/*--------------------------------------PREDICADO IMAGE-IS-BITMAP------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido: Boolean
% Descripcion: Si es una imagen del tipo bitxmap retorna true (muestra la imagen), sino retorna false

imageIsBitmap([_,_, [[_,_, Bit,_]|_]]) :-
    Bit >= 0, Bit < 2-> 
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

