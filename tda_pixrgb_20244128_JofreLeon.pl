:- module(tda_pixrgb_20244128_JofreLeon,[pixrgb/7,imageIsPixmap/1,seleccionar_rgb/2]).
/*-----------------------------------------------------TDA PIXRGB-------------------------------------------*/
% Este archivo corresponde al TDA pixrgb, se encuentran todas los predicados necesarios para su creacion, 
% tambien los predicados organizados segun estructura.

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/
% El TDA PIXRGB representa la unidad inicial de una imagen de tipo PIXMAP expresada con pixeles que pueden
% tomar valores del espectro RGB (donde, Red=Rojo,Green=Verde,Blue=Azul) para representar su color, estos
% valores se mueven entre el 0 y el 255, ademas de incluir la posicion de de cada pixel, representado con
% las letras "x" e "y" y la profundidad de estos.
% Representacion:
% pixrgb-d <- x (int) X y (int) X red X green  X blue X depth

/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*-------------------------------------- PREDICADO PIXRGB------------------------------------------------------*/
% Dominio: 6 numeros de tipo entero
% Recorrido: una lista que contiene los seis numeros del dominio
% Descripcion:  PREDICADO constructora del TDA pixbit, que guarda las posiciones, los colores R,G y B, y la profundidad

pixrgb(X, Y, R,G,B, Profundidad, [X, Y, R,G,B, Profundidad]):-
     integer(X), integer(Y), integer(R), integer(G), integer(B), integer(Profundidad),
     X >= 0, Y >= 0, R >= 0, G >= 0, B >= 0, R =< 255, G =< 255, B =< 255,Profundidad >= 0.

/*-----------------------------------------------------PREDICADOS DE PERTENENCIA---------------------------*/
/*-------------------------------------- PREDICADO IMAGE-IS-PIXMAP------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido: Boolean
% Descripcion: Si es una imagen del tipo pixmap retorna true (muestra la imagen), sino retorna false

imageIsPixmap([_,_, [[_,_,R,G,B,_]|_]]) :-
    R >= 0, G >= 0, B >= 0, R =< 255, G =< 255, B =< 255 -> 
    writeln('#t');
    writeln('#f').
/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*-------------------------------------- PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion: guarda los colores en una lista 

seleccionar_rgb([],[]).    
seleccionar_rgb([[_,_,R,G,B,_]|Cola],[[R,G,B]|ColaResultado]):-
  	seleccionar_rgb(Cola,ColaResultado).
/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*-------------------------------------- PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/
/*-------------------------------------- PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

