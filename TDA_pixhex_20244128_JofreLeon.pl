:- module(tda_pixhex_20244128_JofreLeon,[pixhex/5, imageIsHexmap/1, rgb_hex/2]).
/*-----------------------------------------------------TDA PIXHEX-------------------------------------------*/
% Este archivo corresponde al TDA pixhex, se encuentran todas los predicados necesarios para su creacion, 
% tambien los predicados organizados segun estructura.

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/
% El TDA PIXHEX representa la unidad inicial de una imagen tipo HEXMAP  cuyos colores se representan con un
%  unico valor escrito en forma hexadecimal, ademas de incluir la posicion de de cada pixel,representado
%  con las letras "x" e "y", tal como un plano cartesiano y la profundidad de estos.
% Representacion:
%  pixhex-d <- x (int) X y (int) X hex(String) X depth

% color(Numero de color RGB, HEX)
color(0.0,"0").
color(1.0,"1").
color(2.0,"2").
color(3.0,"3").
color(4.0,"4").
color(5.0,"5").
color(6.0,"6").
color(7.0,"7").
color(8.0,"8").
color(9.0,"9").
color(10.0,"A").
color(11.0,"B").
color(12.0,"C").
color(13.0,"D").
color(14.0,"E").
color(15.0,"F").

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

parte_decimal(Dividendo, Resultado):-
 	A is Dividendo/16,
   floor(A, B),
    Resultado is (A- B) *16 .

/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

parte_entera(Dividendo, Resultado):-
    floor((Dividendo/16), Res),
    Resultado is Res * 1.0.

/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

obtener_hex_por_decimal(Numero, Hex):-
   parte_decimal(Numero,R),
	color(R,Hex).


/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

obtener_hex_por_entero(Numero, Hex):-
   parte_entera(Numero,R),
	color(R,Hex).


/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:


hex(Numero,Hex):-
    obtener_hex_por_entero(Numero, H2),
    obtener_hex_por_decimal(Numero, H1), 
    string_concat(H2, H1, Hex).

/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:


pixRGB_a_pixHex( [_,_,R,G,B,_], PixelHex):-
    hex(B, HexB),
    hex(G, HexG),
    string_concat(HexG,HexB, Res),
    hex(R, HexR),
    string_concat(HexR,Res, PixelHex).
   
/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

rgb_hex([], []).
rgb_hex([[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,Hex,Profundidad1]|ColaResultado]):-
  	X1 is X ,
    Y1 is Y, 
    pixRGB_a_pixHex( [_,_,R,G,B,_], Hex),
    Profundidad1 is Profundidad,
    rgb_hex(Cola, ColaResultado).

/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:
