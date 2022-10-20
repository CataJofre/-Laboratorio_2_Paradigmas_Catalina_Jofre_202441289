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
/*--------------------------------------FUNCION PIXHEX ------------------------------------------------------*/
% Dominio: 3 numeros de tipo entero y un elemento de tipo string
% Recorrido: una lista que contiene los tres numeros del dominio y el string
% Descripcion: Funcion constructora del TDA pixhex, que guarda las posiciones, el hex y la profundidad

pixhex(X, Y, Hex, Profundidad, [X, Y, Hex, Profundidad]):-
    integer(X), integer(Y), string(Hex), integer(Profundidad),
    X >= 0, Y >= 0,  Profundidad >= 0.

/*-----------------------------------------------------PREDICADOS DE PERTENENCIA---------------------------*/
/*--------------------------------------FUNCION ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*--------------------------------------FUNCION ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*--------------------------------------FUNCION ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/
/*--------------------------------------FUNCION ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

