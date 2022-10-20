/*-----------------------------------------------------TDA PIXBIT-------------------------------------------*/
% Este archivo corresponde al TDA pixbit, se encuentran todas los predicados necesarios para su creacion, 
% tambien los predicados organizados segun estructura.

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/
% El TDA PIXBIT representa la formacion de pixeles que pueden tomar valores entre 1 y 0,ademas de incluir la
% posicion de cada pixel, representado con las letras "x" e "y", tal como un plano cartesiano y la
% profundidad de estos, ademas de ser la unidad fundamental de una imagen llamada bitmap-d. 
% Representacion:
% pixbit-d <- x (int) X y (int) X bit ([0|1]) X depth (int))
/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*--------------------------------------FUNCION PIXBIT------------------------------------------------------*/
% Dominio: 4 numeros de tipo entero
% Recorrido: una lista que contiene los cuatro numeros del dominio
% Descripcion: Funcion constructora del TDA pixbit, que guarda las posiciones, el bit y la profundidad

pixbit(X, Y, Bit, Profundidad, [X, Y, Bit, Profundidad]):-
    integer(X), integer(Y), integer(Bit), integer(Profundidad),
    X >= 0, Y >= 0, Bit>= 0, Bit =< 1, Profundidad >= 0.

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

