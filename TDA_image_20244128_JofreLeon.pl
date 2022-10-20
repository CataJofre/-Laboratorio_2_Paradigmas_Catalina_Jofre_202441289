/*-----------------------------------------------------TDA IMAGE-------------------------------------------*/
% Este archivo corresponde al TDA image, se encuentran todas los predicados necesarios para su creacion, 
% tambien los predicados organizados segun estructura.

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/

% Este TDA corresponde a la representacion de una imagen que se compone de diversas caracteristicas como el
% alto y ancho, ademas de tener diferentes tipos de pixeles (pixbit-d, pixrgb-d pixhex-d), lo que le da la
% posibilidad de reproducir diferentes aspectos de lo que es una imagen, usando los colores y la profundidad.

% Es una lista que contiene el alto ancho y una lista con la cantidad correspondiente de pixeles
/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*--------------------------------------FUNCION IMAGE ------------------------------------------------------*/
% Dominio: datos de tipo int y una lista
% Recorrido:una lista con la informacion
% Descripcion: Constructor del TDA image que toma un alto, ancho y una lista con pixeles

image(Ancho, Alto, Lista_pixeles, [Ancho, Alto, Lista_pixeles] ):-
    integer(Ancho), integer(Alto),
    Ancho >= 1, Alto >= 1.

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


