:- module(tda_image_20244128_JofreLeon,[image/4]).
:- use_module(tda_pixbit_20244128_JofreLeon).
:- use_module(tda_pixhex_20244128_JofreLeon).
:- use_module(tda_pixrgb_20244128_JofreLeon).
/*-----------------------------------------------------tda IMAGE-------------------------------------------*/
% Este archivo corresponde al tda image, se encuentran todas los predicados necesarios para su creacion, 
% tambien los predicados organizados segun estructura.

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/

% Este tda corresponde a la representacion de una imagen que se compone de diversas caracteristicas como el
% alto y ancho, ademas de tener diferentes tipos de pixeles (pixbit-d, pixrgb-d pixhex-d), lo que le da la
% posibilidad de reproducir diferentes aspectos de lo que es una imagen, usando los colores y la profundidad.

% Es una lista que contiene el alto ancho y una lista con la cantidad correspondiente de pixeles
/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*--------------------------------------FUNCION IMAGE ------------------------------------------------------*/
% Dominio: datos de tipo int y una lista
% Recorrido:una lista con la informacion
% Descripcion: Constructor del tda image que toma un alto, ancho y una lista con pixeles

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
/*--------------------------------------FUNCION FLIPH_AUX------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al ancho y una lista de tamaño 4 con la informacion de los  pixeles
% Recorrido: lista con los pixeles ya modificados 
% Descripcion: Función auxiliar que permite invertir una imagen horizontalmente,cuando la imagen corresponde a un bitmap o hexmap.
% Tipo de Meta: Secundaria.

flipH_Aux(_,[], []).
flipH_Aux(Ancho,[[X,Y,Color,Profundidad]|Cola], [[X1,Y1,Color1,Profundidad1]|ColaResultado]):-
  	X1 is (Ancho - 1) - X ,
    Y1 is Y, 
    Color1 is Color,
    Profundidad1 is Profundidad,
    flipH_Aux(Ancho, Cola, ColaResultado).
    /*--------------------------------------FUNCION FLIPH_AUXRGB------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al alto y una lista de tamaño 7 con la informacion de los  pixeles
% Recorrido: lista con los pixeles ya modificados 
% Descripcion: Función auxiliar que permite invertir una imagen horizontalmente,cuando la imagen corresponde a un pixmap.
% Tipo de Meta: Secundaria.

flipH_AuxRGB(_,[], []).
flipH_AuxRGB(Ancho,[[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,R1,G1,B1,Profundidad1]|ColaResultado]):-
  	X1 is (Ancho - 1) - X ,
    Y1 is Y, 
    R1 is R,
    G1 is G,
    B1 is B,
    Profundidad1 is Profundidad,
    flipH_AuxRGB(Ancho, Cola, ColaResultado).

/*--------------------------------------FUNCION FLIPH------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido:Imagen (lista con el ancho, alto y los pixeles) pero modificada.
% Descripcion:  Función que permite invertir una imagen horizontalmente, cambiando el valor de la posicion de x, y dejando sin alterar la posicion de y. 
% Tipo de Meta: Primaria

imageFlipH(Image, I):-
    % si la imagen es de tipo hexmap se llama a la funcion auxiliar flipH_AuxRGB
    imageIsPixmap(Image)->  
    image(Ancho, Alto, PixelsIn, Image), flipH_AuxRGB(Ancho,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I);
    % sino se llama a la funcion flipH_Aux
    image(Ancho, Alto, PixelsIn, Image), flipH_Aux(Ancho,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I).
/*--------------------------------------FUNCION FLIPV_AUX------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al alto y una lista de tamaño 4 con la informacion de los  pixeles
% Recorrido: lista con los pixeles ya modificados 
% Descripcion: Función auxiliar que permite invertir una imagen verticalmente,cuando la imagen corresponde a un bitmap o hexmap.
% Tipo de Meta: Secundaria.

flipV_Aux(_,[], []).
flipV_Aux(Alto,[[X,Y,Color,Profundidad]|Cola], [[X1,Y1,Color1,Profundidad1]|ColaResultado]):-
  	X1 is  X ,
    Y1 is (Alto - 1) - Y, 
    Color1 is Color,
    Profundidad1 is Profundidad,
    flipV_Aux(Alto, Cola, ColaResultado).

/*--------------------------------------FUNCION FLIPV_AUXRGB------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al alto y una lista de tamaño 7 con la informacion de los  pixeles
% Recorrido: lista con los pixeles ya modificados 
% Descripcion: Función auxiliar que permite invertir una imagen verticalmente,cuando la imagen corresponde a un pixmap.
% Tipo de Meta: Secundaria.

flipV_AuxRGB(_,[], []).
flipV_AuxRGB(Alto,[[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,R1,G1,B1,Profundidad1]|ColaResultado]):-
  	X1 is (Alto - 1) - X ,
    Y1 is Y, 
    R1 is R,
    G1 is G,
    B1 is B,
    Profundidad1 is Profundidad,
    flipV_AuxRGB(Alto, Cola, ColaResultado).

/*--------------------------------------FUNCION FLIPV------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido:Imagen (lista con el ancho, alto y los pixeles) pero modificada.
% Descripcion:  Función que permite invertir una imagen verticalmente, cambiando el valor de la posicion de y, y dejando sin alterar la posicion de x. 
% Tipo de Meta: Primaria

imageFlipV(Image, I):-
    % si la imagen es de tipo hexmap se llama a la funcion auxiliar flipV_AuxRGB
    imageIsPixmap(Image)->  
    image( Ancho,Alto, PixelsIn, Image), flipV_AuxRGB(Alto,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I);
    % sino se llama a la funcion flipV_Aux
    image(Ancho,Alto, PixelsIn, Image), flipV_Aux(Alto,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I).