:- module(tda_image_20244128_JofreLeon,[image/4, imageFlipH/2, imageFlipV/2, imageCrop/6,imageRGBToHex/2,imageToHistogram/2 ]).
:- use_module(tda_pixbit_20244128_JofreLeon).
:- use_module(tda_pixhex_20244128_JofreLeon).
:- use_module(tda_pixrgb_20244128_JofreLeon).
/*-----------------------------------------------------TDA IMAGE-------------------------------------------*/
% Este archivo corresponde al tda image, se encuentran todas los predicados necesarios para su creacion, 
% tambien los predicados organizados segun estructura.

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/

% Este tda corresponde a la representacion de una imagen que se compone de diversas caracteristicas como el
% alto y ancho, ademas de tener diferentes tipos de pixeles (pixbit-d, pixrgb-d pixhex-d), lo que le da la
% posibilidad de reproducir diferentes aspectos de lo que es una imagen, usando los colores y la profundidad.

% Es una lista que contiene el alto ancho y una lista con la cantidad correspondiente de pixeles
/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*-------------------------------------PREDICADO IMAGE ------------------------------------------------------*/
% Dominio: datos de tipo int y una lista
% Recorrido:una lista con la informacion
% Descripcion: Constructor del tda image que toma un alto, ancho y una lista con pixeles

image(Ancho, Alto, Lista_pixeles, [Ancho, Alto, Lista_pixeles] ):-
    integer(Ancho), integer(Alto),
    Ancho >= 1, Alto >= 1.

/*-----------------------------------------------------PREDICADOS DE PERTENENCIA---------------------------*/
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

seleccionar_pixeles([Cabeza|_],1,1,[Cabeza]).
seleccionar_pixeles([Cabeza|Cola],1,Fin,[Cabeza|ColaResultado]) :- 
	Fin > 1, 
    Fin1 is Fin - 1, 
    seleccionar_pixeles(Cola,1,Fin1,ColaResultado).
seleccionar_pixeles([_|Cola],Inicio,Fin,ColaResultado) :- 
	Inicio > 1, 
    Inicio1 is Inicio - 1, Fin1 is Fin - 1, 
  seleccionar_pixeles(Cola,Inicio1,Fin1,ColaResultado).

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

ordenar_segun_y(Lista_pixeles, Pixeles_ordenados):-
    sort(2, @=<, Lista_pixeles, Pixeles_ordenados).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

ordenar_segun_x(Lista_pixeles, Pixeles_ordenados):-
    sort(1, @=<, Lista_pixeles, Pixeles_ordenados).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

ordenar_lista(Lista, ListaOrdenada):-
    sort(0, @=<, Lista,ListaOrdenada).

/*-------------------------------------PREDICADO FLIPH_AUX------------------------------------------------------*/
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
    /*-------------------------------------PREDICADO FLIPH_AUXRGB------------------------------------------------------*/
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

/*-------------------------------------PREDICADO FLIPV_AUX------------------------------------------------------*/
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

/*-------------------------------------PREDICADO FLIPV_AUXRGB------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al alto y una lista de tamaño 7 con la informacion de los  pixeles
% Recorrido: lista con los pixeles ya modificados 
% Descripcion: Función auxiliar que permite invertir una imagen verticalmente,cuando la imagen corresponde a un pixmap.
% Tipo de Meta: Secundaria.

flipV_AuxRGB(_,[], []).
flipV_AuxRGB(Alto,[[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,R1,G1,B1,Profundidad1]|ColaResultado]):-
  	X1 is X ,
    Y1 is (Alto - 1) - Y, 
    R1 is R,
    G1 is G,
    B1 is B,
    Profundidad1 is Profundidad,
    flipV_AuxRGB(Alto, Cola, ColaResultado).
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados ):-
	seleccionar_pixeles(ListaPixeles, Xa , Xb , Pixeles_SeleccionadosX),
	ordenar_segun_y(Pixeles_SeleccionadosX, Pixeles_ordenados),
    seleccionar_pixeles( Pixeles_ordenados, Ya ,  Yb , Pixeles_SeleccionadosY),
    ordenar_segun_x(Pixeles_SeleccionadosY, PixelesSeleccionados ).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

cropAux(ListaPixeles, X1,Y1, X2,Y2,Alto, PixelesSeleccionados ):-
    % en casos especificos hay que realizar cambios para que la funcion funcione correctamente
    X1 = 0, Y1=0, X2=0, Y2=0, 
    	Xa is X1 + 1 , Ya is Y1 + 1, Xb is X2 + 1, Yb is Y2 +1,crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados);
	X1 = 0, Y1=0, X2=0, Y2=1, 
    	Xa is X1 + 1 , Ya is Y1 + 1, Xb is X2 + 2, Yb is Y2 +1,crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados);
    X1 = 0, Y1=1, X2=1, Y2=1, 
    	Xa is X1 + 1 , Ya is Y1 + 2, Xb is X2 + 3, Yb is Y2 +3, crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados);
    X1 = 0, Y1=0, X2=1, Y2=1, 
    	Xa is X1 + 1 , Ya is Y1 + 1, Xb is X2 + 3, Yb is Y2 +3, crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados);
    % cuando no se cumplen los casos se retorna la llamada de la funcion auxiliar sin cambios
	Xa is X1 * Alto + 1 , Ya is Y1 *Y2 + 1,
    	Xb is (X2 + 1)* Alto, Yb is Y2  * Alto, crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

contar(Elemento,[],[],1,[Elemento,1]).
contar(Elemento,[],[],Cantidad,[Elemento,Cantidad]) :- 
    Cantidad >= 1.
contar(Elemento,[ElementoSiguiente|ListaResultante],[ElementoSiguiente|ListaResultante],1,[Elemento,1]) :- 
    Elemento \= ElementoSiguiente.
contar(Elemento,[ElementoSiguiente|ListaResultante],[ElementoSiguiente|ListaResultante],Cantidad,[Elemento,Cantidad]) :- 
    Cantidad >= 1, Elemento \= ElementoSiguiente.
contar(Elemento,[Elemento|Cola],ListaResultante,Contador,Resultado) :- 
    Contador1 is Contador + 1, 
    contar(Elemento,Cola,ListaResultante,Contador1,Resultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

contar_elemento([],[]).
contar_elemento([Elemento|Cola],[ElementoSalida|ColaResultado]) :- 
    contar(Elemento,Cola,ListaResultante,1,ElementoSalida), 
    contar_elemento(ListaResultante,ColaResultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:


/*-------------------------------------PREDICADO FLIPH------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido:Imagen (lista con el ancho, alto y los pixeles) pero modificada.
% Descripcion:  Función que permite invertir una imagen horizontalmente, cambiando el valor de la posicion de x, y dejando sin alterar la posicion de y. 
% Tipo de Meta: Primaria

imageFlipH(Image, I):-
    % si la imagen es de tipo hexmap se llama a laPREDICADO auxiliar flipH_AuxRGB
    imageIsPixmap(Image)->  
    image(Ancho, Alto, PixelsIn, Image), flipH_AuxRGB(Ancho,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I);
    % sino se llama a laPREDICADO flipH_Aux
    image(Ancho, Alto, PixelsIn, Image), flipH_Aux(Ancho,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I).


/*------------------------------------- PREDICADO FLIPV------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido:Imagen (lista con el ancho, alto y los pixeles) pero modificada.
% Descripcion:  Función que permite invertir una imagen verticalmente, cambiando el valor de la posicion de y, y dejando sin alterar la posicion de x. 
% Tipo de Meta: Primaria

imageFlipV(Image, I):-
    % si la imagen es de tipo hexmap se llama a laPREDICADO auxiliar flipV_AuxRGB
    imageIsPixmap(Image)->  
    image( Ancho,Alto, PixelsIn, Image), flipV_AuxRGB(Alto,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I);
    % sino se llama a laPREDICADO flipV_Aux
    image(Ancho,Alto, PixelsIn, Image), flipV_Aux(Alto,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

imageCrop(Image,X1,Y1, X2,Y2, Image2):-
    image(Ancho,Alto, ListaPixeles, Image),
    cropAux(ListaPixeles, X1,Y1, X2,Y2,Alto, PixelesSeleccionados),
    image(Ancho,Alto, PixelesSeleccionados, Image2).


/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:


imageRGBToHex(Image, Image2):-
    image(Ancho,Alto, ListaPixeles, Image),
  	rgb_hex(ListaPixeles, PixelesAHex),
    image(Ancho,Alto, PixelesAHex, Image2).
    

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:


imageToHistogram(Image, Histograma):-
    imageIsHexmap(Image)->  
		image(_,_, ListaPixeles, Image),
        seleccionar_hex(ListaPixeles, PixelesHex),
        ordenar_lista(PixelesHex,PixelesHexOrdenados),
        contar_elemento(PixelesHexOrdenados, Histograma);
    imageIsPixmap(Image)->  
		image(_,_, ListaPixeles, Image),
        seleccionar_rgb(ListaPixeles, PixelesRGB),
        ordenar_lista(PixelesRGB,PixelesRGBOrdenados),
        contar_elemento(PixelesRGBOrdenados, Histograma);
    imageIsBitmap(Image)->  
		image(_,_, ListaPixeles, Image),
        seleccionar_bit(ListaPixeles, PixelesBit),
        ordenar_lista(PixelesBit,PixelesBitOrdenados),
        contar_elemento(PixelesBitOrdenados, Histograma).