:- module(tda_image_20244128_JofreLeon,[image/4, imageFlipH/2, imageFlipV/2, imageCrop/6,imageRGBToHex/2,imageToHistogram/2, imageRotate90/2,imageCompress/2 ]).
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

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:
color_frecuente([], _, []).
color_frecuente([[_,_, ColorPixel,_]|Cola], ColorFrecuente, Resultado):- 
   ColorPixel\=ColorFrecuente->   
    color_frecuente(Cola, ColorFrecuente, Resultado). 
color_frecuente([[X, Y, ColorPixel, Profundidad]|Cola], ColorFrecuente, [[X, Y, ColorPixel, Profundidad]|Resultado]):- 
    color_frecuente(Cola, ColorFrecuente, Resultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

resto_de_colores([], _, []).
resto_de_colores([[_,_, ColorPixel,_]|Cola], ColorFrecuente, Resultado):- 
   ColorPixel==ColorFrecuente->   
   resto_de_colores(Cola, ColorFrecuente, Resultado). 
resto_de_colores([[X, Y, ColorPixel, Profundidad]|Cola], ColorFrecuente, [[X, Y, ColorPixel, Profundidad]|Resultado]):- 
    resto_de_colores(Cola, ColorFrecuente, Resultado).


/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

color_frecuente_rgb([], _, _,_,[]).
color_frecuente_rgb([[_,_,R,G,B,_]|Cola], R_frecuente, G_frecuente, B_frecuente, Resultado):- 
 R \=  R_frecuente, G \=  G_frecuente, B \=  B_frecuente  ->   
    color_frecuente_rgb(Cola, R_frecuente, G_frecuente, B_frecuente, Resultado). 
color_frecuente_rgb([[X, Y, R,G,B, Profundidad]|Cola], R_frecuente, G_frecuente, B_frecuente, [[X, Y, R,G,B, Profundidad]|Resultado]):- 
    color_frecuente_rgb(Cola, R_frecuente, G_frecuente, B_frecuente, Resultado).


/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

resto_de_colores_rgb([], _,_,_, []).
resto_de_colores_rgb([[_,_,R,G,B,_]|Cola], R_frecuente, G_frecuente, B_frecuente, Resultado):- 
   R ==  R_frecuente, G ==  G_frecuente, B ==  B_frecuente  ->   
   resto_de_colores_rgb(Cola,R_frecuente, G_frecuente, B_frecuente, Resultado). 
resto_de_colores_rgb([[X, Y, R,G,B, Profundidad]|Cola], R_frecuente, G_frecuente, B_frecuente, [[X, Y, R,G,B, Profundidad]|Resultado]):- 
    resto_de_colores_rgb(Cola, R_frecuente, G_frecuente, B_frecuente, Resultado).


/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

colores(Lista, Color, ColorFrecuente,RestoDeColores):-
    color_frecuente(Lista, Color, ColorFrecuente),
	resto_de_colores(Lista, Color, RestoDeColores).

colores_rgb(Lista, R,G,B, ColorFrecuente,RestoDeColores):-
    color_frecuente_rgb(Lista, R,G,B, ColorFrecuente),
	resto_de_colores_rgb(Lista, R,G,B, RestoDeColores).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:


color_histograma( [[Color,_]|_], Color).
color_histograma_rgb( [[[R,G,B],_]|_], R,G,B).

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:



/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

eliminar_pixel_rgb([], _, _,[]).
eliminar_pixel_rgb([[X,Y,_,_,_,_]|Cola], X2,Y2, Resultado):- 
   X == X2, Y == Y2 ->   
   eliminar_pixel_rgb(Cola,X2,Y2, Resultado). 
eliminar_pixel_rgb([[X,Y,R,G,B, Profundidad]|Cola], X2,Y2, [[X, Y, R,G,B, Profundidad]|Resultado]):- 
    eliminar_pixel_rgb(Cola, X2,Y2, Resultado).
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

eliminar_pixel([], _, _,[]).
eliminar_pixel([[X,Y,_,_]|Cola], X2,Y2, Resultado):- 
   X == X2, Y == Y2 ->   
   eliminar_pixel(Cola,X2,Y2, Resultado). 
eliminar_pixel([[X,Y,Color, Profundidad]|Cola], X2,Y2, [[X, Y, Color, Profundidad]|Resultado]):- 
    eliminar_pixel(Cola, X2,Y2, Resultado).
    
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

agregar_pixel_rgb(ListaPixeles, [X,Y,R,G,B, Profundidad], Image2):-
    eliminar_pixel_rgb(ListaPixeles, X,Y, Resultado),
    append(Resultado, [[X,Y,R,G,B, Profundidad]], Imag),
    ordenar_segun_x(Imag,Image2).
    /*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

agregar_pixel(ListaPixeles, [X,Y,Color, Profundidad], Image2):-
    eliminar_pixel_rgb(ListaPixeles, X,Y, Resultado),
    append(Resultado, [[X,Y,Color, Profundidad]], Imag),
    ordenar_segun_x(Imag,Image2).

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

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

largo_lista([],0).
largo_lista([_|Cola],Contador) :- 
    largo_lista(Cola,Contador1), 
   Contador is Contador1 + 1.

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
rotate_Aux(_,[], []).
rotate_Aux(Ancho,[[X,Y,Color,Profundidad]|Cola], [[X1,Y1,Color1,Profundidad1]|ColaResultado]):-
  	X1 is (Ancho - 1) - Y ,
    Y1 is X, 
    Color1 is Color,
    Profundidad1 is Profundidad,
   rotate_Aux(Ancho, Cola, ColaResultado).

 /*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:   
    
rotate_AuxRGB(_,[], []).
rotate_AuxRGB(Ancho,[[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,R1,G1,B1,Profundidad1]|ColaResultado]):-
  	X1 is (Ancho - 1) - Y ,
    Y1 is X, 
    R1 is R,
    G1 is G,
    B1 is B,
    Profundidad1 is Profundidad,
    rotate_AuxRGB(Ancho, Cola, ColaResultado).   
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

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:


imageRotate90(Image, I):-
    % si la imagen es de tipo hexmap se llama al predicado auxiliar rotate_AuxRGB
    imageIsPixmap(Image)->  
    	image( Ancho,Alto, PixelsIn, Image),
    	rotate_AuxRGB(Alto,PixelsIn, PixelsOut),
    	ordenar_segun_x(PixelsOut,PixelsOutOrdenado),
    	image(Ancho, Alto, PixelsOutOrdenado, I);
    % sino se llama al predicado rotate_Aux
    image(Ancho,Alto, PixelsIn, Image), 
    rotate_Aux(Alto,PixelsIn, PixelsOut), 
   	ordenar_segun_x(PixelsOut,PixelsOutOrdenado),
    image(Ancho, Alto, PixelsOutOrdenado, I).


/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:


imageCompress(Image, ImageComprimida):-
    imageIsPixmap(Image)->  
    	image(Alto,Ancho, ListaPixeles, Image),
    	imageToHistogram(Image, Histograma),
    	color_histograma_rgb( Histograma, R,G,B),
    	colores_rgb(ListaPixeles,R,G,B, _,PixelesComprimidos),
    	image(Alto,Ancho, PixelesComprimidos, ImageComprimida);
    image(Alto,Ancho, ListaPixeles, Image),
    imageToHistogram(Image, Histograma),
    color_histograma( Histograma, Color),
    colores(ListaPixeles,Color,_,PixelesComprimidos),
    image(Alto,Ancho, PixelesComprimidos, ImageComprimida).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

imageIsCompressed(Image):-
    image(Alto,Ancho, ListaPixeles,Image),
    Total is Alto * Ancho,
    largo_lista(ListaPixeles, Largo),
    Total == Largo ->  writeln('#f');
    writeln('#t').

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Recorrido:
% Descripcion:

imageChangePixel(Image, Pixel, ImagenModificada):-
    largo_lista(Pixel, Largo),
    Largo==6 ->  
		image(Alto,Ancho, ListaPixeles, Image),
		agregar_pixel_rgb(ListaPixeles, Pixel, ListaNueva),
		image(Alto,Ancho, ListaNueva, ImagenModificada);
    image(Alto,Ancho, ListaPixeles, Image),
    agregar_pixel(ListaPixeles, Pixel, ListaNueva),
    image(Alto,Ancho, ListaNueva, ImagenModificada).
    

