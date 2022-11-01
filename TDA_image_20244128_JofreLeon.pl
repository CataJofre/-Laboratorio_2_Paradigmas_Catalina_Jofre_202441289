% Predicado para exportar los predicados al script de pruebas.
:- module(tda_image_20244128_JofreLeon,[image/4, imageFlipH/2, imageFlipV/2, imageCrop/6,imageRGBToHex/2,imageToHistogram/2, imageRotate90/2,imageCompress/3,imageChangePixel/3,imageToString/2,
                                        imageDepthLayers/2,imageDecompress/3 ]).
% Predicados para importar los predicados de los otros archivos.
:- use_module(tda_pixbit_20244128_JofreLeon).
:- use_module(tda_pixhex_20244128_JofreLeon).
:- use_module(tda_pixrgb_20244128_JofreLeon).


/*-----------------------------------------------------TDA IMAGE-------------------------------------------
Este archivo corresponde al TDA image, se encuentran todos los predicados necesarios para su creacion y tambien
los solicitados en el proyecto de laboratorio.
Por motivos de organizacion se realizo la division del archivo en dos partes principales, la primera corresponde
a aquellos predicados cuyas metas son secundarias por lo que aportan en el funcionamiento de los predicados solicitados
y la segunda donde se encuentran todos los predicados del laboratorio

PRIMERA PARTE:  PREDICADOS AUXILIARES O PREDICADOS CON FUNCIONES ESPECIFICAS                    */

% INFORMACION IMPORTANTE.

% Dominios:
    % TDA pixrgb, TDA pixrbit,TDA pixhex
    % Posiciones X,Y
    % Lista de pixeles.
    % Color
    % Alto
    % Ancho
    % Imagen

% Predicados:
	% seleccionar_pixeles(ListaPixeles,Inicio,Fin,Fin,ListaPixelesSeleccionados).
	% color_frecuente(ListaPixeles, ColorFrecuente, ListaPixelesColorFrecuente).
	% resto_de_colores(ListaPixeles, ColorFrecuente, ListaPixelesDeColoresNoFrecuentes).
	% colores(ListaPixeles, Color, ColorFrecuente,RestoDeColores)
	% colores_rgb(ListaPixeles, R,G,B, ColorFrecuente,RestoDeColores)
	% color_histograma( Histograma, Color).
	% color_histograma_rgb(Histograma, R,G,B).
	% eliminar_pixel(ListaPixeles, X, Y,ListaPixelesConUnPixelMenos).
	% agregar_pixel(ListaPixeles, Pixel, ListaPixelesConUnoNuevo).
	% ordenar_segun_y(Lista_pixeles, Pixeles_ordenados).
	% ordenar_segun_x(ListaPixeles, Pixeles_ordenados).
	% ordenar_lista(Lista, ListaOrdenada).
	% largo_lista(Lista,Cantidad).
	% flipH_Aux(Alto,ListaPixeles, ListaPixelesInvertidos).
	% flipV_Aux(Alto,ListaPixeles, ListaPixelesInvertidos).
	% crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados ).
	% cropAux(ListaPixeles, X1,Y1, X2,Y2,Alto, PixelesSeleccionados).
	% contar_color(ListaPixeles,ListaConColoresYCantidad).
	% rotate_Aux(Alto,ListaPixeles, ListaPixelesRotados).
	% insertarAnchoAlto(Ancho, Alto,ListaDeListaPixeles,ListaDeListaPixelesConAnchoYAlto).
	% image(Ancho, Alto, ListaPixeles, Image ).
	% imageIsCompressed(Image).
	% imageFlipH(Image, ImagenInvertida).
	% imageFlipV(Image, ImagenInvertida).
	% imageCrop(Image,X1,Y1, X2,Y2, ImageCortada).
	% imageRGBToHex(ImageRGB, ImageHEX).
	% imageToHistogram(Image, Histograma).
	% imageRotate90(Image, ImagenRotada).
	% imageCompress(Image, ImageComprimida,Z).
	% imageChangePixel(Image, Pixel, ImagenModificada).
	% imageToString(Image, ImageEnString).
	% imageDepthLayers(Image, ImageEnCapas).
	% imageDecompress(ImageComprimida, PixelesEliminados , ImagenDescomprimida).

% Metas:
% Primarias
    % image(Ancho, Alto, ListaPixeles, Image ).
	% imageIsCompressed(Image).
	% imageFlipH(Image, ImagenInvertida).
	% imageFlipV(Image, ImagenInvertida).
	% imageCrop(Image,X1,Y1, X2,Y2, ImageCortada).
	% imageRGBToHex(ImageRGB, ImageHEX).
	% imageToHistogram(Image, Histograma).
	% imageRotate90(Image, ImagenRotada).
	% imageCompress(Image, ImageComprimida,Z).
	% imageChangePixel(Image, Pixel, ImagenModificada).
	% imageToString(Image, ImageEnString).
	% imageDepthLayers(Image, ImageEnCapas).
	% imageDecompress(ImageComprimida, PixelesEliminados , ImagenDescomprimida).

% Secundarias
	% seleccionar_pixeles(ListaPixeles,Inicio,Fin,Fin,ListaPixelesSeleccionados).
	% color_frecuente(ListaPixeles, ColorFrecuente, ListaPixelesColorFrecuente).
	% resto_de_colores(ListaPixeles, ColorFrecuente, ListaPixelesDeColoresNoFrecuentes).
	% colores(ListaPixeles, Color, ColorFrecuente,RestoDeColores)
	% colores_rgb(ListaPixeles, R,G,B, ColorFrecuente,RestoDeColores)
	% color_histograma( Histograma, Color).
	% color_histograma_rgb(Histograma, R,G,B).
	% eliminar_pixel(ListaPixeles, X, Y,ListaPixelesConUnPixelMenos).
	% agregar_pixel(ListaPixeles, Pixel, ListaPixelesConUnoNuevo).
	% ordenar_segun_y(Lista_pixeles, Pixeles_ordenados).
	% ordenar_segun_x(ListaPixeles, Pixeles_ordenados).
	% ordenar_lista(Lista, ListaOrdenada).
	% largo_lista(Lista,Cantidad).
	% flipH_Aux(Alto,ListaPixeles, ListaPixelesInvertidos).
	% flipV_Aux(Alto,ListaPixeles, ListaPixelesInvertidos).
	% crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados ).
	% cropAux(ListaPixeles, X1,Y1, X2,Y2,Alto, PixelesSeleccionados).
	% contar_color(ListaPixeles,ListaConColoresYCantidad).
	% rotate_Aux(Alto,ListaPixeles, ListaPixelesRotados).
	% insertarAnchoAlto(Ancho, Alto,ListaDeListaPixeles,ListaDeListaPixelesConAnchoYAlto).

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/

% Este tda corresponde a la representacion de una imagen que se compone de diversas caracteristicas como el
% alto y ancho, ademas de tener diferentes tipos de pixeles (pixbit-d, pixrgb-d pixhex-d), lo que le da la
% posibilidad de reproducir diferentes aspectos de lo que es una imagen, usando los colores y la profundidad.

% Es una lista que contiene el alto ancho y una lista con la cantidad correspondiente de pixeles

/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*-------------------------------------PREDICADO SELECCIONAR-PIXELES------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Toma una lista de pixeles y toma una porcion de estos desde un rango definido. Utilizado en imageCrop.

% Caso base cuando se selecciona solo un elemento.
seleccionar_pixeles([Cabeza|_],1,1,[Cabeza]).
seleccionar_pixeles([Cabeza|Cola],1,Fin,[Cabeza|ColaResultado]) :- 
    % Se toman los elementos desde el primero hasta el decidido
	Fin > 1, 
    Fin1 is Fin - 1, 
    % Llamada recursiva.
    seleccionar_pixeles(Cola,1,Fin1,ColaResultado).
seleccionar_pixeles([_|Cola],Inicio,Fin,ColaResultado) :- 
    % Se guardan los elementos que se encuentran entre el intervalo inicio-fin.
	Inicio > 1, 
    Inicio1 is Inicio - 1, 
    Fin1 is Fin - 1, 
    % Llamada recursiva.
    seleccionar_pixeles(Cola,Inicio1,Fin1,ColaResultado).

/*-------------------------------------PREDICADO COLOR-FRECUENTE------------------------------------------------------*/
% Dominio:Lista de pixeles
% Descripcion: Con la informacion del histograma guarda los pixeles que contienen el color mas frecuente en una lista. Utilizado en imageCompress.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
color_frecuente([], _, []).
color_frecuente([[_,_, ColorPixel,_]|Cola], ColorFrecuente, Resultado):- 
% Se comparan los colores ingresados con los colores de los pixeles.
    ColorPixel\=ColorFrecuente->   
    % Llamada recursiva.
    color_frecuente(Cola, ColorFrecuente, Resultado). 
% Se guardan los pixeles cuyos colores coinciden con lo ingresado.   
color_frecuente([[X, Y, ColorPixel, Profundidad]|Cola], ColorFrecuente, [[X, Y, ColorPixel, Profundidad]|Resultado]):-
    % Llamada recursiva.
    color_frecuente(Cola, ColorFrecuente, Resultado).

/*-------------------------------------PREDICADO RESTO-DE-COLORES------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Con la informacion del histograma guarda aquellos pixeles que tienen los otros colores no frecuentes en una lista. Utilizado en imageCompress.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
resto_de_colores([], _, []).
resto_de_colores([[_,_, ColorPixel,_]|Cola], ColorFrecuente, Resultado):- 
    % Se comparan los colores ingresados con los colores de los pixeles.
    ColorPixel==ColorFrecuente->   
    % Llamada recursiva.
    resto_de_colores(Cola, ColorFrecuente, Resultado). 
% Se guardan los pixeles cuyos colores no coinciden con lo ingresado.
resto_de_colores([[X, Y, ColorPixel, Profundidad]|Cola], ColorFrecuente, [[X, Y, ColorPixel, Profundidad]|Resultado]):- 
    % Llamada recursiva.
    resto_de_colores(Cola, ColorFrecuente, Resultado).

/*-------------------------------------PREDICADO COLORES------------------------------------------------------*/
% Dominio: Lista de pixeles y el color mas frecuente
% Descripcion: Realiza la separacion de una lista de pixeles entre los que tienen el color mas frecuente y los que no.
% Utilizado en imageCompress.

colores(Lista, Color, ColorFrecuente,RestoDeColores):-
    % Obtiene los pixeles con el color frecuente.
    color_frecuente(Lista, Color, ColorFrecuente),
    % genera una lista con el resto de los colores de la lista.
	resto_de_colores(Lista, Color, RestoDeColores).

colores_rgb(Lista, R,G,B, ColorFrecuente,RestoDeColores):-
    % Obtiene los pixeles con el color frecuente.
    color_frecuente_rgb(Lista, R,G,B, ColorFrecuente),
    % genera una lista con el resto de los colores de la lista.
	resto_de_colores_rgb(Lista, R,G,B, RestoDeColores).

/*-------------------------------------PREDICADO COLOR-HISTOGRAMA------------------------------------------------------*/
% Dominio: Lista de colores con su cantidad.
% Descripcion: Toma el color que tiene la mayor cantidad.

color_histograma( [[Color,_]|_], Color).
color_histograma_rgb( [[[R,G,B],_]|_], R,G,B).

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*-------------------------------------PREDICADO ELIMINAR-PIXEL------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Elimina un pixel de una posicion especifica. Utilizado en imageChangePixel.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
eliminar_pixel([], _, _,[]).
eliminar_pixel([[X,Y,_,_]|Cola], X2,Y2, Resultado):- 
    % Comparacion de posiciones de pixeles con lo ingresado.
    X == X2, Y == Y2 ->   
    % Llamada recursiva.
    eliminar_pixel(Cola,X2,Y2, Resultado). 
% Guarda los pixeles que no corresponden con lo ingresado.
eliminar_pixel([[X,Y,Color, Profundidad]|Cola], X2,Y2, [[X, Y, Color, Profundidad]|Resultado]):- 
    % Llamada recursiva.
    eliminar_pixel(Cola, X2,Y2, Resultado).

/*-------------------------------------PREDICADO AGREGAR-PIXEL---------------------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Inserta el pixel ingresado en la lista de pixeles. Utilizado en imageChangePixel.

agregar_pixel(ListaPixeles, [X,Y,Color, Profundidad], ListaPixelesConUnoNuevo):-
    % Elimina el pixel que tiene la misma posicion que el ingresado.
    eliminar_pixel(ListaPixeles, X,Y, Resultado),
    % Inserta el pixel nuevo.
    append(Resultado, [[X,Y,Color, Profundidad]], Lista),
    % Ordena toda la lista de menor a mayor segun el primer elemento.
    ordenar_segun_x(Lista,ListaPixelesConUnoNuevo).
/*-------------------------------------PREDICADO FLIPH_AUX------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al ancho y una lista de tamaño 4 con la informacion de los  pixeles
% Descripcion: Función auxiliar que permite invertir una imagen horizontalmente,cuando la imagen corresponde a un bitmap o hexmap.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
flipH_Aux(_,[], []).
flipH_Aux(Ancho,[[X,Y,Color,Profundidad]|Cola], [[X1,Y1,Color,Profundidad1]|ColaResultado]):-
    % Se produce el cambio en la posicion X, invirtiendo su posicion.
  	X1 is (Ancho - 1) - X ,
    Y1 is Y, 
    Profundidad1 is Profundidad,
    % Llamada recursiva.
    flipH_Aux(Ancho, Cola, ColaResultado).

/*-------------------------------------PREDICADO FLIPV_AUX------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al alto y una lista de tamaño 4 con la informacion de los  pixeles
% Descripcion: Función auxiliar que permite invertir una imagen verticalmente,cuando la imagen corresponde a un bitmap o hexmap.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
flipV_Aux(_,[], []).
flipV_Aux(Alto,[[X,Y,Color,Profundidad]|Cola], [[X1,Y1,Color,Profundidad1]|ColaResultado]):-
    % Se produce el cambio en la posicion Y, invirtiendo su posicion.
  	X1 is  X ,
    Y1 is (Alto - 1) - Y, 
    Profundidad1 is Profundidad,
    % Llamada recursiva.
    flipV_Aux(Alto, Cola, ColaResultado).

/*-------------------------------------PREDICADO CROP------------------------------------------------------*/
% Dominio: Lista de pixeles e intervalos de seleccion.
% Descripcion: recibe la lista de los pixeles de una imagen y los datos necesarios para calcular la nueva cantidad de pixeles .Utilizado en imageCrop.

crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados ):-
    % Primero se seleccionan los pixeles que se ubican entre xa y xb.
	seleccionar_pixeles(ListaPixeles, Xa , Xb , Pixeles_SeleccionadosX),
    % Con estos pixeles se ordenan de menor a mayor segun los valores de y de cada pixel.
	ordenar_segun_y(Pixeles_SeleccionadosX, Pixeles_ordenados),
    % Se vuelve a calcular la cantidad de pixeles que se ubican en el intervalo pero ahora con ya e yb.
    seleccionar_pixeles( Pixeles_ordenados, Ya ,  Yb , Pixeles_SeleccionadosY),
    % Teniendo los elementos que se encuentran entre esos valores se vueve a ordenar la lista pero usando x.
    ordenar_segun_x(Pixeles_SeleccionadosY, PixelesSeleccionados ).

/*-------------------------------------PREDICADO CROP-AUX------------------------------------------------------*/
% Dominio: Lista de pixeles, 5 enteros que representan x1,x2,y1,y2 y el alto.
% Descripcion: toma la cantidad de pixeles que se encuentran entre los cuadrantes definidos. Utilizado en imageCrop.

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
    % cuando no se cumplen los casos se retorna la llamada del predicado auxiliar sin cambios
    % Los valores son multiplicados por el alto de la imagen, asi se sabe cuantos elementos seleccionar,
	Xa is X1 * Alto + 1 , Ya is Y1 *Y2 + 1,
    	Xb is (X2 + 1)* Alto, Yb is Y2  * Alto, crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados).

/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/
/*-------------------------------------PREDICADO ORDENAR-SEGUN-Y ------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Ordena los pixeles de menor a mayor utilizando la posicion y. Utilizado en imageChangePixel.

ordenar_segun_y(Lista_pixeles, Pixeles_ordenados):-
    % Ordena de menor a mayor sin eliminar duplicados.
    sort(2, @=<, Lista_pixeles, Pixeles_ordenados).

/*-------------------------------------PREDICADO ORDENAR-SEGUN-X ------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Ordena los pixeles de menor a mayor utilizando la posicion x. Utilizado en imageChangePixel.

ordenar_segun_x(ListaPixeles, Pixeles_ordenados):-
    % Ordena de menor a mayor sin eliminar duplicados.
    sort(1, @=<, ListaPixeles, Pixeles_ordenados).

/*-------------------------------------PREDICADO ORDENAR-LISTA------------------------------------------------------*/
% Dominio: Lista de histograma
% Descripcion:Orena de menor a mayor una lista. Utilizado en imageToHistogram.

ordenar_lista(Lista, ListaOrdenada):-
    sort(0, @=<, Lista,ListaOrdenada).

/*-------------------------------------PREDICADO LARGO-LISTA------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Calcula el largo de una lista de pixeles. Utilizado en imageIsCompressed.

% Caso base donde la lista esta vacia y el contador comienza en 0.
largo_lista([],0).
largo_lista([_|Cola],Contador) :- 
    largo_lista(Cola,Contador1), 
    % Cuenta los elementos.
    Contador is Contador1 + 1.

/*-------------------------------------PREDICADO CONTAR-COLOR------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Cuenta cuantas veces se repite cada color en una lista de pixeles. Utilizado en imageToHistogram.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
contar_color([],[]).
% Se selecciona el color 
contar_color([Color|Cola],[ColorSalida|ColaResultado]):- 
    % Llamada a la funcion para contar los colores.
    contar(Color,Cola,ListaResultante,1,ColorSalida), 
    % Llamada recursiva.
    contar_color(ListaResultante,ColaResultado),!.
% Caso base, cuando solo se repite una vez.
contar(Color,[],[],1,[Color,1]).
% Desde 1 se comienza a guardar la cantidad y el color correspondiente en la lista de salida.
contar(Color,[],[],Cantidad,[Color,Cantidad]):- 
    Cantidad >= 1.
contar(Color,[ColorSiguiente|ListaResultante],[ColorSiguiente|ListaResultante],1,[Color,1]) :- 
    Color \= ColorSiguiente.
contar(Color,[ColorSiguiente|ListaResultante],[ColorSiguiente|ListaResultante],Cantidad,[Color,Cantidad]) :- 
    % Aumenta cuando un elemento esta varias veces
    Cantidad >= 1, 
    % cuando los elementos no son iguales no pasa nada
    Color \= ColorSiguiente.
contar(Color,[Color|Cola],ListaResultante,Contador,Resultado) :- 
    % Para moverse en la lista
    Contador1 is Contador + 1, 
    % Llamada recursiva.
    contar(Color,Cola,ListaResultante,Contador1,Resultado),!.

/*-------------------------------------PREDICADO ROTATE-AUX--------------------------------------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Rota una imagen invirtiendo la posicion de x con y, (la ultima posicion de y es la primera, la penultima la segunda y asi sucesivamente), la
% la posicion Y toma el valor de X. Utilizado en imageRotate90. 

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.    
rotate_Aux(_,[], []).
rotate_Aux(Ancho,[[X,Y,Color,Profundidad]|Cola], [[X1,Y1,Color,Profundidad1]|ColaResultado]):-
  	% Transformacion del valor de X.
    X1 is (Ancho - 1) - Y ,
    % Intercambio de X con Y.
    Y1 is X, 
    Profundidad1 is Profundidad,
    % Llamada recursiva.
    rotate_Aux(Ancho, Cola, ColaResultado).

/*-------------------------------------PREDICADO INSERTAR-ANCHO-ALTO------------------------------------------------------*/
% Dominio: Ancho, Alto, Lista con pixeles.
% Descripcion: inserta el ancho y el alto en la lista de los pixeles generados en las capas de una imagen, utilizado en imageDepthLayers.
 
 % Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.   
insertarAnchoAlto(_,_,[],[]).
% Se inserta el ancho y alto en una lista con la lista de pixeles.
insertarAnchoAlto(Ancho,Alto,[Pixeles|Cola], [[Ancho,Alto, Pixeles]|ColaResultado]):-
    % Llamada recursiva.
    insertarAnchoAlto(Ancho,Alto,Cola,ColaResultado).
 
/*--------------------------------SEGUNDA PARTE DEL ARCHIVO--------------------------------------------------------------*/

/*              AQUI SE ENCUENTRAN LOS PREDICADOS REQUERIDOS EN EL LABORATORIO                                           */

/* ----------------------------------------------------------------------------------------------------------------------*/
/*-----------------------------------------------------CONSTRUCTORES-----------------------------------------------------*/
/*-------------------------------------PREDICADO IMAGE ------------------------------------------------------------------*/
% Dominio: datos de tipo int y una lista
% Descripcion: Constructor del tda image que toma un alto, ancho y una lista con pixeles

image(Ancho, Alto, Lista_pixeles, [Ancho, Alto, Lista_pixeles] ):-
    % Analiza que lo ingresado corresponda al tipo de elemento solicitado.
    integer(Ancho), integer(Alto),
    % La imagen no puede tener un alto o ancho menor a uno.
    Ancho >= 1, Alto >= 1.

/*-------------------------------------PREDICADO IMAGE-IS-COMPRESSED-----------------------------------------------------*/
% Dominio: image
% Recorrido: Boolean.
% Descripcion: Ve si la imagen recibida fue comprimida o no.

imageIsCompressed(Image):-
    % Recibe la imagen.
    image(Alto,Ancho, ListaPixeles,Image),
    % Calcula el total de pixeles que deberia tener la imagen.
    Total is Alto * Ancho,
    % Calcula el largo de la lista de pixeles.
    largo_lista(ListaPixeles, Largo),
    % una imagen comprimida tendra menos pixeles que lo que indica el ancho*alto.
    Total == Largo ->  
        writeln('#f');
        writeln('#t').

/*-------------------------------------PREDICADO FLIPH------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Descripcion:  Función que permite invertir una imagen horizontalmente, cambiando el valor de la posicion de x, y dejando sin alterar la posicion de y. 

imageFlipH(Image, ImagenInvertida):-
    % Ve el tipo de imagen corresponde a hexmap
    imageIsHexmap(Image)->  
        % Recibe la imagen y sus elementos.
    	image(Ancho, Alto, ListaPixeles, Image), 
        % Realiza la inversion.
        flipH_Aux(Ancho,ListaPixeles, PixelesInvertidos), 
        % Retorna la imagen con los pixeles invertidos
        image(Ancho, Alto, PixelesInvertidos, ImagenInvertida);
    % Ve el tipo de imagen corresponde a bitmap
    imageIsBitmap(Image)->  
        % Recibe la imagen y sus elementos.
    	image(Ancho, Alto, ListaPixeles, Image), 
        % Realiza la inversion.
        flipH_Aux(Ancho,ListaPixeles, PixelesInvertidos), 
        % Retorna la imagen con los pixeles invertidos
        image(Ancho, Alto, PixelesInvertidos, ImagenInvertida);
    % Ve el tipo de imagen corresponde a pixmap    
    imageIsPixmap(Image)->  
        % Recibe la imagen y sus elementos.
    	image(Ancho, Alto, ListaPixeles, Image), 
        % Realiza la inversion.
        flipH_AuxRGB(Ancho,ListaPixeles, PixelesInvertidos), 
        % Retorna la imagen con los pixeles invertidos
        image(Ancho, Alto, PixelesInvertidos, ImagenInvertida).

/*------------------------------------- PREDICADO FLIPV------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Descripcion:  Función que permite invertir una imagen verticalmente, cambiando el valor de la posicion de y, y dejando sin alterar la posicion de x. 

imageFlipV(Image, ImagenInvertida):-
    % Ve el tipo de imagen corresponde a hexmap
    imageIsHexmap(Image)-> 
        % Recibe la imagen y sus elementos. 
    	image(Ancho, Alto, ListaPixeles, Image), 
        % Realiza la inversion.
        flipV_Aux(Ancho,ListaPixeles, PixelesInvertidos), 
        % Retorna la imagen con los pixeles invertidos
        image(Ancho, Alto, PixelesInvertidos, ImagenInvertida);
    % Ve el tipo de imagen corresponde a bitmap    
    imageIsBitmap(Image)->  
        % Recibe la imagen y sus elementos.
    	image(Ancho, Alto, ListaPixeles, Image),
        % Realiza la inversion.
        flipV_Aux(Ancho,ListaPixeles, PixelesInvertidos), 
        % Retorna la imagen con los pixeles invertidos
        image(Ancho, Alto, PixelesInvertidos, ImagenInvertida);
    % Ve el tipo de imagen corresponde a pixmap    
    imageIsPixmap(Image)->  
        % Recibe la imagen y sus elementos.
    	image(Ancho, Alto, ListaPixeles, Image), 
        % Realiza la inversion.
        flipV_AuxRGB(Ancho,ListaPixeles, PixelesInvertidos), 
        % Retorna la imagen con los pixeles invertidos
        image(Ancho, Alto, PixelesInvertidos, ImagenInvertida).

/*-------------------------------------PREDICADO IMAGE-CROP------------------------------------------------------*/
% Dominio: image
% Descripcion: Permiete cortar una imagen dado un cuadrante definido.

imageCrop(Image,X1,Y1, X2,Y2, ImageCortada):-
    % Recibe la imagen y sus elementos.
    image(Ancho,Alto, ListaPixeles, Image),
    % Con cropAux realiza la seleccion de pixeles.
    cropAux(ListaPixeles, X1,Y1, X2,Y2,Alto, PixelesSeleccionados),
    % Retorna la imagen cortada
    image(Ancho,Alto, PixelesSeleccionados, ImageCortada).

/*-------------------------------------PREDICADO IMAGE-RGB-TO-HEX------------------------------------------------------*/
% Dominio: Image
% Descripcion: transforma una imagen e tipo pixmap a una hexmap.

imageRGBToHex(ImageRGB, ImageHEX):-
    % Recibe la imagen y sus elementos.
    image(Ancho,Alto, ListaPixeles, ImageRGB),
    % Realiza la transformacion de rgb a hex en la lista de pixeles.
  	rgb_hex(ListaPixeles, PixelesAHex),
    % Retorna la imagen de tipo hexmap.
    image(Ancho,Alto, PixelesAHex, ImageHEX).

/*-------------------------------------PREDICADO IMAGE-TO-HISTOGRAM------------------------------------------------------*/
% Dominio: Image
% Descripcion: Cuenta la cantidad de colores que tiene una imagen y devuelve una lista con esta informacion.

imageToHistogram(Image, Histograma):-
    % Ve el tipo de imagen corresponde a hexmap  
    imageIsHexmap(Image)->  
        % Recibe la imagen y sus elementos.
		image(_,_, ListaPixeles, Image),
        % Selecciona los colores en una lista
        seleccionar_hex(ListaPixeles, PixelesHex),
        % Ordena la lista.
        ordenar_lista(PixelesHex,PixelesHexOrdenados),
        % Cuenta los colores y retorna el histograma
        contar_color(PixelesHexOrdenados, Histograma);
    % Ve el tipo de imagen corresponde a pixmap  
    imageIsPixmap(Image)->  
        % Recibe la imagen y sus elementos.
		image(_,_, ListaPixeles, Image),
        % Selecciona los colores en una lista
        seleccionar_rgb(ListaPixeles, PixelesRGB),
        % Ordena la lista.
        ordenar_lista(PixelesRGB,PixelesRGBOrdenados),
        % Cuenta los colores y retorna el histograma
        contar_color(PixelesRGBOrdenados, Histograma);
    % Ve el tipo de imagen corresponde a bitmap  
    imageIsBitmap(Image)->  
        % Recibe la imagen y sus elementos.
		image(_,_, ListaPixeles, Image),
        % Selecciona los colores en una lista
        seleccionar_bit(ListaPixeles, PixelesBit),
        % Ordena la lista.
        ordenar_lista(PixelesBit,PixelesBitOrdenados),
        % Cuenta los colores y retorna el histograma
        contar_color(PixelesBitOrdenados, Histograma).

/*-------------------------------------PREDICADO IMAGE-ROTATE-90------------------------------------------------------*/
% Dominio: image
% Descripcion: Rota una imagen 90 grados a la derecha.

imageRotate90(Image, ImagenRotada):-
    % si la imagen es de tipo pixmap se llama al predicado auxiliar rotate_AuxRGB
    imageIsPixmap(Image)->  
        % Recibe la imagen y sus elementos.
    	image( Ancho,Alto, ListaPixeles, Image),
        % Rota los pixeles.
    	rotate_AuxRGB(Alto,ListaPixeles, PixelesRotados),
        % Ordena la lista de pixeles.
    	ordenar_segun_x(PixelesRotados,PixelesOrdenado),
        % Retorna la imagen rotada.
    	image(Ancho, Alto, PixelesOrdenado, ImagenRotada);
    % sino se llama al predicado rotate_Aux
    % Recibe la imagen y sus elementos.
    image(Ancho,Alto, ListaPixeles, Image), 
    % Rota los pixeles.
    rotate_Aux(Alto,ListaPixeles, PixelesRotados), 
    % Ordena la lista de pixeles.
   	ordenar_segun_x(PixelesRotados,PixelesOrdenado),
    % Retorna la imagen rotada.
    image(Ancho, Alto, PixelesOrdenado, ImagenRotada).

/*-------------------------------------PREDICADO IMAGE-COMPRESS------------------------------------------------------*/
% Dominio: Image
% Descripcion:Comprime una imagen sacando los pixeles que mas se repiten. la z indica la lista con los pixeles sacados.

imageCompress(Image, ImageComprimida,Z):-
    % si la imagen es de tipo pixmap
    imageIsPixmap(Image)->  
        % Recibe la imagen y sus elementos.
    	image(Alto,Ancho, ListaPixeles, Image),
        % Calcula el histograma.
    	imageToHistogram(Image, Histograma),
        % obtiene el color mas repetido
    	color_histograma_rgb( Histograma, R,G,B),
        % realiza la separacion entre los pixeles que mas se repiten y los que no
    	colores_rgb(ListaPixeles,R,G,B, Z,PixelesComprimidos),
        % Retorna la imagen comprimida
    	image(Alto,Ancho, PixelesComprimidos, ImageComprimida);
    % Sino se realiza la compresion normalmente.
    image(Alto,Ancho, ListaPixeles, Image),
    % Calcula el histograma.
    imageToHistogram(Image, Histograma),
    % obtiene el color mas repetido
    color_histograma( Histograma, Color),
    % realiza la separacion entre los pixeles que mas se repiten y los que no
    colores(ListaPixeles,Color,Z,PixelesComprimidos),
    % Retorna la imagen comprimida
    image(Alto,Ancho, PixelesComprimidos, ImageComprimida).

/*-------------------------------------PREDICADO IMAGE-CHANGE-PIXEL------------------------------------------------------*/
% Dominio: Image
% Descripcion: Cambia un pixel a eleccion.

imageChangePixel(Image, Pixel, ImagenModificada):-
    % Ve si el pixel ingresado es de tipo pixrgb
    largo_lista(Pixel, Largo),
    Largo==6 ->  
        % Recibe la imagen y sus elementos.
		image(Alto,Ancho, ListaPixeles, Image),
        % Realiza el cambio de un pixel con otro
		agregar_pixel_rgb(ListaPixeles, Pixel, ListaNueva),
        % Retorna la imagen modificada
		image(Alto,Ancho, ListaNueva, ImagenModificada);
    % Sino realiza el cambio normalmente
    image(Alto,Ancho, ListaPixeles, Image),
    % Realiza el cambio de un pixel con otro
    agregar_pixel(ListaPixeles, Pixel, ListaNueva),
    % Retorna la imagen modificada
    image(Alto,Ancho, ListaNueva, ImagenModificada).

/*-------------------------------------PREDICADO IMAGE-TO-STRING------------------------------------------------------*/
% Dominio: Image
% Descripcion: Transforma una imagen de lista a una representacion string.

imageToString(Image, ImageEnString):-
    % ve si la imagen es de tipo pixmap
    imageIsHexmap(Image)->  
        % Recibe la imagen y sus elementos.
    	image(_,Alto,ListaPixeles,Image),
        % Realiza la transformacion a string
    	pixeles_a_string_hex(ListaPixeles, Alto, ImageEnString);
    % ve si la imagen es de tipo pixmap
    imageIsPixmap(Image)->  
        % Recibe la imagen y sus elementos.
    	image(_,Alto,ListaPixeles,Image),
        % Realiza la transformacion a string
    	pixeles_a_string_rgb(ListaPixeles, Alto, ImageEnString);
    % ve si la imagen es de tipo pixmap
    imageIsBitmap(Image)->  
        % Recibe la imagen y sus elementos.
    	image(_,Alto,ListaPixeles,Image),
        % Realiza la transformacion a string
    	pixeles_a_string_bit(ListaPixeles, Alto, ImageEnString).

/*-------------------------------------PREDICADO IMAGE-DEPTH-LAYERS------------------------------------------------------*/
% Dominio: Image
% Descripcion: Transforma ua imagen en un conjunto de imagenes de diferentes profundidades, se agrupan los pixeles que tienen
% la misma profundidad y se rellena con pixeles blancos.

imageDepthLayers(Image, ImageEnCapas):-
    % ve si la imagen es de tipo pixmap
    imageIsPixmap(Image)->  
        % Recibe la imagen y sus elementos.
        image(Ancho,Alto,ListaPixeles,Image),
        % Selecciona la profundidad de todos los pixeles y calcula su largo.
        seleccionar_profundidad_rgb(ListaPixeles,Profundidades),
        (length(Profundidades,LargoProfundidades),
        % si el largo de la lista de profundidad es menor al ancho*alto, significa que se repiten profundidades
        LargoProfundidades < Ancho * Alto ->  
            % se separan las capas agregando los pixeles blancos.
            separar_capas_repeticion_profundidades_rgb(ListaPixeles,ListaPixelesSeparados),
            % Se inserta el ancho y el alto a las capas generadas y se retornan las nuevas imagenes
            insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas);
        % Si el largo de la lista de profundidad no es menor significa que no hay profundidades repetidas
        % se separan las capas agregando los pixeles blancos.
        separar_capas_rgb(ListaPixeles,ListaPixelesSeparados),
        % Se inserta el ancho y el alto a las capas generadas y se retornan las nuevas imagenes
        insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas));
    % ve si la imagen es de tipo pixmap
    imageIsBitmap(Image)->  
        % Recibe la imagen y sus elementos.
        image(Ancho,Alto,ListaPixeles,Image),
        % Selecciona la profundidad de todos los pixeles y calcula su largo.
        seleccionar_profundidad_bit(ListaPixeles,Profundidades),
        (length(Profundidades,LargoProfundidades),
        % si el largo de la lista de profundidad es menor al ancho*alto, significa que se repiten profundidades
        LargoProfundidades< Ancho * Alto ->  
            % se separan las capas agregando los pixeles blancos.
            separar_capas_repeticion_profundidades_bit(ListaPixeles,ListaPixelesSeparados),
            insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas);
        % Si el largo de la lista de profundidad no es menor significa que no hay profundidades repetidas
        % se separan las capas agregando los pixeles blancos.
        separar_capas_bit(ListaPixeles,ListaPixelesSeparados),
        % Se inserta el ancho y el alto a las capas generadas y se retornan las nuevas imagenes
        insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas));
    % ve si la imagen es de tipo pixmap
    imageIsHexmap(Image)->  
        % Recibe la imagen y sus elementos.
        image(Ancho,Alto,ListaPixeles,Image),
        % Selecciona la profundidad de todos los pixeles y calcula su largo.
        seleccionar_profundidad_hex(ListaPixeles,Profundidades),
        (length(Profundidades,LargoProfundidades),
        % si el largo de la lista de profundidad es menor al ancho*alto, significa que se repiten profundidades
        LargoProfundidades< Ancho * Alto ->  
            % se separan las capas agregando los pixeles blancos.
            separar_capas_repeticion_profundidades_hex(ListaPixeles,ListaPixelesSeparados),
            insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas);
        % Si el largo de la lista de profundidad no es menor significa que no hay profundidades repetidas
        % se separan las capas agregando los pixeles blancos.
        separar_capas_hex(ListaPixeles,ListaPixelesSeparados),
        % Se inserta el ancho y el alto a las capas generadas y se retornan las nuevas imagenes
        insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas)).

/*-------------------------------------PREDICADO IMAGE-DECOMPRESS------------------------------------------------------*/
% Dominio: Image
% Descripcion: descomprime una imagen agregando los pixeles que fueron eliminados en la compresion.

imageDecompress([Ancho,Alto,Pixeles] , PixelesEliminados , [Ancho,Alto,ImagenDescomprimida]) :- 
    % A la imagen comprimida se le agregan los pixeles que fueron eliminados.
    append(Pixeles,PixelesEliminados,ImagenDesordenada),
    % se ordenan los pixeles para que se tenga el mimso orden que la imagen original
    ordenar_segun_y(ImagenDesordenada,ImagenDesordenada1),
    ordenar_segun_x(ImagenDesordenada1,ImagenDescomprimida).