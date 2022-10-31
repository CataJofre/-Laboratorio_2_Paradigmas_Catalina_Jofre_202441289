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

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/

% Este tda corresponde a la representacion de una imagen que se compone de diversas caracteristicas como el
% alto y ancho, ademas de tener diferentes tipos de pixeles (pixbit-d, pixrgb-d pixhex-d), lo que le da la
% posibilidad de reproducir diferentes aspectos de lo que es una imagen, usando los colores y la profundidad.

% Es una lista que contiene el alto ancho y una lista con la cantidad correspondiente de pixeles

/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
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
% Descripcion:
color_frecuente([], _, []).
color_frecuente([[_,_, ColorPixel,_]|Cola], ColorFrecuente, Resultado):- 
   ColorPixel\=ColorFrecuente->   
    color_frecuente(Cola, ColorFrecuente, Resultado). 
color_frecuente([[X, Y, ColorPixel, Profundidad]|Cola], ColorFrecuente, [[X, Y, ColorPixel, Profundidad]|Resultado]):- 
    color_frecuente(Cola, ColorFrecuente, Resultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

resto_de_colores([], _, []).
resto_de_colores([[_,_, ColorPixel,_]|Cola], ColorFrecuente, Resultado):- 
   ColorPixel==ColorFrecuente->   
   resto_de_colores(Cola, ColorFrecuente, Resultado). 
resto_de_colores([[X, Y, ColorPixel, Profundidad]|Cola], ColorFrecuente, [[X, Y, ColorPixel, Profundidad]|Resultado]):- 
    resto_de_colores(Cola, ColorFrecuente, Resultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

colores(Lista, Color, ColorFrecuente,RestoDeColores):-
    color_frecuente(Lista, Color, ColorFrecuente),
	resto_de_colores(Lista, Color, RestoDeColores).

colores_rgb(Lista, R,G,B, ColorFrecuente,RestoDeColores):-
    color_frecuente_rgb(Lista, R,G,B, ColorFrecuente),
	resto_de_colores_rgb(Lista, R,G,B, RestoDeColores).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

color_histograma( [[Color,_]|_], Color).
color_histograma_rgb( [[[R,G,B],_]|_], R,G,B).

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

eliminar_pixel([], _, _,[]).
eliminar_pixel([[X,Y,_,_]|Cola], X2,Y2, Resultado):- 
   X == X2, Y == Y2 ->   
   eliminar_pixel(Cola,X2,Y2, Resultado). 
eliminar_pixel([[X,Y,Color, Profundidad]|Cola], X2,Y2, [[X, Y, Color, Profundidad]|Resultado]):- 
    eliminar_pixel(Cola, X2,Y2, Resultado).
    
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

agregar_pixel(ListaPixeles, [X,Y,Color, Profundidad], Image2):-
    eliminar_pixel_rgb(ListaPixeles, X,Y, Resultado),
    append(Resultado, [[X,Y,Color, Profundidad]], Imag),
    ordenar_segun_x(Imag,Image2).

/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

ordenar_segun_y(Lista_pixeles, Pixeles_ordenados):-
    sort(2, @=<, Lista_pixeles, Pixeles_ordenados).

/*-------------------------------------PREDICADO ORDENAR-SEGUN-X ------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Ordena los pixeles de menor a mayor utilizando la posicion x.

ordenar_segun_x(Lista_pixeles, Pixeles_ordenados):-
    sort(1, @=<, Lista_pixeles, Pixeles_ordenados).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

ordenar_lista(Lista, ListaOrdenada):-
    sort(0, @=<, Lista,ListaOrdenada).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

largo_lista([],0).
largo_lista([_|Cola],Contador) :- 
    largo_lista(Cola,Contador1), 
   Contador is Contador1 + 1.

/*-------------------------------------PREDICADO FLIPH_AUX------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al ancho y una lista de tamaño 4 con la informacion de los  pixeles
% Descripcion: Función auxiliar que permite invertir una imagen horizontalmente,cuando la imagen corresponde a un bitmap o hexmap.
% Tipo de Meta: Secundaria.

flipH_Aux(_,[], []).
flipH_Aux(Ancho,[[X,Y,Color,Profundidad]|Cola], [[X1,Y1,Color,Profundidad1]|ColaResultado]):-
  	X1 is (Ancho - 1) - X ,
    Y1 is Y, 
    Profundidad1 is Profundidad,
    flipH_Aux(Ancho, Cola, ColaResultado).


/*-------------------------------------PREDICADO FLIPV_AUX------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al alto y una lista de tamaño 4 con la informacion de los  pixeles
% Descripcion: Función auxiliar que permite invertir una imagen verticalmente,cuando la imagen corresponde a un bitmap o hexmap.
% Tipo de Meta: Secundaria.

flipV_Aux(_,[], []).
flipV_Aux(Alto,[[X,Y,Color,Profundidad]|Cola], [[X1,Y1,Color,Profundidad1]|ColaResultado]):-
  	X1 is  X ,
    Y1 is (Alto - 1) - Y, 
    Profundidad1 is Profundidad,
    flipV_Aux(Alto, Cola, ColaResultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

crop(ListaPixeles, Xa,Ya, Xb,Yb, PixelesSeleccionados ):-
	seleccionar_pixeles(ListaPixeles, Xa , Xb , Pixeles_SeleccionadosX),
	ordenar_segun_y(Pixeles_SeleccionadosX, Pixeles_ordenados),
    seleccionar_pixeles( Pixeles_ordenados, Ya ,  Yb , Pixeles_SeleccionadosY),
    ordenar_segun_x(Pixeles_SeleccionadosY, PixelesSeleccionados ).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
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
% Descripcion:

contar_elemento([],[]).
contar_elemento([Elemento|Cola],[ElementoSalida|ColaResultado]) :- 
    contar(Elemento,Cola,ListaResultante,1,ElementoSalida), 
    contar_elemento(ListaResultante,ColaResultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

rotate_Aux(_,[], []).
rotate_Aux(Ancho,[[X,Y,Color,Profundidad]|Cola], [[X1,Y1,Color,Profundidad1]|ColaResultado]):-
  	X1 is (Ancho - 1) - Y ,
    Y1 is X, 
    Profundidad1 is Profundidad,
   rotate_Aux(Ancho, Cola, ColaResultado).

/*-------------------------------------PREDICADO IMPAR------------------------------------------------------*/
% Dominio: Entero, Alto de la imagen
% Recorrido: Boolean
% Descripcion: Ve si el valor del alto de la imagen corresponde a un numero impar o no.

impar(N):- 
    mod(N,2) =:= 0.
/*-------------------------------------PREDICADO AGREGAR-SALTO------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Inserta el simbolo de salto de linea, luego de una cantidad X de elementos, (X corresponde al alto de la imagen), para que cuando se lea hayan saltos entre los elementos

agregar_salto(Lista,Alto,ListaConSalto):-
    agregar_salto(Lista,Alto,ListaConSalto,Alto).
agregar_salto([],_,[],_).
agregar_salto([_|Cola],Alto,["\n",ColaResultado],0):- 
    agregar_salto(Cola,Alto,ColaResultado,Alto).
agregar_salto([Cabeza|Cola],Alto,[Cabeza|ColaResultado],Contador):- 
    Contador > 0,
    Contador1 is Contador - 1, 
    agregar_salto(Cola,Alto,ColaResultado,Contador1).

/*-------------------------------------PREDICADO AGREGAR-TAB------------------------------------------------------*/
% Dominio:Lista de pixeles
% Descripcion: Inserta el simbolo tab luego de cada elemento de la lista, para que cuando se lea haya un espacio entre estos

agregar_tab([],[]) .      
agregar_tab([Cabeza|Cola],[Cabeza,"\t"|ColaResultado]):- 
    agregar_tab(Cola,ColaResultado).

/*-------------------------------------PREDICADO QUITAR-PRIMER-PIXEL------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion:Elimina el primer pixel de una lista de pixeles.
 
quitar_primer_pixel([_|Tail], Tail).
  
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
insertarAnchoAlto(_,_,[],[]).
insertarAnchoAlto(Ancho,Alto,[Pixeles|Cola], [[Ancho,Alto, Pixeles]|ColaResultado]):-
    insertarAnchoAlto(Ancho,Alto,Cola,ColaResultado).
 
/*-------------------------------------PREDICADO INVERTIR------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Invierte el orden de los pixeles, dejando la profundidad al principio.
 
invertir([],[]).
invertir([Cabeza| Cola], [CabezaInvertida|ColaResultado]):-   
    reverse(Cabeza,CabezaInvertida),
    invertir(Cola,ColaResultado).
 
/*-------------------------------------PREDICADO PROFUNDIDAD------------------------------------------------------*/
% Dominio:Lista de pixeles
% Descripcion: Ordena los pixeles de menor a mayor segun la profundidad del pixel.
 
ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados):-
    invertir(ListaPixeles,ListaPixelesInvertidos),
	sort(0, @=<, ListaPixelesInvertidos, ListaPixelesOrdenadosInvertidos),
	invertir(ListaPixelesOrdenadosInvertidos,ListaPixelesOrdenados).
 
/*-------------------------------------PREDICADO AGRUPAR-POR-PROFUNDIDAD------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Agrupa los pixeles que tienen la misma profundidad.
 
agrupar_por_profundidad(ListaPixles,ListaAgrupada):-
    agrupar(ListaPixles,[],ListaAgrupada).
 
agrupar([],[],[]).
agrupar([ListaPixel],ListaPixeles,[PixelesConMismaProfundidad]):- 
    append([ListaPixel],ListaPixeles,PixelesConMismaProfundidad).
agrupar([ListaPixel,ListaPixel1|Cola],PixelesConMismaProfundidad,PixelesConMismaProfundidadLista):- 
    last(ListaPixel,Profundidad),  
    last(ListaPixel1,Profundidad1),
    Profundidad== Profundidad1 , 
    append([ListaPixel],PixelesConMismaProfundidad,PixelesConMismaProfundidad1) , 
   	agrupar([ListaPixel1|Cola],PixelesConMismaProfundidad1,PixelesConMismaProfundidadLista).
agrupar([ListaPixel,ListaPixel1|Cola],PixelesConDistintaProfundidad,[PixelesConDistintaProfundidad1|ColaResultado]):- 
    last(ListaPixel,Profundidad),  
    last(ListaPixel1,Profundidad1),
    Profundidad\=Profundidad1 ,
    append([ListaPixel],PixelesConDistintaProfundidad,PixelesConDistintaProfundidad1) ,
 	agrupar([ListaPixel1|Cola],[],ColaResultado).
	
/*--------------------------------SEGUNDA PARTE DEL ARCHIVO------------------------------------------------*/

/*              AQUI SE ENCUENTRAN LOS PREDICADOS REQUERIDOS EN EL LABORATORIO                             */

/* --------------------------------------------------------------------------------------------------------*/

    
/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*-------------------------------------PREDICADO IMAGE ------------------------------------------------------*/
% Dominio: datos de tipo int y una lista
% Descripcion: Constructor del tda image que toma un alto, ancho y una lista con pixeles

image(Ancho, Alto, Lista_pixeles, [Ancho, Alto, Lista_pixeles] ):-
    integer(Ancho), integer(Alto),
    Ancho >= 1, Alto >= 1.

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

imageIsCompressed(Image):-
    image(Alto,Ancho, ListaPixeles,Image),
    Total is Alto * Ancho,
    largo_lista(ListaPixeles, Largo),
    Total == Largo ->  writeln('#f');
    writeln('#t').

/*-------------------------------------PREDICADO FLIPH------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Descripcion:  Función que permite invertir una imagen horizontalmente, cambiando el valor de la posicion de x, y dejando sin alterar la posicion de y. 
% Tipo de Meta: Primaria

imageFlipH(Image, I):-
    imageIsHexmap(Image)->  
    	image(Ancho, Alto, PixelsIn, Image), flipH_Aux(Ancho,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I);
    imageIsBitmap(Image)->  
    	image(Ancho, Alto, PixelsIn, Image), flipH_Aux(Ancho,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I);
    imageIsPixmap(Image)->  
    	image(Ancho, Alto, PixelsIn, Image), flipH_AuxRGB(Ancho,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I).

/*------------------------------------- PREDICADO FLIPV------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Descripcion:  Función que permite invertir una imagen verticalmente, cambiando el valor de la posicion de y, y dejando sin alterar la posicion de x. 
% Tipo de Meta: Primaria

imageFlipV(Image, I):-
 imageIsHexmap(Image)->  
    	image(Ancho, Alto, PixelsIn, Image), flipV_Aux(Ancho,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I);
    imageIsBitmap(Image)->  
    	image(Ancho, Alto, PixelsIn, Image), flipV_Aux(Ancho,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I);
    imageIsPixmap(Image)->  
    	image(Ancho, Alto, PixelsIn, Image), flipV_AuxRGB(Ancho,PixelsIn, PixelsOut), image(Ancho, Alto, PixelsOut, I).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion:

imageCrop(Image,X1,Y1, X2,Y2, Image2):-
    image(Ancho,Alto, ListaPixeles, Image),
    cropAux(ListaPixeles, X1,Y1, X2,Y2,Alto, PixelesSeleccionados),
    image(Ancho,Alto, PixelesSeleccionados, Image2).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

imageRGBToHex(Image, Image2):-
    image(Ancho,Alto, ListaPixeles, Image),
  	rgb_hex(ListaPixeles, PixelesAHex),
    image(Ancho,Alto, PixelesAHex, Image2).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
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
% Descripcion:

imageCompress(Image, ImageComprimida,Z):-
    imageIsPixmap(Image)->  
    	image(Alto,Ancho, ListaPixeles, Image),
    	imageToHistogram(Image, Histograma),
    	color_histograma_rgb( Histograma, R,G,B),
    	colores_rgb(ListaPixeles,R,G,B, Z,PixelesComprimidos),
    	image(Alto,Ancho, PixelesComprimidos, ImageComprimida);
    image(Alto,Ancho, ListaPixeles, Image),
    imageToHistogram(Image, Histograma),
    color_histograma( Histograma, Color),
    colores(ListaPixeles,Color,Z,PixelesComprimidos),
    image(Alto,Ancho, PixelesComprimidos, ImageComprimida).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
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

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
imageToString(Image, ImageEnString):-
    imageIsHexmap(Image)->  
    	image(_,Alto,ListaPixeles,Image),
    	pixeles_a_string_hex(ListaPixeles, Alto, ImageEnString);
     imageIsPixmap(Image)->  
    	image(_,Alto,ListaPixeles,Image),
    	pixeles_a_string_rgb(ListaPixeles, Alto, ImageEnString);
    imageIsBitmap(Image)->  
    	image(_,Alto,ListaPixeles,Image),
    	pixeles_a_string_bit(ListaPixeles, Alto, ImageEnString).
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
imageDepthLayers(Image, ImageEnCapas):-
        imageIsPixmap(Image)->  
            image(Ancho,Alto,ListaPixeles,Image),
            seleccionar_profundidad_rgb(ListaPixeles,Profundidades),
            (length(Profundidades,LargoProfundidades),
            LargoProfundidades < Ancho * Alto ->  
                separar_capas_repeticion_profundidades_rgb(ListaPixeles,ListaPixelesSeparados),
                insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas);
                separar_capas_rgb(ListaPixeles,ListaPixelesSeparados),
                insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas));
    
    imageIsBitmap(Image)->  
        image(Ancho,Alto,ListaPixeles,Image),
        seleccionar_profundidad_bit(ListaPixeles,Profundidades),
        (length(Profundidades,LargoProfundidades),
        LargoProfundidades< Ancho * Alto ->  
            separar_capas_repeticion_profundidades_bit(ListaPixeles,ListaPixelesSeparados),
            insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas);
            separar_capas_bit(ListaPixeles,ListaPixelesSeparados),
            insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas));
    
    imageIsHexmap(Image)->  
        image(Ancho,Alto,ListaPixeles,Image),
        seleccionar_profundidad_hex(ListaPixeles,Profundidades),
        (length(Profundidades,LargoProfundidades),
        LargoProfundidades< Ancho * Alto ->  
            separar_capas_repeticion_profundidades_hex(ListaPixeles,ListaPixelesSeparados),
            insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas);
            separar_capas_hex(ListaPixeles,ListaPixelesSeparados),
            insertarAnchoAlto(Ancho,Alto,ListaPixelesSeparados, ImageEnCapas)).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
imageDecompress([Ancho,Alto,Pixeles] , PixelesEliminados , [Ancho,Alto,ImagenDescomprimida]) :- 
    append(Pixeles,PixelesEliminados,ImagenDesordenada),
    ordenar_segun_y(ImagenDesordenada,ImagenDesordenada1),
    ordenar_segun_x(ImagenDesordenada1,ImagenDescomprimida).

