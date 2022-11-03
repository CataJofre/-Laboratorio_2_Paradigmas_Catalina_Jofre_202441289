% Predicado para exportar los predicados a otros archivos.
:- module(tda_pixrgb_20244128_JofreLeon,[pixrgb/7,imageIsPixmap/1,seleccionar_rgb/2,pixeles_a_string_rgb/3,color_frecuente_rgb/5,resto_de_colores_rgb/5,eliminar_pixel_rgb/4,agregar_pixel_rgb/3,
                                        flipV_AuxRGB/3,flipH_AuxRGB/3,rotate_AuxRGB/3,imageInvertColorRGB/2,seleccionar_profundidad_rgb/2,pixeles_en_blanco_rgb/2,pixeles_primera_posicion_rgb/2,agregar_profundidad_rgb/3,
                                        insertar_pixeles_blancos_rgb/3,insertar_pixeles_blancos_profundidad_repetida_rgb/3,separar_capas_repeticion_profundidades_rgb/2,separar_capas_rgb/2]).


/*-----------------------------------------------------TDA PIXRGB------------------------------------------------------------------------
Este archivo corresponde al TDA pixrgb, se encuentran todos los predicados necesarios para su creacion y tambien
los solicitados en el proyecto de laboratorio.
Por motivos de organizacion se realizo la division del archivo en dos partes principales, la primera corresponde
a aquellos predicados cuyas metas son secundarias por lo que aportan en el funcionamiento de los predicados solicitados
y la segunda donde se encuentran todos los predicados del laboratorio.


PRIMERA PARTE:  PREDICADOS AUXILIARES O PREDICADOS CON FUNCIONES ESPECIFICAS                                  */
% INFORMACION IMPORTANTE.

% Dominios:
    % TDA pixrgb
    % Posiciones X,Y
    % Colores R,G,B
    % Lista de pixeles.
    % Alto
    % Ancho
    % Imagen

% Predicados:
    % pixrgb(X, Y, R,G,B, Profundidad, Imagen).
    % seleccionar_rgb(ListaPixeles,ListaColores).
    % color_frecuente_rgb(ListaPixeles, R_frecuente, G_frecuente, B_frecuente, ListaPixelesColorFrecuente).
    % resto_de_colores_rgb(ListaPixeles, R_frecuente, G_frecuente, B_frecuente, ListaPixelesDeColoresNoFrecuentes).
    % seleccionar_profundidad_rgb(ListaProfundidad, ListaProfundidadSinDulplicas).
    % pixeles_a_string_rgb(ListaPixeles, Alto, ListaString).
    % eliminar_pixel_rgb(ListaPixeles, X2,Y2, ListaPixelesSinUnPixel).
    % insertar_pixeles_blancos_rgb(ListaPixeles,PixelesBlancos,ListaPixelesConPixelesBlancos).
	% insertar_pixeles_blancos_profundidad_repetida_rgb(ListaPixeles,PixelesBlancos,ListaPixelesConPixelesBlancos).
	% agregar_profundidad_rgb(PixelesBlancos,Profundidad,PixelesBlancosConNuevaProfundidad).
    % quitar_primer_pixel(ListaPixeles, ColaListaPixeles). 
    % flipV_AuxRGB(Alto,ListaPixeles, ListaPixelesModificada).
    % flipH_AuxRGB(Alto,ListaPixeles, ListaPixelesModificada).
    % rotate_AuxRGB(Ancho,ListaPixeles, ListaPixelesModificada).
    % agregar_salto(ListaPixeles,Alto,ListaPixelesConSalto).
    % agregar_tab(ListaPixeles,ListaPixelesConTab).
    % ordenar_segun_x(ListaPixeles, Pixeles_ordenados).
    % pixeles_en_blanco_rgb(ListaPixeles, ListaPixelesBlancos).
    % pixeles_primera_posicion_rgb(ListaPixeles,ListaPixeles).
	% separar_capas_repeticion_profundidades_rgb(ListaPixeles,ListaPixelesSeparados)
	% separar_capas_rgb(ListaPixeles, ListaPixelesSeparados).
	% impar(N).
	% ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados)
	% agrupar_por_profundidad(ListaPixles,ListaAgrupada)
	% invertir(ListaPixles, ListaPixlesInvertida).
	% imageIsPixmap(Image).
	% imageInvertColorRGB(Pixel,PixelColorSimetrico).
% Metas:
% Primarias
    % imageIsPixmap(Image).
    % imageInvertColorRGB(Pixel,PixelColorSimetrico).
% Secundarias
    % pixrgb(X, Y, R,G,B, Profundidad, Imagen).
    % seleccionar_rgb(ListaPixeles,ListaColores).
    % color_frecuente_rgb(ListaPixeles, R_frecuente, G_frecuente, B_frecuente, ListaPixelesColorFrecuente).
    %  resto_de_colores_rgb(ListaPixeles, R_frecuente, G_frecuente, B_frecuente, ListaPixelesDeColoresNoFrecuentes).
    % seleccionar_profundidad_rgb(ListaProfundidad, ListaProfundidadSinDulplicas).
    % pixeles_a_string_rgb(ListaPixeles, Alto, ListaString).
    % eliminar_pixel_rgb(ListaPixeles, X2,Y2, ListaPixelesSinUnPixel).
    % quitar_primer_pixel(ListaPixeles, ColaListaPixeles). 
    % flipV_AuxRGB(Alto,ListaPixeles, ListaPixelesModificada).
    % flipH_AuxRGB(Alto,ListaPixeles, ListaPixelesModificada).
    % rotate_AuxRGB(Ancho,ListaPixeles, ListaPixelesModificada).
    % agregar_salto(ListaPixeles,Alto,ListaPixelesConSalto).
    % agregar_tab(ListaPixeles,ListaPixelesConTab).
    % ordenar_segun_x(ListaPixeles, Pixeles_ordenados).
    % pixeles_en_blanco_rgb(ListaPixeles, ListaPixelesBlancos).
    % pixeles_primera_posicion_rgb(ListaPixeles,ListaPixeles).
	% separar_capas_repeticion_profundidades_rgb(ListaPixeles,ListaPixelesSeparados)
	% separar_capas_rgb(ListaPixeles, ListaPixelesSeparados).
	% impar(N).
	% ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados)
	% agrupar_por_profundidad(ListaPixles,ListaAgrupada)
	% invertir(ListaPixles, ListaPixlesInvertida).

/*-----------------------------------------------------REPRESENTACION----------------------------------------------------------------------*/
% El TDA PIXRGB representa la unidad inicial de una imagen de tipo PIXMAP expresada con pixeles que pueden
% tomar valores del espectro RGB (donde, Red=Rojo,Green=Verde,Blue=Azul) para representar su color, estos
% valores se mueven entre el 0 y el 255, ademas de incluir la posicion de de cada pixel, representado con
% las letras "x" e "y" y la profundidad de estos.
% Representacion:
% pixrgb-d <- x (int) X y (int) X red X green  X blue X depth

/*-----------------------------------------------------CONSTRUCTORES-------------------------------------------------------------------------*/
/*-------------------------------------- PREDICADO PIXRGB------------------------------------------------------------------------------------*/
% Dominio: 6 numeros de tipo entero
% Descripcion:  Constructor del TDA pixbit, que guarda las posiciones, los colores R,G y B, y la profundidad

pixrgb(X, Y, R,G,B, Profundidad, [X, Y, R,G,B, Profundidad]):-
    % Analiza que lo ingresado corresponda al tipo de elemento solicitado.
    integer(X), integer(Y), integer(R), integer(G), integer(B), integer(Profundidad),
    % Lo ingresado debe cumplir parametros.
    X >= 0, Y >= 0, R >= 0, G >= 0, B >= 0, R =< 255, G =< 255, B =< 255,Profundidad >= 0.

/*-----------------------------------------------------SELECTORES----------------------------------------------------------------------------*/
/*-------------------------------------- PREDICADO SELECCIONAR-RGB---------------------------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: guarda el conjunto de colores de un pixel en una lista. Utilizado en imageToHistogram. 

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
seleccionar_rgb([],[]).    
% Los colores se guardan en la lista de salida.
seleccionar_rgb([[_,_,R,G,B,_]|Cola],[[R,G,B]|ColaResultado]):-
    % Llamada recursiva.
  	seleccionar_rgb(Cola,ColaResultado).
/*-------------------------------------PREDICADO COLOR-FRECUENTE-RGB------------------------------------------------------*/
% Dominio:Lista de pixeles
% Descripcion: Con la informacion del histograma guarda los pixeles que contienen el color mas frecuente en una lista. Utilizado en imageCompress.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
color_frecuente_rgb([], _, _,_,[]).
color_frecuente_rgb([[_,_,R,G,B,_]|Cola], R_frecuente, G_frecuente, B_frecuente, Resultado):- 
    % Se comparan los colores ingresados con los colores de los pixeles.
    R \=  R_frecuente, G \=  G_frecuente, B \=  B_frecuente  ->   
        % Llamada recursiva.
        color_frecuente_rgb(Cola, R_frecuente, G_frecuente, B_frecuente, Resultado). 
% Se guardan los pixeles cuyos colores coinciden con lo ingresado.
color_frecuente_rgb([[X, Y, R,G,B, Profundidad]|Cola], R_frecuente, G_frecuente, B_frecuente, [[X, Y, R,G,B, Profundidad]|Resultado]):- 
    % Llamada recursiva.
    color_frecuente_rgb(Cola, R_frecuente, G_frecuente, B_frecuente, Resultado).

/*-------------------------------------PREDICADO RESTO-DE-COLORES------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Con la informacion del histograma guarda aquellos pixeles que tienen los otros colores no frecuentes en una lista. Utilizado en imageCompress.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
resto_de_colores_rgb([], _,_,_, []).
resto_de_colores_rgb([[_,_,R,G,B,_]|Cola], R_frecuente, G_frecuente, B_frecuente, Resultado):- 
    % Se comparan los colores ingresados con los colores de los pixeles.
    R ==  R_frecuente, G ==  G_frecuente, B ==  B_frecuente  ->   
    % Llamada recursiva.
    resto_de_colores_rgb(Cola,R_frecuente, G_frecuente, B_frecuente, Resultado). 
% Se guardan los pixeles cuyos colores no coinciden con lo ingresado.
resto_de_colores_rgb([[X, Y, R,G,B, Profundidad]|Cola], R_frecuente, G_frecuente, B_frecuente, [[X, Y, R,G,B, Profundidad]|Resultado]):- 
    % Llamada recursiva.
    resto_de_colores_rgb(Cola, R_frecuente, G_frecuente, B_frecuente, Resultado).

/*-------------------------------------PREDICADO SELECCIONAR-PROFUNDIDAD------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Guarda las profundidades de todos los pixeles en una lista y elimina los duplicados. Utilizado en imageDepthLayers.
 
seleccionar_profundidad_rgb(ListaProfundidad, ListaSinDulplicas):-
    % Se genera la lista con todas las profundidades.
    seleccionar_profundidad_rgb_1(ListaProfundidad,Lista),
    % Con sort se ordenan y se eliminan duplicados.
    sort(Lista, ListaSinDulplicas).
% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
seleccionar_profundidad_rgb_1([],[]).    
% Las profundidades de todos los pixeles se guarda en una lista.
seleccionar_profundidad_rgb_1([[_,_,_,_,_,Profundidad]|Cola],[Profundidad|ColaResultado]):-
    % Llamada recursiva.
  	seleccionar_profundidad_rgb_1(Cola,ColaResultado).
     
/*-----------------------------------------------------MODIFICADORES-----------------------------------------------------------------------------------*/
/*-------------------------------------PREDICADO PIXELES-A-STRING--------------------------------------------------------------------------------------*/
% Dominio: Lista de pixeles y un entero que corresponde al valor del alto.
% Descripcion: Recibe una lista de pixeles para luego seleccionar los valores correspondientes al color y los agrega en una lista, que ira en otra lista,
% en esta lista se agrega el simbolo "tab" despues de cada color, luego se comeinza a agregar el simbolo de salto de linea, cada una cierta cantidad de 
% elementos que corresponde al alto, se quitan todos los corchetes que representan sublistas, quedando solo una que contiene toda la informacion y con atomics_to_string
% se convierte a string. Dependidendo de la paridad del Alto, se le suma uno, asi este simbolo no ira en un lugar incorrecto. Utilizado en imageToString.

pixeles_a_string_rgb(ListaPixeles, Alto, ListaString):-
    % Analiza si es par o impar.
    impar(Alto) ->  
        % Obtiene los colores.
    	seleccionar_rgb(ListaPixeles, Pixeles),
        % Inserta la tabulacion.
    	agregar_tab(Pixeles, PixTab),
        % Inserta salto de linea.
   		agregar_salto(PixTab, (Alto+1), PixSalto),
        % Quita sublistas.
    	flatten(PixSalto, PixLista),
        % Convierte a string.
    	atomics_to_string(PixLista, ListaString);
    % Obtiene los colores.
    seleccionar_rgb(ListaPixeles, Pixeles),
    % Inserta la tabulacion.
    agregar_tab(Pixeles, PixTab),
    % Inserta salto de linea.
    agregar_salto(PixTab, (Alto+2), PixSalto),
    % Quita sublistas.
    flatten(PixSalto, PixLista),
     % Convierte a string.
    atomics_to_string(PixLista, ListaString).

/*-------------------------------------PREDICADO ELIMINAR-PIXEL-RGB------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Elimina un pixel de una posicion especifica. Utilizado en imageChangePixel.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
eliminar_pixel_rgb([], _, _,[]).
eliminar_pixel_rgb([[X,Y,_,_,_,_]|Cola], X2,Y2, Resultado):- 
    % Comparacion de posiciones de pixeles con lo ingresado.
    X == X2, Y == Y2 ->   
    % Llamada recursiva.
    eliminar_pixel_rgb(Cola,X2,Y2, Resultado). 
% Guarda los pixeles que no corresponden con lo ingresado.
eliminar_pixel_rgb([[X,Y,R,G,B, Profundidad]|Cola], X2,Y2, [[X, Y, R,G,B, Profundidad]|Resultado]):- 
    % Llamada recursiva.
    eliminar_pixel_rgb(Cola, X2,Y2, Resultado).

/*-------------------------------------PREDICADO AGREGAR-PIXEL-RGB---------------------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Inserta el pixel ingresado en la lista de pixeles. Utilizado en imageChangePixel.

agregar_pixel_rgb(ListaPixeles, [X,Y,R,G,B, Profundidad], ListaPixelesConUnoNuevo):-
    % Elimina el pixel que tiene la misma posicion que el ingresado.
    eliminar_pixel_rgb(ListaPixeles, X,Y, Resultado),
    % Inserta el pixel nuevo.
    append(Resultado, [[X,Y,R,G,B, Profundidad]], Lista),
    % Ordena toda la lista de menor a mayor segun el primer elemento.
    ordenar_segun_x(Lista,ListaPixelesConUnoNuevo).

/*-------------------------------------PREDICADO INSERTAR-PIXELES-BLANCOS-RGB--------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion: inserta los pixeles con los colores blancos en los pixeles de la imagen a la que se le quieren obtener las capas. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
insertar_pixeles_blancos_rgb([],_,[]) .      
insertar_pixeles_blancos_rgb([[X,Y,R,G,B,Profundidad]|Cola],PixelesBlancos,[[[X,Y,R,G,B,Profundidad]|PixelesBlancosConProfundidad]|ColaResultado]):- 
    % La profundidad de los pixeles blancos se reemplaza por la del pixel con color.
    agregar_profundidad_rgb(PixelesBlancos,Profundidad,PixelesBlancosConProfundidad),
    % Llamada recursiva.
    insertar_pixeles_blancos_rgb(Cola,PixelesBlancos,ColaResultado).
 
/*-------------------------------------PREDICADO INSERTAR-PIXELES-BLANCOS-PROFUNDIDAD-REPETIDA-RGB-------------------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion: inserta los pixeles con los colores blancos en los pixeles de la imagen a la que se le quieren obtener las capas, pero que tiene
% profundidades repeteidas. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
insertar_pixeles_blancos_profundidad_repetida_rgb([],_,[]) .      
insertar_pixeles_blancos_profundidad_repetida_rgb([[[X,Y,R,G,B,Profundidad]|ColaGrupo]|Cola] , PixelesBlancos, [ PixelesInsertados |ColaResultado]) :- 
    % La profundidad de los pixeles blancos se reemplaza por la del pixel con color.
    agregar_profundidad_rgb(PixelesBlancos,Profundidad,PixelesConProfundidad),
    % Se insertan en la lista con los otros pixeles
    append([[X,Y,R,G,B,Profundidad]|ColaGrupo],PixelesConProfundidad,PixelesInsertados),
    % Llamada recursiva.
    insertar_pixeles_blancos_profundidad_repetida_rgb(Cola,PixelesBlancos,ColaResultado).
	
/*-------------------------------------PREDICADO AGREGAR-PROFUNDIDAD-RGB-----------------------------------------------------------------------------------------*/
% Dominio:Lista de pixeles Blancos.
% Descripcion: Cambia la profundidad de los pixeles blancos por la del pixel a la que se le insertaran estos. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
agregar_profundidad_rgb([],_,[]).      
% Se realiza el cambio de profundidad.
agregar_profundidad_rgb([[X, Y, R,G,B,_]|Cola],Profundidad,[[X, Y, R,G,B,Profundidad]|ColaResultado]) :- 
    % Llamada recursiva.
    agregar_profundidad_rgb(Cola,Profundidad,ColaResultado).
 
/*-------------------------------------PREDICADO QUITAR-PRIMER-PIXEL------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion:Elimina el primer pixel de una lista de pixeles. Utilizado en imageDepthLayers.
 
quitar_primer_pixel([_|Cola], Cola).    
% Deja solo la cola de la lista.
/*-----------------------------------------------------OTROS PREDICADOS------------------------------------------------------*/
/*-------------------------------------PREDICADO FLIPV_AUXRGB----------------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al alto y una lista de tamaño 7 con la informacion de los  pixeles.
% Descripcion: Función auxiliar que permite invertir una imagen verticalmente,cuando la imagen corresponde a un pixmap. Utilizado en imageFlipV.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
flipV_AuxRGB(_,[], []).
flipV_AuxRGB(Alto,[[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,R1,G1,B1,Profundidad1]|ColaResultado]):-
  	X1 is X ,
    % Se produce el cambio en la posicion Y, invirtiendo su posicion.
    Y1 is (Alto - 1) - Y, 
    R1 is R,
    G1 is G,
    B1 is B,
    Profundidad1 is Profundidad,
    % Llamada recursiva.
    flipV_AuxRGB(Alto, Cola, ColaResultado).

/*-------------------------------------PREDICADO FLIPH_AUXRGB------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al alto y una lista de tamaño 7 con la informacion de los  pixeles
% Descripcion: Función auxiliar que permite invertir una imagen horizotalmente,cuando la imagen corresponde a un pixmap. Utilizado en imageFlipH.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
flipH_AuxRGB(_,[], []).
flipH_AuxRGB(Alto,[[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,R1,G1,B1,Profundidad1]|ColaResultado]):-
  	% Se produce el cambio en la posicion X, invirtiendo su posicion.
    X1 is (Alto - 1) - X ,
    Y1 is Y, 
    R1 is R,
    G1 is G,
    B1 is B,
    Profundidad1 is Profundidad,
     % Llamada recursiva.
    flipH_AuxRGB(Alto, Cola, ColaResultado).

/*-------------------------------------PREDICADO ROTATE-AUX-RGB--------------------------------------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Rota una imagen invirtiendo la posicion de x con y, (la ultima posicion de y es la primera, la penultima la segunda y asi sucesivamente), la
% la posicion Y toma el valor de X. Utilizado en imageRotate90. 

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.    
rotate_AuxRGB(_,[], []).
rotate_AuxRGB(Ancho,[[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,R1,G1,B1,Profundidad1]|ColaResultado]):-
  	% Transformacion del valor de X.
    X1 is (Ancho - 1) - Y,
    % Intercambio de X con Y.
    Y1 is X, 
    R1 is R,
    G1 is G,
    B1 is B,
    Profundidad1 is Profundidad,
    % Llamada recursiva.
    rotate_AuxRGB(Ancho, Cola, ColaResultado). 

/*-------------------------------------PREDICADO AGREGAR-SALTO------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Inserta el simbolo de salto de linea, luego de una cantidad X de elementos, (X corresponde al alto de la imagen), 
% para que cuando se lea hayan saltos entre los elementos. Utilizado en imageToString.

agregar_salto(ListaPixeles,Alto,ListaPixelesConSalto):-
    % Se inserta el salto atraves de recursion.
    agregar_salto(ListaPixeles,Alto,ListaPixelesConSalto,Alto).
% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
agregar_salto([],_,[],_).
% Insersion del string con el simbolo.
agregar_salto([_|Cola],Alto,["\n",ColaResultado],0):- 
    % Llamada recursiva.
    agregar_salto(Cola,Alto,ColaResultado,Alto).
agregar_salto([Cabeza|Cola],Alto,[Cabeza|ColaResultado],Contador):- 
    % Contador para moverse entre los elementos.
    Contador > 0,
    Contador1 is Contador - 1, 
    % Llamada recursiva.
    agregar_salto(Cola,Alto,ColaResultado,Contador1).

/*-------------------------------------PREDICADO AGREGAR-TAB------------------------------------------------------*/
% Dominio:Lista de pixeles
% Descripcion: Inserta el simbolo tab luego de cada elemento de la lista, para que cuando se lea haya un espacio entre estos. Utilizado en imageToString.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
agregar_tab([],[]) .   
% Insersion del string con el simbolo.   
agregar_tab([Cabeza|Cola],[Cabeza,"\t"|ColaResultado]):- 
    % Llamada recursiva.
    agregar_tab(Cola,ColaResultado).

/*-------------------------------------PREDICADO ORDENAR-SEGUN-X ------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Ordena los pixeles de menor a mayor utilizando la posicion x. Utilizado en imageChangePixel.

ordenar_segun_x(ListaPixeles, Pixeles_ordenados):-
    % Ordena de menor a mayor sin eliminar duplicados.
    sort(1, @=<, ListaPixeles, Pixeles_ordenados).

/*-------------------------------------PREDICADO PIXELES-EN-BLANCO------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Duplica una lista entregada pero cambia el valor de los colores por blanco y la profundidad por 0, pero mantiene la posicion de los pixeles.
% Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
pixeles_en_blanco_rgb([],[]).
% Cambia el color por blaco y la profundidad por 0.
pixeles_en_blanco_rgb([[X,Y,_,_,_,_]|Cola], [[X,Y,255,255,255,0]|ColaResultado]):-
    % Llamada recursiva.
    pixeles_en_blanco_rgb(Cola,ColaResultado).

/*-------------------------------------PREDICADO PIXELES-PRIMERA-POSCION-RGB------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion: Deja en primera posicion todos los pixeles de la imagen a la que se le crearan las capas. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.  
pixeles_primera_posicion_rgb([],[]).
% Pone a todos los pixeles de la lista en primera posicion.
pixeles_primera_posicion_rgb([[_,_,R,G,B,Profundidad]|Cola], [[0,0,R,G,B,Profundidad]|ColaResultado]):-
    % Llamada recursiva.
    pixeles_primera_posicion_rgb(Cola,ColaResultado).

/*-------------------------------------PREDICADO SEPARAR-CAPAS-REPETICION-PROFUNDIDADES-RGB------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Cuando una imagen tiene mas de una profundidad repetida, estas se agrupan en listas luego de ser ordenadas, para luego agregar los pixeles blancos 
% a los que se les elimino la primera posicion. Utilizado en imageDepthLayers. 
 
separar_capas_repeticion_profundidades_rgb(ListaPixeles,ListaPixelesSeparados):-
    % Los pixeles se ordenan segun profundidad.
    ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados),
    % Los pixeles se agrupan segun profundidad.
    agrupar_por_profundidad(ListaPixelesOrdenados,ListaPixelesAgrupados),
    % Se crean los pixeles blancos.
	pixeles_en_blanco_rgb(ListaPixeles,ListaPixelesBlancos),
    % Se le quita el pixel de la posicion 0,0 a los pixeles blancos.
	quitar_primer_pixel(ListaPixelesBlancos,ListaPixelesBlancos2),
    % A cada profundidad de la imagen se le insertan los pixeles blancos.
	insertar_pixeles_blancos_profundidad_repetida_rgb(ListaPixelesAgrupados,ListaPixelesBlancos2,ListaPixelesSeparados),!.
 
/*-------------------------------------PREDICADO SEPARAR-CAPAS-RGB------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Cuando todos los pixeles tienen profundidades distintas, se le agrega la lista de pixles blancos sin el primer elemento a cada uno de estos
% pero su posicion original se cambia por 0.0. Utilizado en imageDepthLayers. 
 
separar_capas_rgb(Lista, Lista2):-
    % Pone a todos los pixeles de la lista en primera posicion.
    pixeles_primera_posicion_rgb(Lista,T),
    % Se crean los pixeles blancos.
    pixeles_en_blanco_rgb(Lista,R),
    % Se le quita el pixel de la posicion 0,0 a los pixeles blancos.
    quitar_primer_pixel(R,R2),
    % A cada profundidad de la imagen se le insertan los pixeles blancos.
    insertar_pixeles_blancos_rgb(T,R2,Lista2),!.

/*-------------------------------------PREDICADO IMPAR------------------------------------------------------*/
% Dominio: Entero, Alto de la imagen
% Recorrido: Boolean
% Descripcion: Ve si el valor del alto de la imagen corresponde a un numero impar o no.

impar(N):- 
    mod(N,2) =:= 0.

/*-------------------------------------PREDICADO PROFUNDIDAD------------------------------------------------------*/
% Dominio:Lista de pixeles
% Descripcion: Ordena los pixeles de menor a mayor segun la profundidad del pixel. Utilizado en imageDepthLayers. 
 
ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados):-
    % La lista que contiene la informacion de un pixel se invierte para que la profundidad quede primero.
    invertir(ListaPixeles,ListaPixelesInvertidos),
    % Se ordena segun profundidad.
	sort(0, @=<, ListaPixelesInvertidos, ListaPixelesOrdenadosInvertidos),
    % La posicion original de los elementos se restaura.
	invertir(ListaPixelesOrdenadosInvertidos,ListaPixelesOrdenados).
 
/*-------------------------------------PREDICADO AGRUPAR-POR-PROFUNDIDAD------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Agrupa los pixeles que tienen la misma profundidad. Utilizado en imageDepthLayers. 
 
agrupar_por_profundidad(ListaPixles,ListaAgrupada):-
    % Llamado al predicado para agrupar los pixeles.
    agrupar(ListaPixles,[],ListaAgrupada).
% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
agrupar([],[],[]).
agrupar([ListaPixel],ListaPixeles,[PixelesConMismaProfundidad]):- 
    % Cuando hay un solo elemento en la lista este se inserta en la lista final.
    append([ListaPixel],ListaPixeles,PixelesConMismaProfundidad).
% Se comparan pixeles contiguos.
agrupar([ListaPixel,ListaPixel1|Cola],ListaPixeles,PixelesConDistintaProfundidadLista):- 
    % A cada uno de estos pixeles se les saca la profundidad.
    last(ListaPixel,Profundidad),  
    last(ListaPixel1,Profundidad1),
    % Se comparan profundidades.
    Profundidad== Profundidad1, 
    % Si son no iguales se quedan en la lista sin cambios.
    append([ListaPixel],ListaPixeles,PixelesConDistintaProfundidad1) , 
    % Llamada recursiva.
   	agrupar([ListaPixel1|Cola],PixelesConDistintaProfundidad1,PixelesConDistintaProfundidadLista),!.
% Se comparan pixeles contiguos.
agrupar([ListaPixel,ListaPixel1|Cola],ListaPixeles,[PixelesConMismaProfundidad1|ColaResultado]):- 
    % A cada uno de estos pixeles se les saca la profundidad.
    last(ListaPixel,Profundidad),  
    last(ListaPixel1,Profundidad1),
    % Se comparan profundidades.
    Profundidad\=Profundidad1 ,
    % Si son iguales se agregan en la lista.
    append([ListaPixel],ListaPixeles,PixelesConMismaProfundidad1) ,
    % Llamada recursiva.
 	agrupar([ListaPixel1|Cola],[],ColaResultado),!.

/*-------------------------------------PREDICADO INVERTIR------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Invierte el orden de los pixeles, dejando la profundidad al principio. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
invertir([],[]).
invertir([Cabeza| Cola], [CabezaInvertida|ColaResultado]):-   
    % Se invierten los elementos del pixel
    reverse(Cabeza,CabezaInvertida),
    % Llamada recursiva.
    invertir(Cola,ColaResultado).
    
/*--------------------------------SEGUNDA PARTE DEL ARCHIVO------------------------------------------------*/

/*              AQUI SE ENCUENTRAN LOS PREDICADOS REQUERIDOS EN EL LABORATORIO                             */

/* --------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------------PREDICADOS DE PERTENENCIA---------------------------*/
/*-------------------------------------- PREDICADO IMAGE-IS-PIXMAP------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido: Boolean
% Descripcion: Si es una imagen del tipo pixmap retorna true, sino retorna false.

imageIsPixmap([_,_, [[_,_,R,G,B,_]|_]]) :-
    % Los colores deben cumplir parametros.
    R >= 0, G >= 0, B >= 0, R =< 255, G =< 255, B =< 255 -> 
    % Retorno del resultado.
    writeln('#t').

/*-------------------------------------PREDICADO IMAGE-INVERT-COLOR-RGB------------------------------------------------------*/
% Dominio: Una lista que contiene la informacion de un pixel.
% Descripcion: Cambia el color del pixel ingresado por su color simetrico. Para producir este cambio cada color se resta por 255.

imageInvertColorRGB([X,Y,R,G,B,Profundidad],[X,Y,R2,G2,B2,Profundidad]):-
    R2 is 255- R,
    G2 is 255- G,
    B2 is 255- B.