% Predicado para exportar los predicados a otros archivos.
:- module(tda_pixrgb_20244128_JofreLeon,[pixrgb/7,imageIsPixmap/1,seleccionar_rgb/2,pixeles_a_string_rgb/3,color_frecuente_rgb/5,resto_de_colores_rgb/5,eliminar_pixel_rgb/4,agregar_pixel_rgb/3,
                                        flipV_AuxRGB/3,flipH_AuxRGB/3,rotate_AuxRGB/3,imageInvertColorRGB/2,seleccionar_profundidad_rgb/2,pixeles_en_blanco_rgb/2,pixeles_primera_posicion_rgb/2,agregar_profundidad_rgb/3,
                                        insertar_pixeles_blancos_rgb/3,insertar_pixeles_blancos_profundidad_repetida_rgb/3,separar_capas_repeticion_profundidades_rgb/2,separar_capas_rgb/2]).


/*-----------------------------------------------------TDA PIXRGB-------------------------------------------
Este archivo corresponde al TDA pixrgb, se encuentran todos los predicados necesarios para su creacion y tambien
los solicitados en el proyecto de laboratorio.
Por motivos de organizacion se realizo la division del archivo en dos partes principales, la primera corresponde
a aquellos predicados cuyas metas son secundarias por lo que aportan en el funcionamiento de los predicados solicitados
y la segunda donde se encuentran todos los predicados del laboratorio.


PRIMERA PARTE:  PREDICADOS AUXILIARES O PREDICADOS CON FUNCIONES ESPECIFICAS                                  */

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/
% El TDA PIXRGB representa la unidad inicial de una imagen de tipo PIXMAP expresada con pixeles que pueden
% tomar valores del espectro RGB (donde, Red=Rojo,Green=Verde,Blue=Azul) para representar su color, estos
% valores se mueven entre el 0 y el 255, ademas de incluir la posicion de de cada pixel, representado con
% las letras "x" e "y" y la profundidad de estos.
% Representacion:
% pixrgb-d <- x (int) X y (int) X red X green  X blue X depth

/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*-------------------------------------- PREDICADO PIXRGB------------------------------------------------------*/
% Dominio: 6 numeros de tipo entero
% Descripcion:  PREDICADO constructora del TDA pixbit, que guarda las posiciones, los colores R,G y B, y la profundidad

pixrgb(X, Y, R,G,B, Profundidad, [X, Y, R,G,B, Profundidad]):-
    integer(X), integer(Y), integer(R), integer(G), integer(B), integer(Profundidad),
    X >= 0, Y >= 0, R >= 0, G >= 0, B >= 0, R =< 255, G =< 255, B =< 255,Profundidad >= 0.

/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*-------------------------------------- PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion: guarda los colores en una lista 

seleccionar_rgb([],[]).    
seleccionar_rgb([[_,_,R,G,B,_]|Cola],[[R,G,B]|ColaResultado]):-
  	seleccionar_rgb(Cola,ColaResultado).

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

pixeles_a_string_rgb(ListaPixeles, Alto, ListaString):-
    impar(Alto) ->  
    	seleccionar_rgb(ListaPixeles, Pixeles),
    	agregar_tab(Pixeles, PixTab),
   		agregar_salto(PixTab, (Alto+1), PixSalto),
    	flatten(PixSalto, PixLista),
    	atomics_to_string(PixLista, ListaString);
    seleccionar_rgb(ListaPixeles, Pixeles),
    agregar_tab(Pixeles, PixTab),
    agregar_salto(PixTab, Alto, PixSalto),
    flatten(PixSalto, PixLista),
    atomics_to_string(PixLista, ListaString).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

color_frecuente_rgb([], _, _,_,[]).
color_frecuente_rgb([[_,_,R,G,B,_]|Cola], R_frecuente, G_frecuente, B_frecuente, Resultado):- 
    R \=  R_frecuente, G \=  G_frecuente, B \=  B_frecuente  ->   
        color_frecuente_rgb(Cola, R_frecuente, G_frecuente, B_frecuente, Resultado). 
color_frecuente_rgb([[X, Y, R,G,B, Profundidad]|Cola], R_frecuente, G_frecuente, B_frecuente, [[X, Y, R,G,B, Profundidad]|Resultado]):- 
    color_frecuente_rgb(Cola, R_frecuente, G_frecuente, B_frecuente, Resultado).


/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

resto_de_colores_rgb([], _,_,_, []).
resto_de_colores_rgb([[_,_,R,G,B,_]|Cola], R_frecuente, G_frecuente, B_frecuente, Resultado):- 
   R ==  R_frecuente, G ==  G_frecuente, B ==  B_frecuente  ->   
   resto_de_colores_rgb(Cola,R_frecuente, G_frecuente, B_frecuente, Resultado). 
resto_de_colores_rgb([[X, Y, R,G,B, Profundidad]|Cola], R_frecuente, G_frecuente, B_frecuente, [[X, Y, R,G,B, Profundidad]|Resultado]):- 
    resto_de_colores_rgb(Cola, R_frecuente, G_frecuente, B_frecuente, Resultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

eliminar_pixel_rgb([], _, _,[]).
eliminar_pixel_rgb([[X,Y,_,_,_,_]|Cola], X2,Y2, Resultado):- 
   X == X2, Y == Y2 ->   
   eliminar_pixel_rgb(Cola,X2,Y2, Resultado). 
eliminar_pixel_rgb([[X,Y,R,G,B, Profundidad]|Cola], X2,Y2, [[X, Y, R,G,B, Profundidad]|Resultado]):- 
    eliminar_pixel_rgb(Cola, X2,Y2, Resultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

agregar_pixel_rgb(ListaPixeles, [X,Y,R,G,B, Profundidad], Image2):-
    eliminar_pixel_rgb(ListaPixeles, X,Y, Resultado),
    append(Resultado, [[X,Y,R,G,B, Profundidad]], Imag),
    ordenar_segun_x(Imag,Image2).

/*-------------------------------------PREDICADO FLIPV_AUXRGB------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al alto y una lista de tamaño 7 con la informacion de los  pixeles

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

/*-------------------------------------PREDICADO FLIPH_AUXRGB------------------------------------------------------*/
% Dominio: dato tipo int que corresponde al alto y una lista de tamaño 7 con la informacion de los  pixeles

% Descripcion: Función auxiliar que permite invertir una imagen horizotalmente,cuando la imagen corresponde a un pixmap.
% Tipo de Meta: Secundaria.

flipH_AuxRGB(_,[], []).
flipH_AuxRGB(Alto,[[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,R1,G1,B1,Profundidad1]|ColaResultado]):-
  	X1 is (Alto - 1) - X ,
    Y1 is Y, 
    R1 is R,
    G1 is G,
    B1 is B,
    Profundidad1 is Profundidad,
    flipH_AuxRGB(Alto, Cola, ColaResultado).
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
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

/*-------------------------------------PREDICADO ORDENAR-SEGUN-X ------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Ordena los pixeles de menor a mayor utilizando la posicion x.

ordenar_segun_x(Lista_pixeles, Pixeles_ordenados):-
    sort(1, @=<, Lista_pixeles, Pixeles_ordenados).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
seleccionar_profundidad_rgb(ListaProfundidad, ListaSinDulplicas):-
    seleccionar_profundidad_rgb_1(ListaProfundidad,Lista),
    sort(Lista, ListaSinDulplicas).
seleccionar_profundidad_rgb_1([],[]).    
seleccionar_profundidad_rgb_1([[_,_,_,_,_,Profundidad]|Cola],[Profundidad|ColaResultado]):-
  	seleccionar_profundidad_rgb_1(Cola,ColaResultado).
 
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
pixeles_en_blanco_rgb([],[]).
pixeles_en_blanco_rgb([[X,Y,_,_,_,_]|Cola], [[X,Y,255,255,255,0]|Cabeza]):-
    pixeles_en_blanco_rgb(Cola,Cabeza).

 /*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
pixeles_primera_posicion_rgb([],[]).
pixeles_primera_posicion_rgb([[_,_,R,G,B,Profundidad]|Cola], [[0,0,R,G,B,Profundidad]|Cabeza]):-
    pixeles_primera_posicion_rgb(Cola,Cabeza).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
agregar_profundidad_rgb([],_,[]).      
agregar_profundidad_rgb([[X, Y, R,G,B,_]|Cola],Profundidad,[[X, Y, R,G,B,Profundidad]|ColaResultado]) :- 
    agregar_profundidad_rgb(Cola,Profundidad,ColaResultado).
 
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
insertar_pixeles_blancos_rgb([],_,[]) .      
insertar_pixeles_blancos_rgb([[X,Y,R,G,B,Profundidad]|Cola],PixelesBlancos,[[[X,Y,R,G,B,Profundidad]|PixelesBlancosConProfundidad]|ColaResultado]):- 
    agregar_profundidad_rgb(PixelesBlancos,Profundidad,PixelesBlancosConProfundidad),
    insertar_pixeles_blancos_rgb(Cola,PixelesBlancos,ColaResultado).
 
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
insertar_pixeles_blancos_profundidad_repetida_rgb( [],_,[]) .      
insertar_pixeles_blancos_profundidad_repetida_rgb( [[[X,Y,R,G,B,Profundidad]|ColaGrupo]|Cola] , PixelesBlancos, [ PixelesInsertados |ColaResultado] ) :- 
    agregar_profundidad_rgb(PixelesBlancos,Profundidad,PixelesConProfundidad),
    append([[X,Y,R,G,B,Profundidad]|ColaGrupo],PixelesConProfundidad,PixelesInsertados),
    insertar_pixeles_blancos_profundidad_repetida_rgb(Cola,PixelesBlancos,ColaResultado).
 
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 

separar_capas_repeticion_profundidades_rgb(ListaPixeles,ListaPixelesSeparados):-
    ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados),
    agrupar_por_profundidad(ListaPixelesOrdenados,ListaPixelesAgrupados),
	pixeles_en_blanco_rgb(ListaPixeles,ListaPixelesBlancos),
	quitar_primer_pixel(ListaPixelesBlancos,ListaPixelesBlancos2),
	insertar_pixeles_blancos_profundidad_repetida_rgb(ListaPixelesAgrupados,ListaPixelesBlancos2,ListaPixelesSeparados),!.
 
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
separar_capas_rgb(Lista, Lista2):-
    pixeles_primera_posicion_rgb(Lista,T),
    pixeles_en_blanco_rgb(Lista,R),
    quitar_primer_pixel(R,R2),
    insertar_pixeles_blancos_rgb(T,R2,Lista2),!.

/*-------------------------------------PREDICADO IMPAR------------------------------------------------------*/
% Dominio: Entero, Alto de la imagen
% Recorrido: Boolean
% Descripcion: Ve si el valor del alto de la imagen corresponde a un numero impar o no.

impar(N):- 
    mod(N,2) =:= 0.

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

/*-------------------------------------PREDICADO INVERTIR------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Invierte el orden de los pixeles, dejando la profundidad al principio.
 
invertir([],[]).
invertir([Cabeza| Cola], [CabezaInvertida|ColaResultado]):-   
    reverse(Cabeza,CabezaInvertida),
    invertir(Cola,ColaResultado).
    
/*-------------------------------------PREDICADO QUITAR-PRIMER-PIXEL------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion:Elimina el primer pixel de una lista de pixeles.
 
quitar_primer_pixel([_|Tail], Tail).

/*--------------------------------SEGUNDA PARTE DEL ARCHIVO------------------------------------------------*/

/*              AQUI SE ENCUENTRAN LOS PREDICADOS REQUERIDOS EN EL LABORATORIO                             */

/* --------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------------PREDICADOS DE PERTENENCIA---------------------------*/
/*-------------------------------------- PREDICADO IMAGE-IS-PIXMAP------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido: Boolean
% Descripcion: Si es una imagen del tipo pixmap retorna true (muestra la imagen), sino retorna false

imageIsPixmap([_,_, [[_,_,R,G,B,_]|_]]) :-
    R >= 0, G >= 0, B >= 0, R =< 255, G =< 255, B =< 255 -> 
    writeln('#t');
    writeln('#f').

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:  
imageInvertColorRGB([X,Y,R,G,B,Profundidad],[X,Y,R2,G2,B2,Profundidad]):-
    R2 is 255- R,
    G2 is 255- G,
    B2 is 255- B.