% Predicado para exportar los predicados a otros archivos.
:- module(tda_pixbit_20244128_JofreLeon,[pixbit/5, imageIsBitmap/1,seleccionar_bit/2,pixeles_a_string_bit/3,seleccionar_profundidad_bit/2,pixeles_en_blanco_bit/2,pixeles_primera_posicion_bit/2,
                                        agregar_profundidad_bit/3,insertar_pixeles_blancos_bit/3,insertar_pixeles_blancos_profundidad_repetida_bit/3,separar_capas_repeticion_profundidades_bit/2,
                                        separar_capas_bit/2]).

/*-----------------------------------------------------TDA PIXBIT-------------------------------------------
Este archivo corresponde al TDA pixrbit, se encuentran todos los predicados necesarios para su creacion y tambien
los solicitados en el proyecto de laboratorio.
Por motivos de organizacion se realizo la division del archivo en dos partes principales, la primera corresponde
a aquellos predicados cuyas metas son secundarias por lo que aportan en el funcionamiento de los predicados solicitados
y la segunda donde se encuentran todos los predicados del laboratorio.


PRIMERA PARTE:  PREDICADOS AUXILIARES O PREDICADOS CON FUNCIONES ESPECIFICAS                                  */

/*-----------------------------------------------------REPRESENTACION--------------------------------------*/
% El TDA PIXBIT representa la formacion de pixeles que pueden tomar valores entre 1 y 0,ademas de incluir la
% posicion de cada pixel, representado con las letras "x" e "y", tal como un plano cartesiano y la
% profundidad de estos, ademas de ser la unidad fundamental de una imagen llamada bitmap-d. 
% Representacion:
% pixbit-d <- x (int) X y (int) X bit ([0|1]) X depth (int))

/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*--------------------------------------PREDICADO PIXBIT------------------------------------------------------*/
% Dominio: 4 numeros de tipo entero
% Descripcion: PREDICADO constructora del TDA pixbit, que guarda las posiciones, el bit y la profundidad

pixbit(X, Y, Bit, Profundidad, [X, Y, Bit, Profundidad]):-
    integer(X), integer(Y), integer(Bit), integer(Profundidad),
    X >= 0, Y >= 0, Bit>= 0, Bit =< 1, Profundidad >= 0.

/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion: Pone todos los bits en una lista

seleccionar_bit([],[]).    
seleccionar_bit([[_,_,Bit,_]|Cola],[Bit|ColaResultado]):-
  	seleccionar_bit(Cola,ColaResultado).

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/

/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

pixeles_a_string_bit(ListaPixeles, Alto, ListaString):-
    impar(Alto) ->  
    	seleccionar_bit(ListaPixeles, Pixeles),
   		agregar_tab(Pixeles, PixTab),
    	agregar_salto(PixTab, (Alto + 1) , PixSalto),
    	flatten(PixSalto, PixLista),
    	atomics_to_string(PixLista, ListaString);
    seleccionar_bit(ListaPixeles, Pixeles),
   	agregar_tab(Pixeles, PixTab),
    agregar_salto(PixTab, Alto , PixSalto),
    flatten(PixSalto, PixLista),
    atomics_to_string(PixLista, ListaString).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
seleccionar_profundidad_bit(ListaProfundidad, ListaSinDulplicas):-
    seleccionar_profundidad_bit_1(ListaProfundidad,Lista),
    sort(Lista, ListaSinDulplicas).
seleccionar_profundidad_bit_1([],[]).    
seleccionar_profundidad_bit_1([[_,_,_,Profundidad]|Cola],[Profundidad|ColaResultado]):-
  	seleccionar_profundidad_bit_1(Cola,ColaResultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
pixeles_en_blanco_bit([],[]).
pixeles_en_blanco_bit([[X,Y,_,_]|Cola], [[X,Y,1,0]|Cabeza]):-
    pixeles_en_blanco_bit(Cola,Cabeza).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
pixeles_primera_posicion_bit([],[]).
pixeles_primera_posicion_bit([[_,_,Bit,Profundidad]|Cola], [[0,0,Bit,Profundidad]|Cabeza]):-
    pixeles_primera_posicion_bit(Cola,Cabeza).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
agregar_profundidad_bit([],_,[]).      
agregar_profundidad_bit([[X, Y,Bit ,_]|Cola],Profundidad,[[X, Y, Bit,Profundidad]|ColaResultado]) :- 
      agregar_profundidad_bit(Cola,Profundidad,ColaResultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
insertar_pixeles_blancos_bit([],_,[]) .      
insertar_pixeles_blancos_bit([[X,Y,Bit ,Profundidad]|Cola],PixelesBlancos,[[[X,Y,Bit ,Profundidad]|PixelesBlancosConProfundidad]|ColaResultado]):- 
    agregar_profundidad_bit(PixelesBlancos,Profundidad,PixelesBlancosConProfundidad),
    insertar_pixeles_blancos_bit(Cola,PixelesBlancos,ColaResultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
insertar_pixeles_blancos_profundidad_repetida_bit( [],_,[]) .      
insertar_pixeles_blancos_profundidad_repetida_bit( [[[X,Y,Bit ,Profundidad]|ColaGrupo]|Cola] , PixelesBlancos, [ PixelesInsertados |ColaResultado] ) :- 
    agregar_profundidad_bit(PixelesBlancos,Profundidad,PixelesConProfundidad),
    append([[X,Y,Bit,Profundidad]|ColaGrupo],PixelesConProfundidad,PixelesInsertados),
   insertar_pixeles_blancos_profundidad_repetida_bit(Cola,PixelesBlancos,ColaResultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
separar_capas_repeticion_profundidades_bit(ListaPixeles,ListaPixelesSeparados):-
    ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados),
    agrupar_por_profundidad(ListaPixelesOrdenados,ListaPixelesAgrupados),
	pixeles_en_blanco_bit(ListaPixeles,ListaPixelesBlancos),
	quitar_primer_pixel(ListaPixelesBlancos,ListaPixelesBlancos2),
	insertar_pixeles_blancos_profundidad_repetida_bit(ListaPixelesAgrupados,ListaPixelesBlancos2,ListaPixelesSeparados),!.

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
separar_capas_bit(Lista, Lista2):-
    pixeles_primera_posicion_bit(Lista,T),
    pixeles_en_blanco_bit(Lista,R),
    quitar_primer_pixel(R,R2),
    insertar_pixeles_blancos_bit(T,R2,Lista2),!.

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
/*--------------------------------------PREDICADO IMAGE-IS-BITMAP------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido: Boolean
% Descripcion: Si es una imagen del tipo bitxmap retorna true (muestra la imagen), sino retorna false

imageIsBitmap([_,_, [[_,_, Bit,_]|_]]) :-
    integer(Bit),
    Bit >= 0, Bit < 2-> 
    writeln('#t').
