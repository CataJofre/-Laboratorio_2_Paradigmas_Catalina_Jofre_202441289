:- module(tda_pixhex_20244128_JofreLeon,[pixhex/5, imageIsHexmap/1, rgb_hex/2,seleccionar_hex/2,pixeles_a_string_hex/3,seleccionar_profundidad_hex/2,pixeles_en_blanco_hex/2,pixeles_primera_posicion_hex/2,
                                        agregar_profundidad_hex/3,insertar_pixeles_blancos_hex/3,insertar_pixeles_blancos_profundidad_repetida_hex/3,separar_capas_repeticion_profundidades_hex/2,separar_capas_hex/2]).
/*-----------------------------------------------------TDA PIXHEX-------------------------------------------
Este archivo corresponde al TDA pixrhex, se encuentran todos los predicados necesarios para su creacion y tambien
los solicitados en el proyecto de laboratorio.
Por motivos de organizacion se realizo la division del archivo en dos partes principales, la primera corresponde
a aquellos predicados cuyas metas son secundarias por lo que aportan en el funcionamiento de los predicados solicitados
y la segunda donde se encuentran todos los predicados del laboratorio.


PRIMERA PARTE:  PREDICADOS AUXILIARES O PREDICADOS CON FUNCIONES ESPECIFICAS                                  */
/*-----------------------------------------------------REPRESENTACION--------------------------------------*/
% El TDA PIXHEX representa la unidad inicial de una imagen tipo HEXMAP  cuyos colores se representan con un
%  unico valor escrito en forma hexadecimal, ademas de incluir la posicion de de cada pixel,representado
%  con las letras "x" e "y", tal como un plano cartesiano y la profundidad de estos.
% Representacion:
%  pixhex-d <- x (int) X y (int) X hex(String) X depth

% color(Numero de color RGB, HEX)
color(0.0,"0").
color(1.0,"1").
color(2.0,"2").
color(3.0,"3").
color(4.0,"4").
color(5.0,"5").
color(6.0,"6").
color(7.0,"7").
color(8.0,"8").
color(9.0,"9").
color(10.0,"A").
color(11.0,"B").
color(12.0,"C").
color(13.0,"D").
color(14.0,"E").
color(15.0,"F").

/*-----------------------------------------------------CONSTRUCTORES----------------------------------------*/
/*--------------------------------------PREDICADO PIXHEX ------------------------------------------------------*/
% Dominio: 3 numeros de tipo entero y un elemento de tipo string
% Descripcion: PREDICADO constructora del TDA pixhex, que guarda las posiciones, el hex y la profundidad

pixhex(X, Y, Hex, Profundidad, [X, Y, Hex, Profundidad]):-
    integer(X), integer(Y), string(Hex), integer(Profundidad),
    X >= 0, Y >= 0,  Profundidad >= 0.


/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion: todos los colores hex en una lista 

seleccionar_hex([],[]).    
seleccionar_hex([[_,_,Hex,_]|Cola],[Hex|ColaResultado]):-
  	seleccionar_hex(Cola,ColaResultado).

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion:

/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/
/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion:

parte_decimal(Dividendo, Resultado):-
 	A is Dividendo/16,
   floor(A, B),
    Resultado is (A- B) *16 .

/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion:

parte_entera(Dividendo, Resultado):-
    floor((Dividendo/16), Res),
    Resultado is Res * 1.0.

/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion:

obtener_hex_por_decimal(Numero, Hex):-
   parte_decimal(Numero,R),
	color(R,Hex).


/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion:

obtener_hex_por_entero(Numero, Hex):-
   parte_entera(Numero,R),
	color(R,Hex).


/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion:


hex(Numero,Hex):-
    obtener_hex_por_entero(Numero, H2),
    obtener_hex_por_decimal(Numero, H1), 
    string_concat(H2, H1, Hex).

/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion:

/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion:


pixRGB_a_pixHex( [_,_,R,G,B,_], PixelHex):-
    hex(B, HexB),
    hex(G, HexG),
    string_concat(HexG,HexB, Res),
    hex(R, HexR),
    string_concat(HexR,Res, PixelHex).
   
/*--------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:

% Descripcion:

rgb_hex([], []).
rgb_hex([[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,Hex,Profundidad1]|ColaResultado]):-
  	X1 is X ,
    Y1 is Y, 
    pixRGB_a_pixHex( [_,_,R,G,B,_], Hex),
    Profundidad1 is Profundidad,
    rgb_hex(Cola, ColaResultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

pixeles_a_string_hex(ListaPixeles, Alto, ListaString):-
    impar(Alto) ->  
    	seleccionar_hex(ListaPixeles, Pixeles),
    	agregar_tab(Pixeles, PixTab),
    	agregar_salto(PixTab, (Alto+1), PixSalto),
    	flatten(PixSalto, PixLista),
    	atomics_to_string(PixLista, ListaString);
    seleccionar_hex(ListaPixeles, Pixeles),
    agregar_tab(Pixeles, PixTab),
    agregar_salto(PixTab, Alto, PixSalto),
    flatten(PixSalto, PixLista),
    atomics_to_string(PixLista, ListaString).
    

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
seleccionar_profundidad_hex(ListaProfundidad, ListaSinDulplicas):-
    seleccionar_profundidad_hex_1(ListaProfundidad,Lista),
    sort(Lista, ListaSinDulplicas).
seleccionar_profundidad_hex_1([],[]).    
seleccionar_profundidad_hex_1([[_,_,_,Profundidad]|Cola],[Profundidad|ColaResultado]):-
  	seleccionar_profundidad_hex_1(Cola,ColaResultado).

 

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
pixeles_en_blanco_hex([],[]).
pixeles_en_blanco_hex([[X,Y,_,_]|Cola], [[X,Y,"#FFFFFF",0]|Cabeza]):-
    pixeles_en_blanco_hex(Cola,Cabeza).
 



/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
pixeles_primera_posicion_hex([],[]).
pixeles_primera_posicion_hex([[_,_,Hex,Profundidad]|Cola], [[0,0,Hex,Profundidad]|Cabeza]):-
 pixeles_primera_posicion_hex(Cola,Cabeza).


/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
agregar_profundidad_hex([],_,[]).      
agregar_profundidad_hex([[X, Y, Hex,_]|Cola],Profundidad,[[X, Y, Hex,Profundidad]|ColaResultado]) :- 
      agregar_profundidad_hex(Cola,Profundidad,ColaResultado).
 

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
insertar_pixeles_blancos_hex([],_,[]) .      
insertar_pixeles_blancos_hex([[X,Y,Hex ,Profundidad]|Cola],PixelesBlancos,[[[X,Y,Hex ,Profundidad]|PixelesBlancosConProfundidad]|ColaResultado]):- 
    agregar_profundidad_hex(PixelesBlancos,Profundidad,PixelesBlancosConProfundidad),
    insertar_pixeles_blancos_hex(Cola,PixelesBlancos,ColaResultado).

/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
insertar_pixeles_blancos_profundidad_repetida_hex( [],_,[]) .      
insertar_pixeles_blancos_profundidad_repetida_hex( [[[X,Y,Hex ,Profundidad]|ColaGrupo]|Cola] , PixelesBlancos, [ PixelesInsertados |ColaResultado] ) :- 
    agregar_profundidad_hex(PixelesBlancos,Profundidad,PixelesConProfundidad),
    append([[X,Y, Hex,Profundidad]|ColaGrupo],PixelesConProfundidad,PixelesInsertados),
   insertar_pixeles_blancos_profundidad_repetida_hex(Cola,PixelesBlancos,ColaResultado).
 

 /*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:
 
separar_capas_repeticion_profundidades_hex(ListaPixeles,ListaPixelesSeparados):-
     ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados),
    agrupar_por_profundidad(ListaPixelesOrdenados,ListaPixelesAgrupados),
	pixeles_en_blanco_hex(ListaPixeles,ListaPixelesBlancos),
	quitar_primer_pixel(ListaPixelesBlancos,ListaPixelesBlancos2),
	insertar_pixeles_blancos_profundidad_repetida_hex(ListaPixelesAgrupados,ListaPixelesBlancos2,ListaPixelesSeparados),!.
/*-------------------------------------PREDICADO ------------------------------------------------------*/
% Dominio:
% Descripcion:

separar_capas_hex(Lista, Lista2):-
    pixeles_primera_posicion_hex(Lista,T),
    pixeles_en_blanco_hex(Lista,R),
    quitar_primer_pixel(R,R2),
    insertar_pixeles_blancos_hex(T,R2,Lista2),!.

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
/*--------------------------------------PREDICADO IMAGE-IS-HEXMAP------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido: Boolean
% Descripcion: Si es una imagen del tipo hex xmap retorna true (muestra la imagen), sino retorna false

imageIsHexmap([_,_, [[_,_, Hex,_]|_]]) :-
   string(Hex)-> 
    writeln('#t');
    writeln('#f').








