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
% INFORMACION IMPORTANTE.

% Dominios:
    % TDA pixrgb
    % Posiciones X,Y
    % Bit
    % Lista de pixeles.
    % Alto
    % Ancho
    % Imagen

% Predicados:
    % pixbitX, Y, Bit, Profundidad, Imagen).
    % seleccionar_bit(ListaPixeles,ListaColores).
    % seleccionar_profundidad_bit(ListaProfundidad, ListaProfundidadSinDulplicas).
    % pixeles_a_string_bit(ListaPixeles, Alto, ListaString).
    % insertar_pixeles_blancos_bit(ListaPixeles,PixelesBlancos,ListaPixelesConPixelesBlancos).
	% insertar_pixeles_blancos_profundidad_repetida_bit(ListaPixeles,PixelesBlancos,ListaPixelesConPixelesBlancos).
	% agregar_profundidad_bit(PixelesBlancos,Profundidad,PixelesBlancosConNuevaProfundidad).
	% quitar_primer_pixel(ListaPixeles, ColaListaPixeles). 
    % agregar_salto(ListaPixeles,Alto,ListaPixelesConSalto).
    % agregar_tab(ListaPixeles,ListaPixelesConTab).
    % pixeles_en_blanco_bit(ListaPixeles, ListaPixelesBlancos).
    % pixeles_primera_posicion_bit(ListaPixeles,ListaPixeles).
	% separar_capas_repeticion_profundidades_bit(ListaPixeles,ListaPixelesSeparados)
	% separar_capas_bit(ListaPixeles, ListaPixelesSeparados).
	% impar(N).
	% ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados)
	% agrupar_por_profundidad(ListaPixles,ListaAgrupada)
	% invertir(ListaPixles, ListaPixlesInvertida).
	% imageIsBitmap(Image).
% Metas:
% Primarias
    % imageIsBitmap(Image).
% Secundarias
    % pixbitX, Y, Bit, Profundidad, Imagen).
    % seleccionar_bit(ListaPixeles,ListaColores).
    % seleccionar_profundidad_bit(ListaProfundidad, ListaProfundidadSinDulplicas).
    % pixeles_a_string_bit(ListaPixeles, Alto, ListaString).
    % insertar_pixeles_blancos_bit(ListaPixeles,PixelesBlancos,ListaPixelesConPixelesBlancos).
	% insertar_pixeles_blancos_profundidad_repetida_bit(ListaPixeles,PixelesBlancos,ListaPixelesConPixelesBlancos).
	% agregar_profundidad_bit(PixelesBlancos,Profundidad,PixelesBlancosConNuevaProfundidad).
	% quitar_primer_pixel(ListaPixeles, ColaListaPixeles). 
    % agregar_salto(ListaPixeles,Alto,ListaPixelesConSalto).
    % agregar_tab(ListaPixeles,ListaPixelesConTab).
    % pixeles_en_blanco_bit(ListaPixeles, ListaPixelesBlancos).
    % pixeles_primera_posicion_bit(ListaPixeles,ListaPixeles).
	% separar_capas_repeticion_profundidades_bit(ListaPixeles,ListaPixelesSeparados)
	% separar_capas_bit(ListaPixeles, ListaPixelesSeparados).
	% impar(N).
	% ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados)
	% agrupar_por_profundidad(ListaPixles,ListaAgrupada)
	% invertir(ListaPixles, ListaPixlesInvertida).

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
    % Analiza que lo ingresado corresponda al tipo de elemento solicitado.
    integer(X), integer(Y), integer(Bit), integer(Profundidad),
    % Lo ingresado debe cumplir parametros.
    X >= 0, Y >= 0, Bit>= 0, Bit =< 1, Profundidad >= 0.

/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*--------------------------------------PREDICADO SELECCIONAR-BIT------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Pone todos los bits en una lista. Utilizado en imageToHistogram. 

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
seleccionar_bit([],[]).    
% Los bits se guardan en la lista de salida.
seleccionar_bit([[_,_,Bit,_]|Cola],[Bit|ColaResultado]):-
    % Llamada recursiva.
  	seleccionar_bit(Cola,ColaResultado).

/*-------------------------------------PREDICADO SELECCIONAR-PROFUNDIDAD------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Guarda las profundidades de todos los pixeles en una lista y elimina los duplicados. Utilizado en imageDepthLayers.

seleccionar_profundidad_bit(ListaProfundidad, ListaSinDulplicas):-
    % Se genera la lista con todas las profundidades.
    seleccionar_profundidad_bit_1(ListaProfundidad,Lista),
    % Con sort se ordenan y se eliminan duplicados.
    sort(Lista, ListaSinDulplicas).
% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
seleccionar_profundidad_bit_1([],[]).    
% Las profundidades de todos los pixeles se guarda en una lista.
seleccionar_profundidad_bit_1([[_,_,_,Profundidad]|Cola],[Profundidad|ColaResultado]):-
    % Llamada recursiva.
  	seleccionar_profundidad_bit_1(Cola,ColaResultado).

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*-------------------------------------PREDICADO PIXELES-A-STRING--------------------------------------------------------------------------------------*/
% Dominio: Lista de pixeles y un entero que corresponde al valor del alto.
% Descripcion: Recibe una lista de pixeles para luego seleccionar los valores correspondientes al color y los agrega en una lista, que ira en otra lista,
% en esta lista se agrega el simbolo "tab" despues de cada color, luego se comeinza a agregar el simbolo de salto de linea, cada una cierta cantidad de 
% elementos que corresponde al alto, se quitan todos los corchetes que representan sublistas, quedando solo una que contiene toda la informacion y con atomics_to_string
% se convierte a string. Dependidendo de la paridad del Alto, se le suma uno, asi este simbolo no ira en un lugar incorrecto. Utilizado en imageToString.

pixeles_a_string_bit(ListaPixeles, Alto, ListaString):-
    % Analiza si es par o impar.
    impar(Alto) ->  
        % Obtiene los colores.
    	seleccionar_bit(ListaPixeles, Pixeles),
        % Inserta la tabulacion.
    	agregar_tab(Pixeles, PixTab),
        % Inserta salto de linea.
   		agregar_salto(PixTab, (Alto+1), PixSalto),
        % Quita sublistas.
    	flatten(PixSalto, PixLista),
        % Convierte a string.
    	atomics_to_string(PixLista, ListaString);
    % Obtiene los colores.
    seleccionar_bit(ListaPixeles, Pixeles),
    % Inserta la tabulacion.
    agregar_tab(Pixeles, PixTab),
    % Inserta salto de linea.
    agregar_salto(PixTab, (Alto+2), PixSalto),
    % Quita sublistas.
    flatten(PixSalto, PixLista),
    % Convierte a string.
    atomics_to_string(PixLista, ListaString).

/*-------------------------------------PREDICADO INSERTAR-PIXELES-BLANCOS-BIT--------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion: inserta los pixeles con los colores blancos en los pixeles de la imagen a la que se le quieren obtener las capas. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
insertar_pixeles_blancos_bit([],_,[]) .      
insertar_pixeles_blancos_bit([[X,Y,Bit ,Profundidad]|Cola],PixelesBlancos,[[[X,Y,Bit ,Profundidad]|PixelesBlancosConProfundidad]|ColaResultado]):- 
    % La profundidad de los pixeles blancos se reemplaza por la del pixel con color.
    agregar_profundidad_bit(PixelesBlancos,Profundidad,PixelesBlancosConProfundidad),
    % Llamada recursiva.
    insertar_pixeles_blancos_bit(Cola,PixelesBlancos,ColaResultado).

/*-------------------------------------PREDICADO INSERTAR-PIXELES-BLANCOS-PROFUNDIDAD-REPETIDA-BIT-------------------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion: inserta los pixeles con los colores blancos en los pixeles de la imagen a la que se le quieren obtener las capas, pero que tiene
% profundidades repeteidas. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
insertar_pixeles_blancos_profundidad_repetida_bit( [],_,[]) .      
insertar_pixeles_blancos_profundidad_repetida_bit( [[[X,Y,Bit ,Profundidad]|ColaGrupo]|Cola] , PixelesBlancos, [ PixelesInsertados |ColaResultado] ) :- 
    % La profundidad de los pixeles blancos se reemplaza por la del pixel con color.
    agregar_profundidad_bit(PixelesBlancos,Profundidad,PixelesConProfundidad),
    % Se insertan en la lista con los otros pixeles
    append([[X,Y,Bit,Profundidad]|ColaGrupo],PixelesConProfundidad,PixelesInsertados),
    % Llamada recursiva.
    insertar_pixeles_blancos_profundidad_repetida_bit(Cola,PixelesBlancos,ColaResultado).

/*-------------------------------------PREDICADO AGREGAR-PROFUNDIDAD-BIT-----------------------------------------------------------------------------------------*/
% Dominio:Lista de pixeles Blancos.
% Descripcion: Cambia la profundidad de los pixeles blancos por la del pixel a la que se le insertaran estos. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
agregar_profundidad_bit([],_,[]).   
% Se realiza el cambio de profundidad.   
agregar_profundidad_bit([[X, Y,Bit ,_]|Cola],Profundidad,[[X, Y, Bit,Profundidad]|ColaResultado]) :- 
    % Llamada recursiva.
    agregar_profundidad_bit(Cola,Profundidad,ColaResultado).

/*-------------------------------------PREDICADO QUITAR-PRIMER-PIXEL------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion:Elimina el primer pixel de una lista de pixeles.
 
quitar_primer_pixel([_|Tail], Tail).

/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/

/*-------------------------------------PREDICADO PIXELES-EN-BLANCO------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Duplica una lista entregada pero cambia el valor de los colores por blanco y la profundidad por 0, pero mantiene la posicion de los pixeles.
% Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.  
pixeles_en_blanco_bit([],[]).
% Cambia el color por blaco y la profundidad por 0.
pixeles_en_blanco_bit([[X,Y,_,_]|Cola], [[X,Y,1,0]|Cabeza]):-
    % Llamada recursiva.
    pixeles_en_blanco_bit(Cola,Cabeza).

/*-------------------------------------PREDICADO PIXELES-PRIMERA-POSCION-BIT------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion: Deja en primera posicion todos los pixeles de la imagen a la que se le crearan las capas. Utilizado en imageDepthLayers.

 
% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
pixeles_primera_posicion_bit([],[]).
% Pone a todos los pixeles de la lista en primera posicion.
pixeles_primera_posicion_bit([[_,_,Bit,Profundidad]|Cola], [[0,0,Bit,Profundidad]|Cabeza]):-
    % Llamada recursiva.
    pixeles_primera_posicion_bit(Cola,Cabeza).

/*-------------------------------------PREDICADO SEPARAR-CAPAS-REPETICION-PROFUNDIDADES-BIT------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Cuando una imagen tiene mas de una profundidad repetida, estas se agrupan en listas luego de ser ordenadas, para luego agregar los pixeles blancos 
% a los que se les elimino la primera posicion. Utilizado en imageDepthLayers. 
 
separar_capas_repeticion_profundidades_bit(ListaPixeles,ListaPixelesSeparados):-
    % Los pixeles se ordenan segun profundidad.
    ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados),
    % Los pixeles se agrupan segun profundidad.
    agrupar_por_profundidad(ListaPixelesOrdenados,ListaPixelesAgrupados),
    % Se crean los pixeles blancos.
	pixeles_en_blanco_bit(ListaPixeles,ListaPixelesBlancos),
    % Se le quita el pixel de la posicion 0,0 a los pixeles blancos.
	quitar_primer_pixel(ListaPixelesBlancos,ListaPixelesBlancos2),
    % A cada profundidad de la imagen se le insertan los pixeles blancos.
	insertar_pixeles_blancos_profundidad_repetida_bit(ListaPixelesAgrupados,ListaPixelesBlancos2,ListaPixelesSeparados),!.

/*-------------------------------------PREDICADO SEPARAR-CAPAS-BIT------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Cuando todos los pixeles tienen profundidades distintas, se le agrega la lista de pixles blancos sin el primer elemento a cada uno de estos
% pero su posicion original se cambia por 0.0. Utilizado en imageDepthLayers. 
 
separar_capas_bit(Lista, Lista2):-
    % Pone a todos los pixeles de la lista en primera posicion.
    pixeles_primera_posicion_bit(Lista,T),
    % Se crean los pixeles blancos.
    pixeles_en_blanco_bit(Lista,R),
    % Se le quita el pixel de la posicion 0,0 a los pixeles blancos.
    quitar_primer_pixel(R,R2),
    % A cada profundidad de la imagen se le insertan los pixeles blancos.
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

/*-------------------------------------PREDICADO ORDENAR-PROFUNDIDAD------------------------------------------------------*/
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
/*--------------------------------------PREDICADO IMAGE-IS-BITMAP------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles).
% Recorrido: Boolean
% Descripcion: Si es una imagen del tipo bitxmap retorna true, sino retorna false

imageIsBitmap([_,_, [[_,_, Bit,_]|_]]) :-
    % Los bits deben cumplir parametros.
    integer(Bit),
    Bit >= 0, Bit < 2-> 
    writeln('#t').