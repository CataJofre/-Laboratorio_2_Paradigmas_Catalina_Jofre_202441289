% Predicado para exportar los predicados a otros archivos.
:- module(tda_pixhex_20244128_JofreLeon,[pixhex/5, imageIsHexmap/1, rgb_hex/2,seleccionar_hex/2,pixeles_a_string_hex/3,seleccionar_profundidad_hex/2,pixeles_en_blanco_hex/2,pixeles_primera_posicion_hex/2,
                                        agregar_profundidad_hex/3,insertar_pixeles_blancos_hex/3,insertar_pixeles_blancos_profundidad_repetida_hex/3,separar_capas_repeticion_profundidades_hex/2,separar_capas_hex/2]).

/*-----------------------------------------------------TDA PIXHEX-------------------------------------------
Este archivo corresponde al TDA pixrhex, se encuentran todos los predicados necesarios para su creacion y tambien
los solicitados en el proyecto de laboratorio.
Por motivos de organizacion se realizo la division del archivo en dos partes principales, la primera corresponde
a aquellos predicados cuyas metas son secundarias por lo que aportan en el funcionamiento de los predicados solicitados
y la segunda donde se encuentran todos los predicados del laboratorio.


PRIMERA PARTE:  PREDICADOS AUXILIARES O PREDICADOS CON FUNCIONES ESPECIFICAS                                  */
% INFORMACION IMPORTANTE.

% Dominios:
    % TDA pixrgb
    % Posiciones X,Y
    % Hex
    % Lista de pixeles.
    % Alto
    % Ancho
    % Imagen

% Predicados:
    % pixhex(X, Y, Hex, Profundidad, Imagen).
    % color(Numero, ValorHexadecimal).
    % seleccionar_hex(ListaPixeles,ListaColores).
    % seleccionar_profundidad_hex(ListaProfundidad, ListaProfundidadSinDulplicas).
    % pixeles_a_string_hex(ListaPixeles, Alto, ListaString).
    % insertar_pixeles_blancos_hex(ListaPixeles,PixelesBlancos,ListaPixelesConPixelesBlancos).
	% insertar_pixeles_blancos_profundidad_repetida_hex(ListaPixeles,PixelesBlancos,ListaPixelesConPixelesBlancos).
	% agregar_profundidad_hex(PixelesBlancos,Profundidad,PixelesBlancosConNuevaProfundidad).
	% quitar_primer_pixel(ListaPixeles, ColaListaPixeles). 
    % parte_decimal(ColorRGB, ColorRGB/16).
    % parte_entera(ColorRGB, ColorRGB/16).
    % obtener_hex_por_entero(ColorRGB, ValorHexadecimal)
    % obtener_hex_por_decimal(ColorRGB, ValorHexadecimal)
    % hex(Numero,Hex)
    % pixRGB_a_pixHex(Pixelrgb,Pixelhex)
    % rgb_hex(ListaPixelesRGB, ListaPixelesHex).
    % agregar_salto(ListaPixeles,Alto,ListaPixelesConSalto).
    % agregar_tab(ListaPixeles,ListaPixelesConTab).
    % pixeles_en_blanco_hex(ListaPixeles, ListaPixelesBlancos).
    % pixeles_primera_posicion_hex(ListaPixeles,ListaPixeles).
	% separar_capas_repeticion_profundidades_hex(ListaPixeles,ListaPixelesSeparados)
	% separar_capas_hex(ListaPixeles, ListaPixelesSeparados).
	% impar(N).
	% ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados)
	% agrupar_por_profundidad(ListaPixles,ListaAgrupada)
	% invertir(ListaPixles, ListaPixlesInvertida).
	% imageIsHexmap(Image).

% Metas:
% Primarias
    % imageIsHexmap(Image).
% Secundarias
    % pixhex(X, Y, Hex, Profundidad, Imagen).
    % color(Numero, ValorHexadecimal).
    % seleccionar_hex(ListaPixeles,ListaColores).
    % seleccionar_profundidad_hex(ListaProfundidad, ListaProfundidadSinDulplicas).
    % pixeles_a_string_hex(ListaPixeles, Alto, ListaString).
    % insertar_pixeles_blancos_hex(ListaPixeles,PixelesBlancos,ListaPixelesConPixelesBlancos).
	% insertar_pixeles_blancos_profundidad_repetida_hex(ListaPixeles,PixelesBlancos,ListaPixelesConPixelesBlancos).
	% agregar_profundidad_hex(PixelesBlancos,Profundidad,PixelesBlancosConNuevaProfundidad).
	% quitar_primer_pixel(ListaPixeles, ColaListaPixeles). 
    % parte_decimal(ColorRGB, ColorRGB/16).
    % parte_entera(ColorRGB, ColorRGB/16).
    % obtener_hex_por_entero(ColorRGB, ValorHexadecimal)
    % obtener_hex_por_decimal(ColorRGB, ValorHexadecimal)
    % hex(Numero,Hex)
    % pixRGB_a_pixHex(Pixelrgb,Pixelhex)
    % rgb_hex(ListaPixelesRGB, ListaPixelesHex).
    % agregar_salto(ListaPixeles,Alto,ListaPixelesConSalto).
    % agregar_tab(ListaPixeles,ListaPixelesConTab).
    % pixeles_en_blanco_hex(ListaPixeles, ListaPixelesBlancos).
    % pixeles_primera_posicion_hex(ListaPixeles,ListaPixeles).
	% separar_capas_repeticion_profundidades_hex(ListaPixeles,ListaPixelesSeparados)
	% separar_capas_hex(ListaPixeles, ListaPixelesSeparados).
	% impar(N).
	% ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados)
	% agrupar_por_profundidad(ListaPixles,ListaAgrupada)
	% invertir(ListaPixles, ListaPixlesInvertida).
/*-----------------------------------------------------REPRESENTACION--------------------------------------*/
% El TDA PIXHEX representa la unidad inicial de una imagen tipo HEXMAP  cuyos colores se representan con un
%  unico valor escrito en forma hexadecimal, ademas de incluir la posicion de de cada pixel,representado
%  con las letras "x" e "y", tal como un plano cartesiano y la profundidad de estos.
% Representacion:
%  pixhex-d <- x (int) X y (int) X hex(String) X depth

/*-----------------------------------------------------HECHOS--------------------------------------*/
% Para cada numero calculado de un color de tipo RGB existe su equivalente en hexadecimal.
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
    % Analiza que lo ingresado corresponda al tipo de elemento solicitado.
    integer(X), integer(Y), string(Hex), integer(Profundidad),
    % Lo ingresado debe cumplir parametros.
    X >= 0, Y >= 0,  Profundidad >= 0.

/*-----------------------------------------------------SELECTORES------------------------------------------*/
/*-------------------------------------- PREDICADO SELECCIONAR-HEX---------------------------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: guarda el conjunto de colores de un pixel en una lista. Utilizado en imageToHistogram. 

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
seleccionar_hex([],[]).    
% Los colores se guardan en la lista de salida.
seleccionar_hex([[_,_,Hex,_]|Cola],[Hex|ColaResultado]):-
    % Llamada recursiva.
  	seleccionar_hex(Cola,ColaResultado).

/*-------------------------------------PREDICADO SELECCIONAR-PROFUNDIDAD------------------------------------------------------*/
% Dominio: Lista de pixeles
% Descripcion: Guarda las profundidades de todos los pixeles en una lista y elimina los duplicados. Utilizado en imageDepthLayers.
 
seleccionar_profundidad_hex(ListaProfundidad, ListaSinDulplicas):-
    % Se genera la lista con todas las profundidades.
    seleccionar_profundidad_hex_1(ListaProfundidad,Lista),
    % Con sort se ordenan y se eliminan duplicados.
    sort(Lista, ListaSinDulplicas).
% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
seleccionar_profundidad_hex_1([],[]).  
% Las profundidades de todos los pixeles se guarda en una lista.  
seleccionar_profundidad_hex_1([[_,_,_,Profundidad]|Cola],[Profundidad|ColaResultado]):-
    % Llamada recursiva.
  	seleccionar_profundidad_hex_1(Cola,ColaResultado).

/*-----------------------------------------------------MODIFICADORES---------------------------------------*/
/*-------------------------------------PREDICADO PIXELES-A-STRING--------------------------------------------------------------------------------------*/
% Dominio: Lista de pixeles y un entero que corresponde al valor del alto.
% Descripcion: Recibe una lista de pixeles para luego seleccionar los valores correspondientes al color y los agrega en una lista, que ira en otra lista,
% en esta lista se agrega el simbolo "tab" despues de cada color, luego se comeinza a agregar el simbolo de salto de linea, cada una cierta cantidad de 
% elementos que corresponde al alto, se quitan todos los corchetes que representan sublistas, quedando solo una que contiene toda la informacion y con atomics_to_string
% se convierte a string. Dependidendo de la paridad del Alto, se le suma uno, asi este simbolo no ira en un lugar incorrecto. Utilizado en imageToString.

pixeles_a_string_hex(ListaPixeles, Alto, ListaString):-
    % Analiza si es par o impar.
    impar(Alto) ->  
    	seleccionar_hex(ListaPixeles, Pixeles),
        % Obtiene los colores.
    	agregar_tab(Pixeles, PixTab),
        % Inserta salto de linea.
    	agregar_salto(PixTab, (Alto+1), PixSalto),
        % Quita sublistas.
    	flatten(PixSalto, PixLista),
        % Convierte a string.
    	atomics_to_string(PixLista, ListaString);
    % Obtiene los colores.
    seleccionar_hex(ListaPixeles, Pixeles),
    % Inserta la tabulacion.
    agregar_tab(Pixeles, PixTab),
    % Inserta salto de linea.
    agregar_salto(PixTab, Alto, PixSalto),
    % Quita sublistas.
    flatten(PixSalto, PixLista),
    % Convierte a string.
    atomics_to_string(PixLista, ListaString).

/*-------------------------------------PREDICADO AGREGAR-PROFUNDIDAD-HEX-----------------------------------------------------------------------------------------*/
% Dominio:Lista de pixeles Blancos.
% Descripcion: Cambia la profundidad de los pixeles blancos por la del pixel a la que se le insertaran estos. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente.
agregar_profundidad_hex([],_,[]).   
% Se realiza el cambio de profundidad.   
agregar_profundidad_hex([[X, Y, Hex,_]|Cola],Profundidad,[[X, Y, Hex,Profundidad]|ColaResultado]) :- 
    % Llamada recursiva.
    agregar_profundidad_hex(Cola,Profundidad,ColaResultado).

/*-------------------------------------PREDICADO INSERTAR-PIXELES-BLANCOS-HEX--------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion: inserta los pixeles con los colores blancos en los pixeles de la imagen a la que se le quieren obtener las capas. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
insertar_pixeles_blancos_hex([],_,[]) .      
insertar_pixeles_blancos_hex([[X,Y,Hex ,Profundidad]|Cola],PixelesBlancos,[[[X,Y,Hex ,Profundidad]|PixelesBlancosConProfundidad]|ColaResultado]):- 
    % La profundidad de los pixeles blancos se reemplaza por la del pixel con color.
    agregar_profundidad_hex(PixelesBlancos,Profundidad,PixelesBlancosConProfundidad),
    % Llamada recursiva.
    insertar_pixeles_blancos_hex(Cola,PixelesBlancos,ColaResultado).

/*-------------------------------------PREDICADO INSERTAR-PIXELES-BLANCOS-PROFUNDIDAD-REPETIDA-HEX-----------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion: inserta los pixeles con los colores blancos en los pixeles de la imagen a la que se le quieren obtener las capas, pero que tiene
% profundidades repeteidas. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
insertar_pixeles_blancos_profundidad_repetida_hex([],_,[]) .      
insertar_pixeles_blancos_profundidad_repetida_hex([[[X,Y,Hex ,Profundidad]|ColaGrupo]|Cola] , PixelesBlancos, [ PixelesInsertados |ColaResultado]) :- 
    % La profundidad de los pixeles blancos se reemplaza por la del pixel con color.
    agregar_profundidad_hex(PixelesBlancos,Profundidad,PixelesConProfundidad),
    % Se insertan en la lista con los otros pixeles
    append([[X,Y, Hex,Profundidad]|ColaGrupo],PixelesConProfundidad,PixelesInsertados),
    % Llamada recursiva.
    insertar_pixeles_blancos_profundidad_repetida_hex(Cola,PixelesBlancos,ColaResultado).
     
/*-------------------------------------PREDICADO QUITAR-PRIMER-PIXEL------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion:Elimina el primer pixel de una lista de pixeles. Utilizado en imageDepthLayers.
 
quitar_primer_pixel([_|Cola], Cola).    
% Deja solo la cola de la lista.

/*-----------------------------------------------------OTROS PREDICADOS------------------------------------*/
/*--------------------------------------PREDICADO PARTE-DECIMAL------------------------------------------------------*/
% Dominio: Entero que corresponde al valor de R,G o B.
% Descripcion: Realiza la division para obtener el valor en hexadecimal de la division de un valor en rgb. Utilizado en imageRGBToHex

parte_decimal(Dividendo, Resultado):-
    % Divide en 16 el valor que corresonde al color.
 	A is Dividendo/16,
    % Obtiene la parte entera de la division.
    floor(A, B),
    % Resta la parte entera del resultado al resultado y lo multiplica por 16.
    Resultado is (A- B) *16 .

/*--------------------------------------PREDICADO PARTE-ENTERA------------------------------------------------------*/
% Dominio: Entero que corresponde al valor de R,G o B.
% Descripcion:  Realiza la division para obtener el valor en hexadecimal de la division de un valor en rgb. Utilizado en imageRGBToHex

parte_entera(Dividendo, Resultado):-
    % Divide en 16 el valor que corresonde al color, obtiene la parte entera de la division.
    floor((Dividendo/16), Res),
    % Se multiplica por 1.0 para que quede igual al resultado de la parte decimal.
    Resultado is Res * 1.0.

/*--------------------------------------PREDICADO OBTENER-HEX-POR-DECIMAL------------------------------------------------------*/
% Dominio: Entero que corresponde al valor de R,G o B.
% Descripcion: Realiza el calculo que permite pasar del color en rgb a hexadecimal, tomando la parte decimal de la division
% por 16 realizada anteriormente, devuelve un varlor en hexadecimal. Utilizado en imageRGBToHex

obtener_hex_por_decimal(Numero, Hex):-
    % Se realiza el calculo del numero correspondiente.
    parte_decimal(Numero,R),
    % Con el numero obtenido se encuentra su parte hexadecimal definida en los hechos.
	color(R,Hex).

/*--------------------------------------PREDICADO OBTENER-HEX-POR-ENTERO------------------------------------------------------*/
% Dominio: Entero que corresponde al valor de R,G o B.
% Descripcion: Realiza el calculo que permite pasar del color en rgb a hexadecimal, tomando la parte entera de la division
% por 16 realizada anteriormente, devuelve un varlor en hexadecimal. Utilizado en imageRGBToHex

obtener_hex_por_entero(Numero, Hex):-
    % Se realiza el calculo del numero correspondiente.
    parte_entera(Numero,R),
    % Con el numero obtenido se encuentra su parte hexadecimal definida en los hechos.
	color(R,Hex).

/*--------------------------------------PREDICADO HEX------------------------------------------------------*/
% Dominio: Entero que corresponde al valor de R,G o B.
% Descripcion: Convierte un numero que corresponde a un color de tipo R,G,o B, y utilizando los predicados anteriores
% devuelve un string de 2 valores en hexadecimal. Utilizado en imageRGBToHex

hex(Numero,Hex):-
    % Se obtienen los 2 valores hexadecimales.
    obtener_hex_por_entero(Numero, H2),
    obtener_hex_por_decimal(Numero, H1), 
    % Se unen los valores hexadecimales.
    string_concat(H2, H1, Hex).

/*--------------------------------------PREDICADO PIXRGB-A-PIXHEX------------------------------------------------------*/
% Dominio: Pixel de tipo RGB.
% Descripcion: Toma un pixel de tipo pixrgb y transforma los valores del color RGB en color hexadecimal, luego agrega el "#".
% Utilizado en imageRGBToHex

pixRGB_a_pixHex( [_,_,R,G,B,_], PixelHex):-
    % Calcula el valor de B y G, luego los une.
    hex(B, HexB),
    hex(G, HexG),
    string_concat(HexG,HexB, Res),
    % Calcula el valor correspondiente a R y lo junta con el string anterior.
    hex(R, HexR),
    string_concat(HexR,Res, Pixel),
    % Agrega el simbolo de "#"
    string_concat("#",Pixel, PixelHex).
   
/*--------------------------------------PREDICADO RGB-HEX------------------------------------------------------*/
% Dominio: Lista de pixeles de tipo pixrgb.
% Descripcion: Transforma una lista de pixeles de tipo pixrgb a pixhex, cambiando los colores de decimal a hexadecimal. Utilizado en imageRGBToHex

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
rgb_hex([], []).
rgb_hex([[X,Y,R,G,B,Profundidad]|Cola], [[X1,Y1,Hex,Profundidad1]|ColaResultado]):-
    % Se mantienen los valores de X, Y y la Profundidad.
  	X1 is X ,
    Y1 is Y, 
    % De los pixrgb se toman los colores y se modifican.
    pixRGB_a_pixHex( [_,_,R,G,B,_], Hex),
    Profundidad1 is Profundidad,
    % Llamada recursiva.
    rgb_hex(Cola, ColaResultado).

/*-------------------------------------PREDICADO PIXELES-EN-BLANCO------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Duplica una lista entregada pero cambia el valor de los colores por blanco y la profundidad por 0, pero mantiene la posicion de los pixeles.
% Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
pixeles_en_blanco_hex([],[]).
% Cambia el color por blaco y la profundidad por 0.
pixeles_en_blanco_hex([[X,Y,_,_]|Cola], [[X,Y,"#FFFFFF",0]|Cabeza]):-
    % Llamada recursiva.
    pixeles_en_blanco_hex(Cola,Cabeza).

/*-------------------------------------PREDICADO PIXELES-PRIMERA-POSCION-HEX------------------------------------------------------*/
% Dominio:Lista de pixeles.
% Descripcion: Deja en primera posicion todos los pixeles de la imagen a la que se le crearan las capas. Utilizado en imageDepthLayers.

% Se recorren las listas hasta que no haya ningun elemento en la posicion siguiente. 
pixeles_primera_posicion_hex([],[]).
% Pone a todos los pixeles de la lista en primera posicion.
pixeles_primera_posicion_hex([[_,_,Hex,Profundidad]|Cola], [[0,0,Hex,Profundidad]|Cabeza]):-
    % Llamada recursiva.
    pixeles_primera_posicion_hex(Cola,Cabeza).

/*-------------------------------------PREDICADO SEPARAR-CAPAS-REPETICION-PROFUNDIDADES-HEX------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Cuando una imagen tiene mas de una profundidad repetida, estas se agrupan en listas luego de ser ordenadas, para luego agregar los pixeles blancos 
% a los que se les elimino la primera posicion. Utilizado en imageDepthLayers. 
 
separar_capas_repeticion_profundidades_hex(ListaPixeles,ListaPixelesSeparados):-
    % Los pixeles se ordenan segun profundidad.
    ordenar_profundidad(ListaPixeles,ListaPixelesOrdenados),
    % Los pixeles se agrupan segun profundidad.
    agrupar_por_profundidad(ListaPixelesOrdenados,ListaPixelesAgrupados),
    % Se crean los pixeles blancos.
	pixeles_en_blanco_hex(ListaPixeles,ListaPixelesBlancos),
    % Se le quita el pixel de la posicion 0,0 a los pixeles blancos.
	quitar_primer_pixel(ListaPixelesBlancos,ListaPixelesBlancos2),
    % A cada profundidad de la imagen se le insertan los pixeles blancos.
	insertar_pixeles_blancos_profundidad_repetida_hex(ListaPixelesAgrupados,ListaPixelesBlancos2,ListaPixelesSeparados),!.

/*-------------------------------------PREDICADO SEPARAR-CAPAS-HEX------------------------------------------------------*/
% Dominio: Lista de pixeles.
% Descripcion: Cuando todos los pixeles tienen profundidades distintas, se le agrega la lista de pixles blancos sin el primer elemento a cada uno de estos
% pero su posicion original se cambia por 0.0. Utilizado en imageDepthLayers. 
 
separar_capas_hex(Lista, Lista2):-
    % Pone a todos los pixeles de la lista en primera posicion.
    pixeles_primera_posicion_hex(Lista,T),
    % Se crean los pixeles blancos.
    pixeles_en_blanco_hex(Lista,R),
    % Se le quita el pixel de la posicion 0,0 a los pixeles blancos.
    quitar_primer_pixel(R,R2),
    % A cada profundidad de la imagen se le insertan los pixeles blancos.
    insertar_pixeles_blancos_hex(T,R2,Lista2),!.

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

/*-------------------------------------PREDICADO IMPAR------------------------------------------------------*/
% Dominio: Entero, Alto de la imagen
% Recorrido: Boolean
% Descripcion: Ve si el valor del alto de la imagen corresponde a un numero impar o no.

impar(N):- 
    mod(N,2) =:= 0.

/*-------------------------------------PREDICADO ORDEDAR-PROFUNDIDAD------------------------------------------------------*/
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
% Descripcion: Invierte el orden de los pixeles, dejando la profundidad al principio.
 
invertir([],[]).
invertir([Cabeza| Cola], [CabezaInvertida|ColaResultado]):-   
    reverse(Cabeza,CabezaInvertida),
    invertir(Cola,ColaResultado).

/*--------------------------------SEGUNDA PARTE DEL ARCHIVO------------------------------------------------*/

/*              AQUI SE ENCUENTRAN LOS PREDICADOS REQUERIDOS EN EL LABORATORIO                             */

/* --------------------------------------------------------------------------------------------------------*/
/*-----------------------------------------------------PREDICADOS DE PERTENENCIA---------------------------*/
/*--------------------------------------PREDICADO IMAGE-IS-HEXMAP------------------------------------------------------*/
% Dominio: Imagen (lista con el ancho, alto y los pixeles)
% Recorrido: Boolean
% Descripcion: Si es una imagen del tipo hex xmap retorna true, sino retorna false

imageIsHexmap([_,_, [[_,_, Hex,_]|_]]) :-
    % El tercer elemento debe ser de tipo string   
    string(Hex)-> 
    % Retorno del resultado.
    writeln('#t').