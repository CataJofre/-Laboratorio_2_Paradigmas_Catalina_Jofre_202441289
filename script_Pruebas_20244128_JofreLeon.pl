% predicados para importar los predicados de los otros archivos.
:- use_module(tda_image_20244128_JofreLeon).
:- use_module(tda_pixbit_20244128_JofreLeon).
:- use_module(tda_pixhex_20244128_JofreLeon).
:- use_module(tda_pixrgb_20244128_JofreLeon).

/*-------------------------------------------------------------------------------------------------------------------

                             SCRIPT DE PRUEBAS: EJEMPLOS DEL LABORATORIO

--------------------------------------------------------------------------------------------------------------------*/

/*-----------------------------------------------------------EJEMPLOS IMAGEN PIXBIT---------------------------------------------------------*/
% Probar que se puede generar una imagen pixbit
% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap(I), imageToString(I, Str),write(Str).

% Probar que imageIsBitMap detecta cuando se tiene una imagen en hex o en rgb.
% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap(I).

% Estos casos deben dar false:

% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap(I).

% pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), pixrgb( 1, 0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap(I).

/*-----------------------------------------------------------EJEMPLOS IMAGEN PIXHEX---------------------------------------------------------*/
% Probar que se puede generar una imagen pixhex
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageToString(I, Str),write(Str).

% Probar que imageIsHexmap detecta cuando se tiene una imagen en bit o en rgb.
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap(I).

% Estos casos deben dar false:

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap(I).

% pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), pixrgb( 1, 0, 190, 190, 190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap(I).

/*-----------------------------------------------------------EJEMPLOS IMAGEN PIXRGB---------------------------------------------------------*/
% Probar que se puede generar una imagen pixrgb
% pixrgb( 0, 0, 255, 0, 0, 10, PA), pixrgb( 0, 1, 255, 0, 0, 20, PB), pixrgb( 1, 0, 0, 0, 255, 30, PC), pixrgb( 1, 1, 0, 0, 255, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageToString(I, Str),write(Str).

% Probar que imageIsPixmap detecta cuando se tiene una imagen en hex o en bit.
% pixrgb( 0, 0, 200, 200, 200, 10, PA), pixrgb( 0, 1, 200, 200, 200, 20, PB), pixrgb( 1, 0, 190, 190,190, 30, PC), pixrgb( 1, 1, 190, 190, 190, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap(I).

% Estos casos deben dar false:

% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap(I).
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap(I).

/*-----------------------------------------------------------EJEMPLOS IMAGERGBTOHEX---------------------------------------------------------*/

% Convierte una imagen RGB a HEX y comprueba con los EJEMPLOSs de pertenencia, luego convierte a string y muestra por pantalla:

/*
pixrgb(0, 0, 200, 200, 200, 10, PA), pixrgb(0, 1, 200, 200, 200, 20, PB), pixrgb(1, 0, 190, 190,190, 30, PC), pixrgb(1, 1, 190, 190, 190, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), 
imageIsPixmap(I), imageRGBToHex(I, I2), imageIsHexmap(I2), imageToString(I2, Str), write(Str).

*/

/*-----------------------------------------------------------EJEMPLOS IMAGE-COMPRESS Y IMAGE-DECOMPRESS---------------------------------------------------------*/

% Comprime una imagen, luego descomprime y debe resultar la misma imagen original:
% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageCompress(I, I2,Z), imageDecompress(I2,Z,I3).

% En el ejemplo anterior "I" debería ser igual a "I3"

/*-----------------------------------------------------------EJEMPLOS IMAGE-ROTATE-90---------------------------------------------------------*/

% Si se rota una imagen 4 veces en 90°, debería resultar la imagen original:

% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2), imageRotate90(I2, I3), imageRotate90(I3, I4), imageRotate90(I4, I5).

% En el ejemplo anterior "I" debería ser igual a "I5"

% Si se rota una imagen en 90° que tiene el mismo color y profundidad en todos sus píxeles, entonces la imagen resultante debería ser la misma imagen original.
% pixhex( 0, 0, "#FF0000", 30, PA), pixhex( 0, 1, "#FF0000", 30, PB), pixhex( 1, 0, "#FF0000", 30, PC), pixhex( 1, 1, "#FF0000", 30, PD), image( 2, 2, [PA, PB, PC, PD], I), imageRotate90(I, I2).

% En el ejemplo anterior "I" debería ser igual a "I2"
/*-----------------------------------------------------------EJEMPLOS IMAGE-FLIP-V---------------------------------------------------------*/
% Si se hace imageFlipV dos veces de una imagen, debería resultar la imagen original:
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageFlipV(I, I2), imageFlipV(I2, I3).

% En el ejemplo anterior "I" debería ser igual a "I3"

/*-----------------------------------------------------------EJEMPLOS IMAGE-FLIP-H---------------------------------------------------------*/
% Si se hace imageFlipH dos veces de una imagen, debería resultar la imagen original:
% pixhex( 0, 0, "#FF0000", 10, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 1, 0, "#0000FF", 30, PC), pixhex( 1, 1, "#0000FF", 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2), imageFlipH(I2, I3).

% En el ejemplo anterior "I" debería ser igual a "I3"

% Si se hace imageFlipH a una imagen que tiene el mismo color y profundidad en todos sus pixeles, entonces la imagen resultante debería ser la misma imagen original.
% pixhex( 0, 0, "#FF0000", 30, PA), pixhex( 0, 1, "#FF0000", 30, PB), pixhex( 1, 0, "#FF0000", 30, PC), pixhex( 1, 1, "#FF0000", 30, PD), image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I, I2).

% En el ejemplo anterior "I" debería ser igual a "I2"
/*-----------------------------------------------------------EJEMPLOS IMAGE-CROP---------------------------------------------------------*/
% Se crea una imagen de 3x3 pixeles y se corta en una de 2x2 con solo la esquina inferior izquierda:
/*
pixhex( 0, 0, "#FF0000", 20, PA), pixhex( 0, 1, "#FF0000", 20, PB), pixhex( 0, 2, "#FF0000", 20, PC), pixhex( 1, 0, "#0000FF", 30, PD), pixhex( 1, 1, "#0000FF", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#0000FF", 4, PG), pixhex( 2, 1, "#0000FF", 4, PH), pixhex( 2, 2, "#0000FF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageCrop( I, 1, 1, 2, 2, I2), 
pixhex( 0, 0, "#0000FF", 4, PE2), pixhex( 0, 1, "#0000FF", 4, PF2), pixhex( 1, 0, "#0000FF", 4, PH2), pixhex( 1, 1, "#0000FF", 4, PI2), image( 2, 2, [PE2, PF2, PH2, PI2], I3).

*/

% En el ejemplo anterior, "I2" debería ser una imagen con los mismos pixeles y dimensiones que "I3"

/*-----------------------------------------------------------EJEMPLOS IMAGE-CHANGE-PIXEL---------------------------------------------------------*/ 
% Toma el píxel de la posición (0,1) que en la imagen original tiene los valores RGB (20, 20, 20) y lo reemplaza por otro píxel con valor RGB (54, 54, 54).
% pixrgb( 0, 0, 10, 10, 10, 10, P1), pixrgb( 0, 1, 20, 20, 20, 20, P2), pixrgb( 1, 0, 30, 30, 30, 30, P3), pixrgb( 1, 1, 40, 40, 40, 40, P4), image( 2, 2, [P1, P2, P3, P4], I1), pixrgb( 0, 1, 54, 54, 54, 20, P2_modificado), imageChangePixel(I1, P2_modificado, I2).

/*-----------------------------------------------------------EJEMPLOS IMAGE-DEPTH-LAYERS---------------------------------------------------------*/
% Se construye imagen de 2x2 con los primeros 2 pixeles con profundidad 10 y los otros 2 con profundidad de 30, entonces al consultar "imageDepthLayers" se debería obtener una lista con dos imágenes.
/* 
pixrgb(0, 0, 33, 33, 33, 10, PA), pixrgb(0, 1, 44, 44, 44, 10, PB), pixrgb(1, 0, 55, 55, 55, 30, PC), pixrgb(1, 1, 66, 66, 66, 30, PD), image(2, 2, [PA, PB, PC, PD], I), 
imageDepthLayers(I, [PRIMERA, SEGUNDA]), pixrgb(0, 0, 33, 33, 33, 10, PA2), pixrgb(0, 1, 44, 44, 44, 10, PB2), pixrgb(1, 0, 255, 255, 255, 10, PC2), pixrgb(1, 1, 255, 255, 255, 10, PD2), 
image(2, 2, [PA2, PB2, PC2, PD2], I2), pixrgb( 0, 0, 255, 255, 255, 30, PA3), pixrgb(0, 1, 255, 255, 255, 30, PB3), pixrgb(1, 0, 55, 55, 55, 30, PC3), pixrgb( 1, 1, 66, 66, 66, 30, PD3), 
image(2, 2, [PA3, PB3, PC3, PD3], I3).
 
 */
% En el ejemplo anterior, "I2" debería ser una imagen con los mismos pixeles y dimensiones que "PRIMERA". "I3" debería ser una imagen con los mismos pixeles y dimensiones que "SEGUNDA".


/*----------------------------------------------------------------------------------------------------------------------------------------

                             SCRIPT DE PRUEBAS: EJEMPLOS DE CADA FUNCION

-----------------------------------------------------------------------------------------------------------------------------------------*/

/*-------------------------------------EJEMPLOS IMAGE -----------------------------------------------------------------------------------*/
% Pixbit
% pixbit( 0, 0, 1, 5, PA), pixbit( 0, 1, 0, 6, PB), pixbit( 1, 0, 0, 26, PC), pixbit( 1, 1, 1, 13,PD), image( 2, 2, [PA, PB, PC, PD], I).

% pixhex
/*
pixhex( 0, 0, "#CC0000", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#B800DD", 4, PG), pixhex( 2, 1, "#DD1200", 4, PH), pixhex( 2, 2, "#DDCCFF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I).

*/
% pixrgb
% pixrgb(0, 0, 13, 18, 19, 10, PA), pixrgb(0, 1, 26, 220, 210, 130, PB), pixrgb(1, 0, 10, 140,110, 34, PC), pixrgb(1, 1, 110, 12, 132, 4, PD), image( 2, 2, [PA, PB, PC, PD], I).

/*-------------------------------------EJEMPLOS IMAGE-IS-BITMAP--------------------------------------------------------------------------*/
% verdadero
% pixbit( 0, 0, 0, 16, PA), pixbit( 0, 1, 0, 28, PB), pixbit( 1, 0, 1, 35, PC), pixbit( 1, 1, 1, 57, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap(I).


% falso.
% pixrgb(0, 0, 13, 18, 19, 10, PA), pixrgb(0, 1, 26, 220, 210, 130, PB), pixrgb(1, 0, 10, 140,110, 34, PC), pixrgb(1, 1, 110, 12, 132, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsBitmap(I).

% falso
/*
 pixhex( 0, 0, "#CC0000", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#B800DD", 4, PG), pixhex( 2, 1, "#DD1200", 4, PH), pixhex( 2, 2, "#DDCCFF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageIsBitmap(I).

*/
/*-------------------------------------EJEMPLOS IMAGE-IS-HEXMAP--------------------------------------------------------------------------*/

% verdadero
% pixhex( 0, 0, "#FF0000", 30, PA), pixhex( 0, 1, "#FF0000", 30, PB), pixhex( 1, 0, "#FF0000", 30, PC), pixhex( 1, 1, "#FF0000", 30, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap(I).

% falso.
% pixrgb(0, 0, 13, 18, 19, 10, PA), pixrgb(0, 1, 26, 220, 210, 130, PB), pixrgb(1, 0, 10, 140,110, 34, PC), pixrgb(1, 1, 110, 12, 132, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap(I).

% falso
% pixbit( 0, 0, 1, 7, PA), pixbit( 0, 1, 0, 14, PB), pixbit( 1, 0, 1, 56, PC), pixbit( 1, 1, 1, 6, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsHexmap(I).

/*-------------------------------------EJEMPLOS IMAGE-IS-PIXMAP--------------------------------------------------------------------------*/
% verdadero
% pixrgb(0, 0, 13, 18, 19, 10, PA), pixrgb(0, 1, 26, 220, 210, 130, PB), pixrgb(1, 0, 10, 140,110, 34, PC), pixrgb(1, 1, 110, 12, 132, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap(I).

% falso.
% pixhex( 0, 0, "#FF0D00", 30, PA), pixhex( 0, 1, "#FF00DD", 30, PB), pixhex( 1, 0, "#FF0D03", 30, PC), pixhex( 1, 1, "#FD00D0", 30, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap(I).

% verdadero
% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 23, PB), pixbit( 1, 0, 0, 32, PC), pixbit( 1, 1, 1, 1, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsPixmap(I).

/*-------------------------------------EJEMPLOS IMAGE-IS-COMPRESSED----------------------------------------------------------------------*/
% falso
% pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 1, 0, 0, 30, PC), pixbit( 1, 1, 1, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageIsCompressed(I).

% verdadero.
% pixhex( 0, 0, "#FF0D00", 30, PA), pixhex( 0, 1, "#FF00DD", 30, PB), pixhex( 1, 0, "#FF0D03", 30, PC), pixhex( 1, 1, "#FD00D0", 30, PD), image( 2, 2, [PA, PB, PC, PD], I), imageCompress(I,I2,_),imageIsCompressed(I2).

% falso 
% pixhex( 0, 0, "#FF0D00", 30, PA), pixhex( 0, 1, "#FF00DD", 30, PB), pixhex( 1, 0, "#FF0D03", 30, PC), pixhex( 1, 1, "#FD00D0", 30, PD), image( 2, 2, [PA, PB, PC, PD], I), imageCompress(I,I2,Z),imageDecompress(I2,Z,I3), imageIsCompressed(I3).

/*-------------------------------------EJEMPLOS FLIPH------------------------------------------------------------------------------------*/
% Pixbit
% pixbit( 0, 0, 1, 5, PA), pixbit( 0, 1, 0, 6, PB), pixbit( 1, 0, 0, 26, PC), pixbit( 1, 1, 1, 13,PD), image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I,I2).

% pixhex= con dos inversiones la imagen queda igual a la original
/*
pixhex( 0, 0, "#CC0000", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#B800DD", 4, PG), pixhex( 2, 1, "#DD1200", 4, PH), pixhex( 2, 2, "#DDCCFF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageFlipH(I,I2), imageFlipH(I2,I3).

*/
% pixrgb
% pixrgb(0, 0, 13, 18, 19, 10, PA), pixrgb(0, 1, 26, 220, 210, 130, PB), pixrgb(1, 0, 10, 140,110, 34, PC), pixrgb(1, 1, 110, 12, 132, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageFlipH(I,I2).

/*-------------------------------------EJEMPLOS FLIPV------------------------------------------------------------------------------------*/
% Pixbit= con dos inversiones la imagen queda igual a la original
% pixbit( 0, 0, 1, 5, PA), pixbit( 0, 1, 0, 6, PB), pixbit( 1, 0, 0, 26, PC), pixbit( 1, 1, 1, 13,PD), image( 2, 2, [PA, PB, PC, PD], I), imageFlipV(I,I2), imageFlipV(I2,I3).

% pixhex
/*
pixhex( 0, 0, "#CC0000", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#B800DD", 4, PG), pixhex( 2, 1, "#DD1200", 4, PH), pixhex( 2, 2, "#DDCCFF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageFlipV(I,I2).

*/
% pixrgb
% pixrgb(0, 0, 13, 18, 19, 10, PA), pixrgb(0, 1, 26, 220, 210, 130, PB), pixrgb(1, 0, 10, 140,110, 34, PC), pixrgb(1, 1, 110, 12, 132, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageFlipV(I,I2).

/*-------------------------------------EJEMPLOS IMAGE-CROP-------------------------------------------------------------------------------*/
% Pixbit= retorna una imágen con 2 pixeles
% pixbit( 0, 0, 1, 5, PA), pixbit( 0, 1, 0, 6, PB), pixbit( 1, 0, 0, 26, PC), pixbit( 1, 1, 1, 13,PD), image( 2, 2, [PA, PB, PC, PD], I), imageCrop(I,0,1,1,1,I2).

% pixhex= retorna una imágen con 4 pixeles
/*
pixhex( 0, 0, "#CC0000", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#B800DD", 4, PG), pixhex( 2, 1, "#DD1200", 4, PH), pixhex( 2, 2, "#DDCCFF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageCrop(I,1,1,2,2,I2).

*/
% pixrgb = retorna 1 pixel
% pixrgb(0, 0, 13, 18, 19, 10, PA), pixrgb(0, 1, 26, 220, 210, 130, PB), pixrgb(1, 0, 10, 140,110, 34, PC), pixrgb(1, 1, 110, 12, 132, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageCrop(I,0,0,0,0,I2).

/*-------------------------------------EJEMPLOS IMAGE-RGB-TO-HEX-------------------------------------------------------------------------*/
% pixrgb 
% pixrgb(0, 0, 13, 18, 19, 10, PA), pixrgb(0, 1, 26, 220, 210, 130, PB), pixrgb(1, 0, 10, 140,110, 34, PC), pixrgb(1, 1, 110, 12, 132, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageRGBToHex(I,I2).

/*
pixrgb( 0, 0, 24,55,63, 20, PA), pixrgb( 0, 1, 222,55,63, 20, PB), pixrgb( 0, 2, 22,53,46, 20, PC), pixrgb( 1, 0, 21,56,65, 30, PD), pixrgb( 1, 1, 255,54,65, 4, PE), pixrgb( 1, 2, 2,5,6, 4, PF), 
pixrgb( 2, 0, 22,65,67, 4, PG), pixrgb( 2, 1, 28,57,66, 4, PH), pixrgb( 2, 2, 26,58,68, 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageRGBToHex(I,I2).

*/
% pixrgb(0, 0, 3, 4, 19, 10, PA), pixrgb(0, 1, 26, 45, 23, 130, PB), pixrgb(1, 0, 10, 5,115, 34, PC), pixrgb(1, 1, 14, 122, 133, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageRGBToHex(I,I2).

/*-------------------------------------EJEMPLOS IMAGE-TO-HISTOGRAM-----------------------------------------------------------------------*/
/*
pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 0, 2 , 0, 30, PC), pixbit( 1, 0, 1, 4,PD),pixbit( 1, 1, 1, 4,PE), pixbit( 1, 2, 1, 4,PF), 
pixbit( 2, 0, 1, 4,PG), pixbit( 2, 1, 1, 4,PH), pixbit( 2, 2, 1, 4,PI), image(3,3,[PA, PB, PC, PD,PE, PF, PG,PH, PI],I), imageToHistogram(I,Histograma).

*/

/*
pixrgb( 0, 0, 26,26,26, 20, PA), pixrgb( 0, 1, 26,26,26, 20, PB), pixrgb( 0, 2, 26,26,26, 20, PC), pixrgb( 1, 0, 21,56,65, 30, PD), pixrgb( 1, 1, 255,54,65, 4, PE), pixrgb( 1, 2, 2,5,6, 4, PF), 
pixrgb( 2, 0, 22,65,67, 4, PG), pixrgb( 2, 1, 28,57,66, 4, PH), pixrgb( 2, 2, 26,58,68, 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageToHistogram(I,Histograma).

*/

/*
pixhex( 0, 0, "#CCCCCC", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#FFFFFF", 4, PG), pixhex( 2, 1, "#CCCCCC", 4, PH), pixhex( 2, 2, "#CCCCCC", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), imageToHistogram(I,Histograma).

*/


/*-------------------------------------EJEMPLOS IMAGE-ROTATE-90--------------------------------------------------------------------------*/
/*
pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 0, 2 , 0, 30, PC), pixbit( 1, 0, 1, 4,PD),pixbit( 1, 1, 1, 4,PE), pixbit( 1, 2, 1, 4,PF), 
pixbit( 2, 0, 1, 4,PG), pixbit( 2, 1, 1, 4,PH), pixbit( 2, 2, 1, 4,PI), image(3,3,[PA, PB, PC, PD,PE, PF, PG,PH, PI],I), imageRotate90(I,I2).

*/

/*
pixrgb( 0, 0, 10, 10, 10, 10, P1), pixrgb( 0, 1, 20, 20, 20, 20, P2), pixrgb( 1, 0, 20, 20, 20, 30, P3), 
pixrgb( 1, 1, 40, 40, 40, 10, P4), image( 2, 2, [P1, P2, P3, P4], I), imageRotate90(I,I2).

*/
/*
pixhex( 0, 0, "#CCCCCC", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#FFFFFF", 4, PG), pixhex( 2, 1, "#CCCCCC", 4, PH), pixhex( 2, 2, "#CCCCCC", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), 
imageRotate90(I,I2).

*/

/*-------------------------------------EJEMPLOS IMAGE-COMPRESS---------------------------------------------------------------------------*/
/*
pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 0, 2 , 0, 30, PC), pixbit( 1, 0, 1, 4,PD),pixbit( 1, 1, 1, 4,PE), pixbit( 1, 2, 1, 4,PF), 
pixbit( 2, 0, 1, 4,PG), pixbit( 2, 1, 1, 4,PH), pixbit( 2, 2, 1, 4,PI), image(3,3,[PA, PB, PC, PD,PE, PF, PG,PH, PI],I), imageCompress(I, I2,_).

*/
/*
pixrgb( 0, 0, 10, 10, 10, 10, P1), pixrgb( 0, 1, 20, 20, 20, 20, P2), pixrgb( 1, 0, 20, 20, 20, 30, P3), 
pixrgb( 1, 1, 40, 40, 40, 10, P4), image( 2, 2, [P1, P2, P3, P4], I), imageCompress(I,I2,_).

*/
/*
pixhex( 0, 0, "#CCCCCC", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#FFFFFF", 4, PG), pixhex( 2, 1, "#CCCCCC", 4, PH), pixhex( 2, 2, "#CCCCCC", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), 
imageCompress(I,I2,_).

*/

/*-------------------------------------EJEMPLOS IMAGE-CHANGE-PIXEL-----------------------------------------------------------------------*/
/*
pixhex( 0, 0, "#CC0000", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#B800DD", 4, PG), pixhex( 2, 1, "#DD1200", 4, PH), pixhex( 2, 2, "#DDCCFF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), 
pixhex( 2, 1, "#B800DD", 20, P8_modificado), imageChangePixel(I, P8_modificado, I2).

*/
/*
pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 0, 2 , 0, 30, PC), pixbit( 1, 0, 1, 4,PD),pixbit( 1, 1, 1, 4,PE), pixbit( 1, 2, 1, 4,PF), 
pixbit( 2, 0, 1, 4,PG), pixbit( 2, 1, 1, 4,PH), pixbit( 2, 2, 1, 4,PI), image(3,3,[PA, PB, PC, PD,PE, PF, PG,PH, PI],I), pixbit( 2, 1, 1, 20, PH_modificado), imageChangePixel(I, PH_modificado, I2).

*/
/*
pixrgb( 0, 0, 10, 10, 10, 10, P1), pixrgb( 0, 1, 20, 20, 20, 20, P2), pixrgb( 1, 0, 20, 20, 20, 30, P3), 
pixrgb( 1, 1, 40, 40, 40, 10, P4), image( 2, 2, [P1, P2, P3, P4], I),pixrgb( 1, 1, 6, 6, 6, 30, P4_modificado),imageChangePixel(I, P4_modificado, I2).

*/

/*-------------------------------------EJEMPLOS IMAGE-INVERT-COLOR-RGB-------------------------------------------------------------------*/
/*
pixrgb( 0, 0, 12, 7, 3, 10, P1), pixrgb( 0, 1, 250, 20, 25, 20, P2), pixrgb( 1, 0, 36, 36, 40, 30, P3), pixrgb( 1, 1, 40, 50, 46, 40, P4), image( 2, 2, [P1, P2, P3, P4], I1), 
imageInvertColorRGB(P2, P2_modificado), imageChangePixel(I1, P2_modificado, I2).

*/
/*
pixrgb( 0, 0, 6, 2, 4, 10, P1), pixrgb( 0, 1, 255, 255, 255, 20, P2), pixrgb( 1, 0, 255, 255, 255, 30, P3), pixrgb( 1, 1, 23,67,3, 40, P4), image( 2, 2, [P1, P2, P3, P4], I1), 
imageInvertColorRGB(P3, P3_modificado), imageChangePixel(I1, P3_modificado, I2).

*/
/*
pixrgb( 0, 0, 13, 37, 35, 10, P1), pixrgb( 0, 1, 254, 233, 245, 20, P2), pixrgb( 1, 0, 11, 36, 40, 30, P3), pixrgb( 1, 1, 0, 0, 46, 0, P4), image( 2, 2, [P1, P2, P3, P4], I1), 
imageInvertColorRGB(P2, P4_modificado), imageChangePixel(I1, P4_modificado, I2).

*/


/*-------------------------------------EJEMPLOS IMAGE-TO-STRING--------------------------------------------------------------------------*/
/*
pixhex( 0, 0, "#CC0000", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#B800DD", 4, PG), pixhex( 2, 1, "#DD1200", 4, PH), pixhex( 2, 2, "#DDCCFF", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), 
imageToString(I,A),write(A).

*/
/*
pixbit( 0, 0, 0, 14, PA), pixbit( 0, 1, 0, 7, PB), pixbit( 0, 2 , 0, 8, PC), pixbit( 1, 0, 1,13,PD),pixbit( 1, 1, 1, 86,PE), pixbit( 1, 2, 1, 56,PF), 
pixbit( 2, 0, 1, 34,PG), pixbit( 2, 1, 1, 13,PH), pixbit( 2, 2, 1, 26,PI), image(3,3,[PA, PB, PC, PD,PE, PF, PG,PH, PI],I), imageToString(I,A),write(A).

*/
% pixrgb(0, 0, 13, 18, 19, 10, PA), pixrgb(0, 1, 26, 220, 210, 130, PB), pixrgb(1, 0, 10, 140,110, 34, PC), pixrgb(1, 1, 110, 12, 132, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageToString(I,A),write(A).

/*-------------------------------------EJEMPLOS IMAGE-DEPTH-LAYERS-----------------------------------------------------------------------*/
% 4 imagenes 
% pixrgb(0, 0, 13, 18, 19, 10, PA), pixrgb(0, 1, 26, 220, 210, 130, PB), pixrgb(1, 0, 10, 140,110, 34, PC), pixrgb(1, 1, 110, 12, 132, 4, PD), image( 2, 2, [PA, PB, PC, PD], I), imageDepthLayers(I,Layers).

% 3 imagenes
% pixbit( 0, 0, 1, 5, PA), pixbit( 0, 1, 0, 6, PB), pixbit( 1, 0, 0, 6, PC), pixbit( 1, 1, 1, 13,PD), image( 2, 2, [PA, PB, PC, PD], I), imageDepthLayers(I,Layers).

% 2 imagenes
% pixhex( 0, 0, "#FF0D00", 30, PA), pixhex( 0, 1, "#FF00DD", 30, PB), pixhex( 1, 0, "#FF0D03", 5, PC), pixhex( 1, 1, "#FD00D0", 5, PD), image( 2, 2, [PA, PB, PC, PD], I), imageDepthLayers(I,Layers).

/*-------------------------------------EJEMPLOS IMAGE-DECOMPRESS-------------------------------------------------------------------------*/

/*
pixbit( 0, 0, 1, 10, PA), pixbit( 0, 1, 0, 20, PB), pixbit( 0, 2 , 0, 30, PC), pixbit( 1, 0, 1, 4,PD),pixbit( 1, 1, 1, 4,PE), pixbit( 1, 2, 1, 4,PF), 
pixbit( 2, 0, 1, 4,PG), pixbit( 2, 1, 1, 4,PH), pixbit( 2, 2, 1, 4,PI), image(3,3,[PA, PB, PC, PD,PE, PF, PG,PH, PI],I), imageCompress(I,I2,Z), imageDecompress(I2,Z,I3).

*/
/*
pixhex( 0, 0, "#CCCCCC", 20, PA), pixhex( 0, 1, "#FFFFFF", 20, PB), pixhex( 0, 2, "#DDDDDD", 20, PC), pixhex( 1, 0, "#1100FF", 30, PD), pixhex( 1, 1, "#0000DD", 4, PE), pixhex( 1, 2, "#0000FF", 4, PF), 
pixhex( 2, 0, "#FFFFFF", 4, PG), pixhex( 2, 1, "#CCCCCC", 4, PH), pixhex( 2, 2, "#CCCCCC", 4, PI), image( 3, 3, [PA, PB, PC, PD, PE, PF, PG, PH, PI], I), 
imageCompress(I,I2,Z), imageDecompress(I2,Z,I3).

*/
/*
pixrgb( 0, 0, 26,26,26, 20, PA), pixrgb( 0, 1, 26,26,26, 20, PB), pixrgb( 1,0 , 26,26,26, 20, PC), pixrgb( 1,1, 21,56,65, 30, PD),image(2, 2, [PA, PB, PC, PD], I), 
imageCompress(I,I2,Z), imageDecompress(I2,Z,I3).

*/