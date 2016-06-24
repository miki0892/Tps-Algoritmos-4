      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP-2.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TIEMPOS         ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE IS DYNAMIC
                                  RECORD KEY IS TIE-CLAVE
                                  ALTERNATE RECORD KEY IS TIE-CUIT
                                  WITH DUPLICATES
                                  FILE STATUS IS FS-TIEMPOS.

           SELECT CONSULTORES     ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE IS RANDOM
                                  RECORD KEY IS CONS-NUM
                                  FILE STATUS IS FS-CONSULTORES.

           SELECT EMPRESAS        ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE IS RANDOM
                                  RECORD KEY IS EMP-COD
                                  ALTERNATE RECORD KEY IS EMP-CUIT
                                  FILE STATUS IS FS-EMPRESAS.

           SELECT TARIFAS         ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE IS RANDOM
                                  RECORD KEY IS TAR-CLAVE
                                  FILE STATUS IS FS-TARIFAS.

           SELECT PARAMETROS      ASSIGN TO DISK
                                  ORGANIZATION IS SEQUENTIAL
                                  FILE STATUS IS FS-PARAMETROS.

           SELECT ARCHIVO-ORDENADO
                                  ASSIGN TO DISK
                                  SORT STATUS IS FS-ORDENADO.

           SELECT LISTADO         ASSIGN TO DISK
                                  ORGANIZATION IS SEQUENTIAL
                                  FILE STATUS IS FS-LISTADO.

       DATA DIVISION.
       FILE SECTION.

       FD TIEMPOS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Indexados/times.dat'.

       01 TIE-REG.
           03 TIE-CLAVE.
               05 TIE-NUM PIC X(5).
               05 TIE-FECHA.
                   07 TIE-FECHA-AAAA PIC 9(4).
                   07 TIE-FECHA-MM   PIC 9(2).
                   07 TIE-FECHA-DD   PIC 9(2).
               05 TIE-CUIT PIC 9(11).
           03 TIE-TAREA PIC X(4).
           03 TIE-HORAS PIC 9(2)V99.
           03 TIE-TIPO  PIC X(2).

       FD CONSULTORES LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Indexados/consultores.dat'.

       01 CONS-REG.
           03 CONS-NUM PIC X(5).
           03 CONS-DNI PIC 9(8).
           03 CONS-SRT PIC X(2).
           03 CONS-NOM PIC X(25).
           03 CONS-DIR PIC X(20).
           03 CONS-TEL PIC X(20).

       FD EMPRESAS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Indexados/empresas.dat'.

       01 EMP-REG.
           03 EMP-COD  PIC 9(3).
           03 EMP-RAZ  PIC X(25).
           03 EMP-DIR  PIC X(20).
           03 EMP-TEL  PIC X(20).
           03 EMP-CUIT PIC 9(11).

       FD TARIFAS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Indexados/tarifas.dat'.

       01 TAR-REG.
           03 TAR-CLAVE.
               05 TAR-SRT  PIC X(2).
               05 TAR-TIPO PIC 99.
           03 TAR-TAR PIC 9(5)V99.

       FD PARAMETROS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Secuenciales/parametros.dat'.

       01 REG-PARAM.
           03 PARAM-DESDE PIC 9(11).
           03 PARAM-HASTA PIC 9(11).

       SD ARCHIVO-ORDENADO
          DATA RECORD IS REG-ORDENADO.

       01 REG-ORDENADO.
           03 ORD-CLAVE.
               05 ORD-RAZON PIC X(25).
               05 ORD-CUIT PIC 9(11).
               05 ORD-FECHA.
                   07 ORD-FECHA-ANIO PIC 9(4).
                   07 ORD-FECHA-MES PIC 9(2).
                   07 ORD-FECHA-DIA PIC 9(2).
               05 ORD-NRO-CONS PIC X(5).
           03 ORD-NOMBRE PIC X(25).
           03 ORD-TARIFA PIC 9(5)V99.
           03 ORD-HORAS PIC 9(2)V99.

       FD LISTADO LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Secuenciales/listado.dat'.

       01 LINEA PIC X(80).

       WORKING-STORAGE SECTION.

       77 FS-TIEMPOS PIC XX.
       77 FS-CONSULTORES PIC XX.
       77 FS-EMPRESAS PIC XX.
       77 FS-TARIFAS PIC XX.
       77 FS-PARAMETROS PIC XX.
       77 FS-ORDENADO PIC XX.
       77 FS-LISTADO PIC XX.

       01 REP-LINEA1.
           02 FILLER PIC X(8) VALUE 'Fecha: '.
           02 REP-LINEA1-FECHA-DD PIC 9(2).
           02 FILLER PIC X(1) VALUE '/'.
           02 REP-LINEA1-FECHA-MM PIC 9(2).
           02 FILLER PIC X(1) VALUE '/'.
           02 REP-LINEA1-FECHA-AAAA PIC 9(4).
           02 FILLER PIC X(54) VALUE SPACES.
           02 FILLER PIC X(5) VALUE 'Hoja '.
           02 REP-LINEA1-HOJAS PIC 9(3) VALUE ZERO.

       01 REP-TITULO.
           02 FILLER PIC X(27) VALUE SPACES.
           02 PARTE-1 PIC X(27) VALUE 'Horas Aplicadas por Empresa'.
           02 FILLER PIC X(27) VALUE SPACES.

       01 REP-RAZON-EMP.
           02 FILLER PIC X(9) VALUE 'Empresa: '.
           02 REP-RAZON PIC X(25).
           02 FILLER PIC X(46) VALUE SPACES.

       01 REP-CUIT-EMP.
           02 FILLER PIC X(6) VALUE 'Cuit: '.
           02 REP-NRO-CUIT PIC 9(11) VALUE ZERO.
           02 FILLER PIC X(63) VALUE SPACES.

       01 REP-HEADER-TABLA.
           02 FILLER PIC X(2) VALUE SPACES.
           02 FILLER PIC X(8) VALUE 'Fecha'.
           02 FILLER PIC X(13) VALUE 'Consultor'.
           02 FILLER PIC X(28) VALUE 'Nombre'.
           02 FILLER PIC X(9) VALUE 'Tarifa'.
           02 FILLER PIC X(11) VALUE 'Horas'.
           02 FILLER PIC X(9) VALUE 'Importe'.

       01 REP-FILA.
           02 REP-FILA-DIA PIC X(2) VALUE ZERO.
           02 FILLER PIC X VALUE '/'.
           02 REP-FILA-MES PIC X(2) VALUE ZERO.
           02 FILLER PIC X VALUE '/'.
           02 REP-FILA-ANIO PIC X(4) VALUE ZERO.
           02 FILLER PIC X(2) VALUE SPACES.

           02 REP-FILA-CONS PIC X(5) VALUE SPACES.
           02 FILLER PIC X(5) VALUE SPACES.

           02 REP-FILA-NOMB PIC X(25) VALUE SPACES.
           02 FILLER PIC XX VALUE SPACES.

           02 REP-FILA-TAR PIC Z(4)9,99 VALUE ZERO.
           02 FILLER PIC X(3) VALUE SPACES.

           02 REP-FILA-HORAS PIC Z9,99 VALUE ZERO.
           02 FILLER PIC X(3) VALUE SPACES.

           02 REP-FILA-IMPORTE PIC Z(6)9,99 VALUE ZERO.
           02 FILLER PIC XXX VALUE SPACES.

           02 REP-TABLA-IMPORTE PIC Z(6)9,99 VALUE ZERO.

       01 REP-TOTALES-FECHA.
           02 FILLER PIC X(17) VALUE 'Totales por fecha'.
           02 FILLER PIC X(42) VALUE SPACES.
           02 REP-TOTAL-FECHA-HS PIC ZZ9,99 VALUE ZERO.
           02 FILLER PIC X(2) VALUE SPACES.
           02 REP-TOTAL-FECHA-IMP PIC Z(7)9,99.

       01 REP-TOTALES-EMP.
           02 FILLER PIC X(19) VALUE 'Totales por Empresa'.
           02 FILLER PIC X(39) VALUE SPACES.
           02 REP-TOTAL-EMP-HS PIC ZZZ9,99 VALUE ZERO.
           02 FILLER PIC X VALUE SPACES.
           02 REP-TOTAL-EMP-IMP PIC Z(8)9,99.

       01 REP-TOTALES-GRAL.
           02 FILLER PIC X(21) VALUE 'Total general'.
           02 FILLER PIC X(44) VALUE SPACES.
           02 REP-TOTAL-GRAL-IMPORTE PIC Z(9)9,99 VALUE ZERO.

       01 LINEA-VACIA PIC X(80) VALUE SPACES.
       01 LINEA-GUION PIC X(80) VALUE ALL "-".

       01 LINEA-TOTALES-FECHA.
           02 FILLER PIC X(60) VALUE SPACES.
           02 FILLER PIC X(20) VALUE ALL "-".

       01 CANT-LINEAS PIC 99.

       01 RAZON-ANT PIC X(25).
       01 FECHA-ANT PIC 9(8).

       01 IMP-ACTUAL PIC 9(7)V99.

       01 TOTAL-FECHA-HS PIC 9(3)V99.
       01 TOTAL-FECHA-IMP PIC 9(8)V99.
       01 TOTAL-EMP-HS PIC 9(4)V99.
       01 TOTAL-EMP-IMP PIC 9(9)V99.
       01 TOTAL-GRAL-IMP PIC 9(10)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            SORT ARCHIVO-ORDENADO
            ON ASCENDING KEY ORD-CLAVE
            INPUT PROCEDURE IS PROCESO-ENTRADA
            OUTPUT PROCEDURE IS PROCESO-SALIDA.
            STOP RUN.

       PROCESO-ENTRADA SECTION.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM LEER-PARAMETROS.
           PERFORM PROCESAR-TIEMPOS.
           PERFORM CERRAR-ARCHIVOS.

       RUTINAS-ENTRADA SECTION.

           ABRIR-ARCHIVOS.
               OPEN INPUT TIEMPOS.
               IF FS-TIEMPOS NOT = ZERO
                   DISPLAY "ERROR AL ABRIR TIMES FS: " FS-TIEMPOS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.
               OPEN INPUT CONSULTORES.
               IF FS-CONSULTORES NOT = ZERO
                   DISPLAY "ERROR AL ABRIR CONSULTORES FS: "
                   FS-CONSULTORES
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.
               OPEN INPUT EMPRESAS.
               IF FS-EMPRESAS NOT = ZERO
                   DISPLAY "ERROR AL ABRIR EMPRESAS FS: " FS-EMPRESAS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.
               OPEN INPUT TARIFAS.
               IF FS-TARIFAS NOT = ZERO
                   DISPLAY "ERROR AL ABRIR TARIFAS FS: " FS-TARIFAS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.
               OPEN INPUT PARAMETROS.
               IF FS-PARAMETROS NOT = ZERO
                   DISPLAY "ERROR AL ABRIR PARAMETROS FS: "
                   FS-PARAMETROS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.

           LEER-PARAMETROS.
               READ PARAMETROS.
               IF FS-PARAMETROS NOT = ZERO AND '10'
                   DISPLAY "ERROR AL LEER PARAMETROS FS: " FS-PARAMETROS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.

           LEER-TIEMPOS.
               READ TIEMPOS NEXT RECORD.
               IF FS-TIEMPOS NOT = ZERO AND '10'
                   DISPLAY "ERROR AL LEER TIEMPOS FS: " FS-TIEMPOS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.

           LEER-CONSULTORES.
               READ CONSULTORES.
               IF FS-CONSULTORES NOT = ZERO AND '10'
                   DISPLAY "ERROR AL LEER CONSULTORES FS: "
                   FS-CONSULTORES
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.

           LEER-EMPRESAS.
               READ EMPRESAS RECORD.
               IF FS-EMPRESAS NOT = ZERO AND '10'
                   DISPLAY "ERROR AL LEER EMPRESAS FS: " FS-EMPRESAS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.

           LEER-TARIFAS.
               READ TARIFAS.
               IF FS-TARIFAS NOT = ZERO AND '10'
                   DISPLAY "ERROR AL LEER TARIFAS FS: " FS-TARIFAS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.

           PROCESAR-TIEMPOS.
               MOVE PARAM-DESDE TO TIE-CUIT.
               START TIEMPOS KEY >= TIE-CUIT.
               IF FS-TIEMPOS = '00'
                   READ TIEMPOS NEXT RECORD
                   PERFORM CARGAR-ORDENADO UNTIL FS-TIEMPOS = '10'
                                           OR TIE-CUIT > PARAM-HASTA.


           CARGAR-ORDENADO.
               MOVE TIE-NUM TO ORD-NRO-CONS.
               MOVE TIE-FECHA TO ORD-FECHA.
               MOVE TIE-CUIT TO ORD-CUIT.
               MOVE TIE-HORAS TO ORD-HORAS.
               PERFORM BUSCAR-RAZON-SOCIAL.
               PERFORM BUSCAR-NOMB-CONS.
               PERFORM BUSCAR-TARIFA.
               RELEASE REG-ORDENADO.
               PERFORM LEER-TIEMPOS.

           BUSCAR-RAZON-SOCIAL.
               MOVE TIE-CUIT TO EMP-CUIT.
               READ EMPRESAS RECORD KEY IS EMP-CUIT.
               IF FS-EMPRESAS = '00'
                   MOVE EMP-RAZ TO ORD-RAZON.

           BUSCAR-NOMB-CONS.
               MOVE TIE-NUM TO CONS-NUM.
               PERFORM LEER-CONSULTORES.
               IF FS-CONSULTORES = '00'
                   MOVE CONS-NOM TO ORD-NOMBRE.

           BUSCAR-TARIFA.
               IF FS-CONSULTORES = '00'
                   MOVE CONS-SRT TO TAR-SRT.
               MOVE TIE-TIPO TO TAR-TIPO.
               PERFORM LEER-TARIFAS.
               IF FS-TARIFAS = '00'
                   MOVE TAR-TAR TO ORD-TARIFA.


           CERRAR-ARCHIVOS.
               CLOSE TIEMPOS CONSULTORES EMPRESAS TARIFAS PARAMETROS.



       PROCESO-SALIDA SECTION.
           PERFORM ABRIR-LISTADO.
           MOVE 1 TO REP-LINEA1-HOJAS.
           RETURN ARCHIVO-ORDENADO END MOVE '10' TO FS-ORDENADO.
           MOVE 0 TO TOTAL-GRAL-IMP.
           PERFORM PROCESAR-ORDENADO UNTIL FS-ORDENADO = '10'.
           PERFORM IMPRIMIR-TOTAL-GENERAL.
           CLOSE LISTADO.

       RUTINAS-SALIDA SECTION.
           ABRIR-LISTADO.
               OPEN OUTPUT LISTADO.
               IF FS-LISTADO NOT = ZERO
                   DISPLAY "ERROR AL ABRIR LISTADO FS: " FS-LISTADO
                   CLOSE LISTADO
                   STOP RUN.

           PROCESAR-ORDENADO.
               PERFORM COMENZAR-NUEVA-HOJA.
               MOVE 0 TO TOTAL-EMP-HS TOTAL-EMP-IMP.
               MOVE ORD-RAZON TO RAZON-ANT.
               PERFORM PROCESAR-RAZONES UNTIL FS-ORDENADO = '10' OR
                                              RAZON-ANT <> ORD-RAZON.
               PERFORM IMPRIMIR-TOTALES-EMPRESA.




           COMENZAR-NUEVA-HOJA.
               PERFORM IMPRIMIR-LINEA-1.
               PERFORM IMPRIMIR-LINEA-TITULO.
               PERFORM IMPRIMIR-LINEA-EMPRESA.
               ADD 1 TO REP-LINEA1-HOJAS.
               MOVE 6 TO CANT-LINEAS.

           IMPRIMIR-LINEA-1.
               MOVE FUNCTION CURRENT-DATE(7:2) TO REP-LINEA1-FECHA-DD.
               MOVE FUNCTION CURRENT-DATE(5:2) TO REP-LINEA1-FECHA-MM.
               MOVE FUNCTION CURRENT-DATE(1:4) TO REP-LINEA1-FECHA-AAAA.
               WRITE LINEA FROM REP-LINEA1 AFTER 1.

           IMPRIMIR-LINEA-TITULO.
               WRITE LINEA FROM REP-TITULO AFTER 2.

           IMPRIMIR-LINEA-EMPRESA.
               MOVE ORD-RAZON TO REP-RAZON.
               MOVE ORD-CUIT TO REP-NRO-CUIT.
               WRITE LINEA FROM REP-RAZON-EMP AFTER 1.
               WRITE LINEA FROM REP-CUIT-EMP AFTER 1.

           PROCESAR-RAZONES.
               PERFORM IMPRIMIR-HEADER-TABLA.
               MOVE 0 TO TOTAL-FECHA-HS TOTAL-FECHA-IMP.
               MOVE ORD-FECHA TO FECHA-ANT.
               PERFORM PROCESAR-FECHAS UNTIL FS-ORDENADO = '10' OR
                                             RAZON-ANT <> ORD-RAZON OR
                                             FECHA-ANT <> ORD-FECHA.
               PERFORM IMPRIMIR-TOTALES-FECHA.



           IMPRIMIR-HEADER-TABLA.
               PERFORM CHEQUEAR-CANT-LINEAS.
               WRITE LINEA FROM REP-HEADER-TABLA AFTER 2.
               ADD 2 TO CANT-LINEAS.
               PERFORM CHEQUEAR-CANT-LINEAS.
               WRITE LINEA FROM LINEA-GUION AFTER 1.
               ADD 1 TO CANT-LINEAS.

           CHEQUEAR-CANT-LINEAS.
               IF CANT-LINEAS >= 60
                   PERFORM COMENZAR-NUEVA-HOJA.

           PROCESAR-FECHAS.
               PERFORM CHEQUEAR-CANT-LINEAS.
               PERFORM IMPRIMIR-FILA-TABLA.
               PERFORM ACTUALIZAR-TOTALES.
               RETURN ARCHIVO-ORDENADO END MOVE '10' TO FS-ORDENADO.


           IMPRIMIR-FILA-TABLA.
               MOVE ORD-FECHA-DIA TO REP-FILA-DIA.
               MOVE ORD-FECHA-MES TO REP-FILA-MES.
               MOVE ORD-FECHA-ANIO TO REP-FILA-ANIO.
               MOVE ORD-NRO-CONS TO REP-FILA-CONS.
               MOVE ORD-NOMBRE TO REP-FILA-NOMB.
               MOVE ORD-TARIFA TO REP-FILA-TAR.
               MOVE ORD-HORAS TO REP-FILA-HORAS.
               MULTIPLY ORD-TARIFA BY ORD-HORAS GIVING IMP-ACTUAL.
               MOVE IMP-ACTUAL TO REP-FILA-IMPORTE.
               WRITE LINEA FROM REP-FILA AFTER 1.
               ADD 1 TO CANT-LINEAS.

           ACTUALIZAR-TOTALES.
               ADD ORD-HORAS  TO TOTAL-FECHA-HS TOTAL-EMP-HS.
               ADD IMP-ACTUAL TO TOTAL-FECHA-IMP TOTAL-EMP-IMP
                                 TOTAL-GRAL-IMP.


           IMPRIMIR-TOTALES-FECHA.
              PERFORM CHEQUEAR-CANT-LINEAS.
              MOVE TOTAL-FECHA-HS TO REP-TOTAL-FECHA-HS.
              MOVE TOTAL-FECHA-IMP TO REP-TOTAL-FECHA-IMP.
              WRITE LINEA FROM LINEA-TOTALES-FECHA AFTER 1.
              WRITE LINEA FROM REP-TOTALES-FECHA AFTER 1.
              ADD 2 TO CANT-LINEAS.


           IMPRIMIR-TOTALES-EMPRESA.
               PERFORM CHEQUEAR-CANT-LINEAS.
               MOVE TOTAL-EMP-HS TO REP-TOTAL-EMP-HS.
               MOVE TOTAL-EMP-IMP TO REP-TOTAL-EMP-IMP.
               WRITE LINEA FROM REP-TOTALES-EMP AFTER 1.
               ADD 2 TO CANT-LINEAS.

           IMPRIMIR-TOTAL-GENERAL.
               PERFORM CHEQUEAR-CANT-LINEAS.
               MOVE TOTAL-GRAL-IMP TO REP-TOTAL-GRAL-IMPORTE.
               WRITE LINEA FROM REP-TOTALES-GRAL AFTER 1.
               ADD 1 TO CANT-LINEAS.

       END PROGRAM TP-2.
