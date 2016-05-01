      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP1-PUNTO-A.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NOVTIMES1    ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-NOVTIMES1.

           SELECT NOVTIMES2    ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-NOVTIMES2.

           SELECT NOVTIMES3    ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-NOVTIMES3.

           SELECT TIEMPOS      ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-TIEMPOS.

           SELECT TIEMPOS-NEW  ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-TIEMPOS-NEW.

           SELECT CONSULTORES  ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-CONSULTORES.

           SELECT EMPRESAS     ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-EMPRESAS.

           SELECT TARIFAS      ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-TARIFAS.

           SELECT LISTADO ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-LISTADO.

       DATA DIVISION.
       FILE SECTION.
       FD NOVTIMES1 LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algori
      -             'tmos-4/Tp1/Archivos de prueba/novTimes1.dat'.

       01 NOV1-REG    .
           03 NOV1-NUMERO PIC X(5).
           03 NOV1-FECHA.
               05 DIA PIC 9(2).
               05 MES PIC 9(2).
               05 ANIO PIC 9(4).
           03 NOV1-EMPRESA PIC 9(03).
           03 NOV1-TAREA PIC X(04).
           03 NOV1-HORAS PIC 9(2)V99.
           03 NOV1-TIPO PIC 9.


       FD NOVTIMES2 LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algori
      -             'tmos-4/Tp1/Archivos de prueba/novTimes2.dat'.
       01 NOV2-REG    .
           03 NOV2-NUMERO PIC X(5).
           03 NOV2-FECHA.
               05 DIA PIC 9(2).
               05 MES PIC 9(2).
               05 ANIO PIC 9(4).
           03 NOV2-EMPRESA PIC 9(03).
           03 NOV2-TAREA PIC X(04).
           03 NOV2-HORAS PIC 9(2)V99.
           03 NOV2-TIPO PIC 9.

       FD NOVTIMES3 LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algori
      -             'tmos-4/Tp1/Archivos de prueba/novTimes3.dat'.
       01 NOV3-REG    .
           03 NOV3-NUMERO PIC X(5).
           03 NOV3-FECHA.
               05 DIA PIC 9(2).
               05 MES PIC 9(2).
               05 ANIO PIC 9(4).
           03 NOV3-EMPRESA PIC 9(03).
           03 NOV3-TAREA PIC X(04).
           03 NOV3-HORAS PIC 9(2)V99.
           03 NOV3-TIPO PIC 9.

       FD TIEMPOS LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algorit
      -            'mos-4/Tp1/Archivos de prueba/times.dat'.
       01 TIE-REG.
           03 TIE-NUMERO PIC X(5).
           03 TIE-FECHA.
               05 DIA PIC 9(2).
               05 MES PIC 9(2).
               05 ANIO PIC 9(4).
           03 TIE-EMPRESA PIC 9(03).
           03 TIE-TAREA PIC X(04).
           03 TIE-HORAS PIC 9(2)V99.

       FD TIEMPOS-NEW LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algorit
      -            'mos-4/Tp1/Archivos de prueba/timesNew.dat'.
       01 TIE-NEW-REG.
           03 TIE-NEW-NUMERO PIC X(5).
           03 TIE-NEW-REG-FECHA.
               05 DIA PIC 9(2).
               05 MES PIC 9(2).
               05 ANIO PIC 9(4).
           03 TIE-NEW-EMPRESA PIC 9(03).
           03 TIE-NEW-TAREA PIC X(04).
           03 TIE-NEW-HORAS PIC 9(2)V99.

       FD EMPRESAS LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algorit
      -            'mos-4/Tp1/Archivos de prueba/empresas.dat'.
       01 EMP-REG.
           03 EMP-CODIGO PIC 9(03).
           03 EMP-RAZON PIC X(25).
           03 EMP-DIRE PIC X(20).
           03 EMP-TEL PIC X(20).
           03 EMP-CUIT PIC 9(11).

       FD CONSULTORES LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algorit
      -            'mos-4/Tp1/Archivos de prueba/consultores.dat'.
       01 CONS-REG.
           03 CONS-NUMERO PIC X(5).
           03 CONS-DNI PIC 9(8).
           03 CONS-SRT PIC X(2).
           03 CONS-NOMBRE PIC X(25).
           03 CONS-DIRE PIC X(20).
           03 CONS-TEL PIC X(20).

       FD TARIFAS LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algorit
      -            'mos-4/Tp1/Archivos de prueba/tarifas.dat'.
       01 TAR-REG.
           03 TAR-SRT PIC X(02).
           03 TAR-TIPO PIC 9.
           03 TAR-TARIFA PIC 9(5)V99.

       FD LISTADO LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algoritm
      -           'os-4/Tp1/Archivos de prueba/listado.dat'.
       01 LINEA PIC X(80).

       WORKING-STORAGE SECTION.

       77 FS-CONSULTORES PIC XX.
       77 FS-TIEMPOS     PIC XX.
       77 FS-TIEMPOS-NEW PIC XX.
       77 FS-NOVTIMES1   PIC XX.
       77 FS-NOVTIMES2   PIC XX.
       77 FS-NOVTIMES3   PIC XX.
       77 FS-EMPRESAS    PIC XX.
       77 FS-TARIFAS     PIC XX.
       77 FS-LISTADO     PIC XX.

       01 TABLA-EMPRESAS.
           03 EMPRESA OCCURS 3 TIMES INDEXED BY IND-EMP.
               05 NUMERO-EMP PIC 9(03).
               05 RAZON-SOCIAL PIC X(25).

       01 TABLA-TARIFAS.
           03 ELEMENTO OCCURS 90 TIMES INDEXED BY IND-TAR.
               05 CATEGORIA PIC X(02).
               05 TIPO-TAR PIC 9.
               05 TARIFA PIC 9(5)V99.

       01 ARCHIVO-MINIMO PIC 9.

       01 TABLA-FS.
           03 FS-ARCHIVOS PIC 9 OCCURS 3 TIMES VALUE ZERO.

       01 REG-MIN.
           03 NUMERO PIC X(5).
           03 FECHA.
               05 DIA PIC 9(2).
               05 MES PIC 9(2).
               05 ANIO PIC 9(4).
           03 EMPRESA PIC 9(03).
           03 TAREA PIC X(04).
           03 HORAS PIC 9(2)V99.
           03 TIPO PIC 9.

       01 REG-MIN-ANT.
           03 NUMERO PIC X(5).
           03 FECHA PIC 9(8).
           03 EMPRESA PIC 9(03).
           03 TAREA PIC X(04).
           03 HORAS PIC 9(2)V99.
           03 TIPO PIC 9.

       01 TOTAL-GRAL-IMPORTE PIC Z(9)9V99 VALUE ZERO.
       01 TOTAL-HOJAS PIC 9(3) VALUE ZEROES.
       01 TOTAL-CONS-IMPORTE PIC 9(8)9V99 VALUE ZERO.
       01 TOTAL-CONS-HS PIC 9(3)9V99 VALUE ZERO.
       01 TOTAL-FECHA-IMPORTE PIC 9(7)9V99 VALUE ZERO.
       01 TOTAL-FECHA-HS PIC 999V99 VALUE ZERO.
       01 TOTAL-LINEAS PIC 99 VALUE ZERO.
       01 LINEAS-TABLA PIC 999 VALUE ZERO.

       01 FECHA-INV1.
           03 ANIO PIC 9(4).
           03 MES PIC 9(2).
           03 DIA PIC 9(2).
       01 FECHA-INV2.
           03 ANIO PIC 9(4).
           03 MES PIC 9(2).
           03 DIA PIC 9(2).

       01 REP-LINEA1.
           02 FILLER PIC X(8) VALUE 'Fecha: '.
           02 REP-LINEA1-FECHA-DD PIC 9(2).
           02 FILLER PIC X(1) VALUE '/'.
           02 REP-LINEA1-FECHA-MM PIC 9(2).
           02 FILLER PIC X(1) VALUE '/'.
           02 REP-LINEA1-FECHA-AAAA PIC 9(4).
           02 FILLER PIC X(50) VALUE SPACES.
           02 REP-LINEA1-HOJA PIC 9(3) VALUE ZERO.

       01 REP-TITULO.
           02 FILLER PIC X(27) VALUE SPACES.
           02 PARTE-1 PIC X(26) VALUE 'Listado de horas aplicadas'.
           02 FILLER PIC X(27) VALUE SPACES.

       01 REP-CONSULTOR-1.
           02 FILLER PIC X(11) VALUE 'Consultor: '.
           02 CONS-NUM PIC X(5) VALUE ZERO.
           02 FILLER PIC X(10) VALUE SPACES.
           02 FILLER PIC X(8) VALUE 'Nombre: '.
           02 CONS-NOM PIC X(25) VALUE SPACES.
           02 FILLER PIC X(21) VALUE SPACES.

       01 REP-CONSULTOR-2.
           02 FILLER PIC X(26) VALUE SPACES.
           02 FILLER PIC X(12) VALUE 'Categoría: '.
           02 CONS-CAT PIC X(20) VALUE SPACES.
           02 FILLER PIC X(22) VALUE SPACES.

       01 REP-HEADER-TABLA.
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(5) VALUE 'Fecha'.
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(7) VALUE 'Empresa'.
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(12) VALUE 'Razon social'.
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(4) VALUE 'Tipo'.
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(6) VALUE 'Tarifa'.
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(5) VALUE 'Horas'.
           02 FILLER PIC X(4) VALUE SPACES.
           02 FILLER PIC X(7) VALUE 'Importe'.

       01 REP-FILA-TABLA.
           02 FILLER PIC X VALUE SPACES.
           02 REP-TABLA-FECHA PIC X(8) VALUE ZERO.
           02 FILLER PIC X(3) VALUE SPACES.
           02 REP-TABLA-EMPRESA PIC 9(3) VALUE ZERO.
           02 FILLER PIC X(3) VALUE SPACES.
           02 REP-TABLA-RS PIC X(25) VALUE SPACES.
           02 FILLER PIC X(2) VALUE SPACES.
           02 REP-TABLA-TIPO PIC X(10) VALUE SPACES.
           02 FILLER PIC X(1) VALUE SPACES.
           02 REP-TABLA-TARIFA PIC Z(3)9V99 VALUE ZERO.
           02 FILLER PIC X(1) VALUE SPACES.
           02 REP-TABLA-HS PIC Z9V99 VALUE ZERO.
           02 FILLER PIC X(1) VALUE SPACES.
           02 REP-TABLA-IMPORTE PIC Z(6)9V99 VALUE ZERO.

       01 REP-TOTALES-FECHA.
           02 FILLER PIC X(17) VALUE 'Totales por fecha'.
           02 FILLER PIC X(43) VALUE SPACES.
           02 REP-TOTAL-FECHA-HS PIC ZZ9V99 VALUE ZERO.
           02 FILLER PIC X(3) VALUE SPACES.
           02 REP-TOTAL-FECHA-IMP PIC Z(7)9V99.

       01 REP-TOTALES-CONS.
           02 FILLER PIC X(21) VALUE 'Totales por Consultor'.
           02 FILLER PIC X(38) VALUE SPACES.
           02 REP-TOTAL-CONS-HS PIC ZZZ9V99 VALUE ZERO.
           02 FILLER PIC X(2) VALUE SPACES.
           02 REP-TOTAL-CONS-IMP PIC Z(8)9V99.

       01 REP-TOTALES-GRAL.
           02 FILLER PIC X(21) VALUE 'Total general'.
           02 FILLER PIC X(46) VALUE SPACES.
           02 REP-TOTAL-GRAL-IMPORTE PIC Z(9)9V99 VALUE ZERO.

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM LEER-ARCHIVOS.
           PERFORM CARGAR-TABLAS.
           PERFORM BUSCAR-CLAVE-MINIMA.
           MOVE REG-MIN TO REG-MIN-ANT.
           PERFORM AVANZAR-CONSULTOR.
           PERFORM PROCESAMIENTO-GRAL UNTIL FS-NOVTIMES1 = 10
               AND FS-NOVTIMES2 = 10
               AND FS-NOVTIMES3 = 10.
           PERFORM IMPRIMIR-TOTAL-GRAL.
           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.

      *******************************************************************
       ABRIR-ARCHIVOS.
           OPEN INPUT TIEMPOS.
           IF FS-TIEMPOS NOT = ZERO
               DISPLAY "ERROR AL ABRIR TIMES FS: " FS-TIEMPOS
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN OUTPUT TIEMPOS-NEW.
           IF FS-TIEMPOS-NEW NOT = ZERO
               DISPLAY "ERROR AL ABRIR TIMES NEW FS: " FS-TIEMPOS-NEW
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN INPUT NOVTIMES1.
           IF FS-NOVTIMES1 NOT = ZERO
               DISPLAY "ERROR AL ABRIR NOVTIMES1 FS: " FS-NOVTIMES1
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN INPUT NOVTIMES2.
           IF FS-NOVTIMES2 NOT = ZERO
               DISPLAY "ERROR AL ABRIR NOVTIMES2 FS: " FS-NOVTIMES2
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN INPUT NOVTIMES3.
           IF FS-NOVTIMES3 NOT = ZERO
               DISPLAY "ERROR AL ABRIR NOVTIMES3 FS: " FS-NOVTIMES3
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN INPUT CONSULTORES.
           IF FS-CONSULTORES NOT = ZERO
               DISPLAY "ERROR AL ABRIR CONSULTORES FS: " FS-CONSULTORES
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN INPUT EMPRESAS.
           IF FS-NOVTIMES3 NOT = ZERO
               DISPLAY "ERROR AL ABRIR EMPRESAS FS: " FS-EMPRESAS
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN INPUT TARIFAS.
           IF FS-TARIFAS NOT = ZERO
               DISPLAY "ERROR AL ABRIR TARIFAS FS: " FS-TARIFAS
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN OUTPUT LISTADO.
           IF FS-LISTADO NOT = ZERO
               DISPLAY "ERROR AL ABRIR LISTADO FS: " FS-LISTADO
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.

      *******************************************************************
       LEER-ARCHIVOS.
           PERFORM LEER-TIEMPOS.
           PERFORM LEER-NOVTIMES1.
           PERFORM LEER-NOVTIMES2.
           PERFORM LEER-NOVTIMES3.
           PERFORM LEER-CONSULTORES.
           PERFORM LEER-EMPRESAS.
           PERFORM LEER-TARIFAS.

       LEER-TIEMPOS.
           READ TIEMPOS.
           IF FS-TIEMPOS NOT = ZERO AND 10
               DISPLAY "ERROR AL LEER TIMES FS: " FS-TIEMPOS
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.

       LEER-NOVTIMES1.
           READ NOVTIMES1.
           IF FS-NOVTIMES1 NOT = ZERO AND 10
               DISPLAY "ERROR AL LEER NOVTIMES1 FS: " FS-NOVTIMES1
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           IF FS-NOVTIMES1 = 10
               MOVE 1 TO FS-ARCHIVOS(1).

       LEER-NOVTIMES2.
           READ NOVTIMES2.
           IF FS-NOVTIMES2 NOT = ZERO AND 10
               DISPLAY "ERROR AL LEER NOVTIMES2 FS: " FS-NOVTIMES2
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           IF FS-NOVTIMES2 = 10
               MOVE 1 TO FS-ARCHIVOS(2).

       LEER-NOVTIMES3.
           READ NOVTIMES3.
           IF FS-NOVTIMES3 NOT = ZERO AND 10
               DISPLAY "ERROR AL LEER NOVTIMES3 FS: " FS-NOVTIMES3
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           IF FS-NOVTIMES3 = 10
               MOVE 1 TO FS-ARCHIVOS(3).

       LEER-CONSULTORES.
           READ CONSULTORES.
           IF FS-CONSULTORES NOT = ZERO AND 10
               DISPLAY "ERROR AL LEER CONSULTORES FS: "
      -            FS-CONSULTORES
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.

       LEER-EMPRESAS.
           READ EMPRESAS.
           IF FS-EMPRESAS NOT = ZERO AND 10
               DISPLAY "ERROR AL LEER EMPRESAS FS: " FS-EMPRESAS
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.

       LEER-TARIFAS.
           READ TARIFAS.
           IF FS-TARIFAS NOT = ZERO AND 10
               DISPLAY "ERROR AL LEER TARIFAS FS: " FS-TARIFAS
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.

      *******************************************************************
       CARGAR-TABLAS.
           PERFORM CARGAR-TABLA-EMPRESAS.
           PERFORM CARGAR-TABLA-TARIFAS.

       CARGAR-TABLA-EMPRESAS.
           PERFORM CARGAR-EMPRESAS VARYING IND-EMP FROM 1 BY 1
               UNTIL FS-EMPRESAS = '10'.

       CARGAR-EMPRESAS.
           MOVE EMP-CODIGO TO NUMERO-EMP(IND-EMP).
           MOVE EMP-RAZON TO RAZON-SOCIAL(IND-EMP).
           PERFORM LEER-EMPRESAS.

       CARGAR-TABLA-TARIFAS.
           PERFORM CARGAR-TARIFAS VARYING IND-TAR FROM 1 BY 1
               UNTIL FS-TARIFAS = '10'.

       CARGAR-TARIFAS.
           MOVE TAR-SRT TO CATEGORIA(IND-TAR).
           MOVE TAR-TIPO TO TIPO-TAR(IND-TAR).
           MOVE TAR-TARIFA TO TARIFA(IND-TAR).
           PERFORM LEER-TARIFAS.

      *******************************************************************
       BUSCAR-CLAVE-MINIMA.
           MOVE 1 TO ARCHIVO-MINIMO.
           MOVE NOV1-REG     TO REG-MIN.

           MOVE ANIO IN REG-MIN TO ANIO IN FECHA-INV1.
           MOVE MES IN REG-MIN TO MES IN FECHA-INV1.
           MOVE DIA IN REG-MIN TO DIA IN FECHA-INV1.

           MOVE ANIO IN NOV2-FECHA TO ANIO IN FECHA-INV2.
           MOVE MES IN NOV2-FECHA TO MES IN FECHA-INV2.
           MOVE DIA IN NOV2-FECHA TO DIA IN FECHA-INV2.

           IF NOV2-NUMERO < NUMERO IN REG-MIN
               MOVE NOV2-REG TO REG-MIN
               MOVE 2 TO ARCHIVO-MINIMO
           ELSE
               IF (NOV2-NUMERO = NUMERO IN REG-MIN) AND
               (FECHA-INV2 < FECHA-INV1)
                   MOVE NOV2-REG     TO REG-MIN
                   MOVE 2 TO ARCHIVO-MINIMO.

           MOVE ANIO IN REG-MIN TO ANIO IN FECHA-INV1.
           MOVE MES IN REG-MIN TO MES IN FECHA-INV1.
           MOVE DIA IN REG-MIN TO DIA IN FECHA-INV1.

           MOVE ANIO IN NOV3-FECHA TO ANIO IN FECHA-INV2.
           MOVE MES IN NOV3-FECHA TO MES IN FECHA-INV2.
           MOVE DIA IN NOV3-FECHA TO DIA IN FECHA-INV2.

           IF NOV3-NUMERO < NUMERO IN REG-MIN
               MOVE NOV3-REG TO REG-MIN
               MOVE 3 TO ARCHIVO-MINIMO
           ELSE
               IF (NOV3-NUMERO = NUMERO IN REG-MIN) AND
               (FECHA-INV2 < FECHA-INV1)
                   MOVE NOV3-REG     TO REG-MIN
                   MOVE 3 TO ARCHIVO-MINIMO.

      *******************************************************************
       AVANZAR-CONSULTOR.
           PERFORM LEER-CONSULTORES UNTIL
               (CONS-NUMERO >= NUMERO IN REG-MIN) OR
               FS-CONSULTORES = 10.

      *******************************************************************
       PROCESAMIENTO-GRAL.
           PERFORM AVANZAR-CONSULTOR-DEL-TIMES.
           PERFORM SALTAR-PAGINA.
           PERFORM IMPRIMIR-DATOS-CONSULTOR.
           PERFORM INICIALIZAR-TOTALES-CONSULTOR.
           PERFORM PROCESAMIENTO-CONSULTOR
               UNTIL (NUMERO IN REG-MIN <> NUMERO IN REG-MIN-ANT)
               OR (FS-ARCHIVOS(ARCHIVO-MINIMO) = 1).
           PERFORM IMPRIMIR-TOTAL-CONSULTOR.
           PERFORM AVANZAR-CONSULTOR.

      *******************************************************************
       AVANZAR-CONSULTOR-DEL-TIMES.
           PERFORM AVANZAR-TIMES UNTIL TIE-NUMERO >= NUMERO IN REG-MIN
               OR FS-TIEMPOS = 10.

       AVANZAR-TIMES.
           WRITE TIE-NEW-REG FROM TIE-REG.
           PERFORM LEER-TIEMPOS.

      *******************************************************************
       SALTAR-PAGINA.
           ADD 1 TO TOTAL-HOJAS.
           MOVE ZERO TO TOTAL-LINEAS.
           PERFORM IMPRIMIR-ENCABEZADO.

       IMPRIMIR-ENCABEZADO.
           MOVE FUNCTION CURRENT-DATE(7:2) TO REP-LINEA1-FECHA-DD.
           MOVE FUNCTION CURRENT-DATE(5:2) TO REP-LINEA1-FECHA-MM.
           MOVE FUNCTION CURRENT-DATE(1:4) TO REP-LINEA1-FECHA-AAAA.
           MOVE TOTAL-HOJAS TO REP-LINEA1-HOJA.
           WRITE LINEA FROM REP-LINEA1.
           WRITE LINEA FROM REP-TITULO BEFORE 1.
           ADD 3 TO TOTAL-LINEAS.

      *******************************************************************
       IMPRIMIR-DATOS-CONSULTOR.
           MOVE CONS-NUMERO TO CONS-NUM.
           MOVE CONS-NOMBRE TO CONS-NOM.
           MOVE CONS-SRT TO CONS-CAT.

           WRITE LINEA FROM REP-CONSULTOR-1.
           WRITE LINEA FROM REP-CONSULTOR-2 BEFORE 1.
           ADD 3 TO TOTAL-LINEAS.

      *******************************************************************
       INICIALIZAR-TOTALES-CONSULTOR.
           MOVE ZERO TO TOTAL-CONS-IMPORTE.
           MOVE ZERO TO TOTAL-CONS-HS.

      *******************************************************************
       PROCESAMIENTO-CONSULTOR.
           PERFORM AVANZAR-FECHA-DEL-TIMES.
           PERFORM IMPRIMIR-HEADER-TABLA.
           PERFORM INICIALIZAR-TOTALES-FECHA.
           PERFORM PROCESAMIENTO-FECHA UNTIL
               (NUMERO IN REG-MIN <> NUMERO IN REG-MIN-ANT
               AND FECHA IN REG-MIN <> FECHA IN REG-MIN-ANT)
               OR (FS-ARCHIVOS(ARCHIVO-MINIMO) = 1).
           PERFORM IMPRIMIR-TOTAL-FECHA.

      *******************************************************************
       AVANZAR-FECHA-DEL-TIMES.
           MOVE ANIO IN TIE-FECHA TO ANIO IN FECHA-INV1.
           MOVE MES IN TIE-FECHA TO MES IN FECHA-INV1.
           MOVE DIA IN TIE-FECHA TO DIA IN FECHA-INV1.

           MOVE ANIO IN REG-MIN TO ANIO IN FECHA-INV2.
           MOVE MES IN REG-MIN TO MES IN FECHA-INV2.
           MOVE DIA IN REG-MIN TO DIA IN FECHA-INV2.

           PERFORM AVANZAR-TIMES-FECHA UNTIL
               (TIE-NUMERO <> NUMERO IN REG-MIN
               OR FECHA-INV1 > FECHA-INV2)
               OR FS-TIEMPOS = 10.

       AVANZAR-TIMES-FECHA.
           WRITE TIE-NEW-REG FROM TIE-REG.
           PERFORM LEER-TIEMPOS.

           MOVE ANIO IN TIE-FECHA TO ANIO IN FECHA-INV1.
           MOVE MES IN TIE-FECHA TO MES IN FECHA-INV1.
           MOVE DIA IN TIE-FECHA TO DIA IN FECHA-INV1.
           MOVE ANIO IN REG-MIN TO ANIO IN FECHA-INV2.
           MOVE MES IN REG-MIN TO MES IN FECHA-INV2.
           MOVE DIA IN REG-MIN TO DIA IN FECHA-INV2.

      *******************************************************************
       IMPRIMIR-HEADER-TABLA.
           WRITE LINEA FROM REP-HEADER-TABLA.
           ADD 1 TO TOTAL-LINEAS.

      *******************************************************************
       INICIALIZAR-TOTALES-FECHA.
           MOVE ZERO TO TOTAL-FECHA-IMPORTE.
           MOVE ZERO TO TOTAL-FECHA-HS.
           MOVE ZERO TO LINEAS-TABLA.

      *******************************************************************
       PROCESAMIENTO-FECHA.
           PERFORM ESCRIBIR-MINIMO-EN-TIMES-NEW.
           PERFORM IMPRIMIR-FILA-TABLA.
           PERFORM ACTUALIZAR-TOTALES.
           MOVE REG-MIN TO REG-MIN-ANT.
           PERFORM LEER-DE-ARCHIVO-MIN.
           PERFORM BUSCAR-CLAVE-MINIMA.

      *******************************************************************
       ESCRIBIR-MINIMO-EN-TIMES-NEW.
           WRITE TIE-NEW-REG FROM REG-MIN.

      *******************************************************************
       IMPRIMIR-FILA-TABLA.
           PERFORM CHEQUEAR-CANT-LINEAS.
           MOVE EMPRESA IN REG-MIN TO REP-TABLA-EMPRESA.
           MOVE TIPO IN REG-MIN TO REP-TABLA-TIPO.
           MOVE HORAS IN REG-MIN TO REP-TABLA-HS.

           SET IND-TAR TO 1.
           SEARCH ELEMENTO
               AT END DISPLAY 'NO SE ENCONTRO LA TARIFA'
               WHEN (CATEGORIA(IND-TAR) = CONS-SRT
                   AND TIPO-TAR(IND-TAR) = TIPO IN REG-MIN)
               NEXT SENTENCE
               END-SEARCH.

           MOVE TARIFA(IND-TAR) TO REP-TABLA-TARIFA.
           COMPUTE REP-TABLA-IMPORTE =
               TARIFA(IND-TAR) * HORAS IN REG-MIN.

      * BUSCAR RAZON SOCIAL EN TABLA EMPRESAS.
      * MOVER LA FECHA

           ADD 1 TO LINEAS-TABLA.

      *******************************************************************
       ACTUALIZAR-TOTALES.
      *    CALCULAR EL IMPORTE.
      *     ADD IN REG-MIN TO TOTAL-GRAL-IMPORTE.
      *     ADD TO TOTAL-CONS-IMPORTE.
      *     ADD TO TOTAL-FECHA-IMPORTE.

           ADD HORAS IN REG-MIN TO TOTAL-CONS-HS.
           ADD HORAS IN REG-MIN TO TOTAL-FECHA-HS.

      *******************************************************************
       LEER-DE-ARCHIVO-MIN.
           IF ARCHIVO-MINIMO = 1
               PERFORM LEER-NOVTIMES1.

           IF ARCHIVO-MINIMO = 2
               PERFORM LEER-NOVTIMES2.

           IF ARCHIVO-MINIMO = 3
               PERFORM LEER-NOVTIMES3.

      *******************************************************************
       IMPRIMIR-TOTAL-FECHA.
           MOVE TOTAL-FECHA-HS TO REP-TOTAL-FECHA-HS.
           MOVE TOTAL-FECHA-IMPORTE TO REP-TOTAL-FECHA-IMP.
           WRITE LINEA FROM REP-TOTALES-FECHA.
           ADD 1 TO TOTAL-LINEAS.

      *******************************************************************
       IMPRIMIR-TOTAL-CONSULTOR.
           MOVE TOTAL-CONS-HS TO REP-TOTAL-CONS-HS.
           MOVE TOTAL-CONS-IMPORTE TO REP-TOTAL-CONS-IMP.
           WRITE LINEA FROM REP-TOTALES-CONS.
           ADD 1 TO TOTAL-LINEAS.

      *******************************************************************
       IMPRIMIR-TOTAL-GRAL.
           MOVE TOTAL-GRAL-IMPORTE TO REP-TOTAL-GRAL-IMPORTE.
           WRITE LINEA FROM REP-TOTALES-GRAL AFTER 1.
           ADD 2 TO TOTAL-LINEAS.

      *******************************************************************
           CHEQUEAR-CANT-LINEAS.

      *******************************************************************
       CERRAR-ARCHIVOS.
           CLOSE TIEMPOS NOVTIMES1 NOVTIMES2 NOVTIMES3 CONSULTORES
           EMPRESAS TARIFAS TIEMPOS-NEW LISTADO.

       END PROGRAM TP1-PUNTO-A.
