      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TP1-PUNTO-B.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPRESAS     ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-EMPRESAS.

           SELECT TIEMPOS      ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-TIEMPOS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPRESAS LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS "/home/miki/empresas.dat".
       01 EMP.
           03 EMP-CODIGO PIC 9(03).
           03 EMP-RAZON PIC X(25).
           03 EMP-DIRE PIC X(20).
           03 EMP-TEL PIC X(20).
           03 EMP-CUIT PIC 9(11).

       FD TIEMPOS LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS "/home/miki/times.dat".

       01 TIE.
           03 TIE-NUMERO PIC X(5).
           03 TIE-FECHA.
               05 TIE-FECHA-DD PIC 9(2).
               05 TIE-FECHA-MM PIC 9(2).
               05 TIE-FECHA-AAAA PIC 9(4).
           03 TIE-EMPRESA PIC 9(03).
           03 TIE-TAREA PIC X(04).
           03 TIE-HORAS PIC 9(2)V99.

       WORKING-STORAGE SECTION.

       77 FS-EMPRESAS PIC XX.
       77 FS-TIEMPOS PIC XX.

       01 TABLA-EMPRESAS.
           03 WS-EMPRESA OCCURS 3 TIMES INDEXED BY IND-EMP.
               05 CODIGO PIC 9(3).
               05 RAZON-SOCIAL PIC x(25).

       01 TABLA-TOTAL-EMPRESAS.
           03 EMPRESA OCCURS 3 TIMES.
               05 ANIO OCCURS 5 TIMES.
                   07 ANIO-IMP.
                       09 NOMBRE-EMP PIC X(20) VALUE SPACES.
                       09 ANIO-NRO PIC X(6) VALUE SPACES.
                       09 MES OCCURS 12 TIMES.
                           11 HORAS-ACUMU PIC 9(3) VALUE ZERO.
                           11 FILLER PIC X(1) VALUE SPACES.
                       09 TOTAL-ANIO PIC 9(4) VALUE ZERO.

       01 TABLA-TOTAL-MESES.
           03 MES-IMP.
               05 FILLER PIC X(26) VALUE "Totales".
               05 MES OCCURS 12 TIMES.
                   07 HORAS-ACUM PIC 9(3) VALUE ZERO.
                   07 FILLER PIC X(1) VALUE SPACES.
               05 TOTAL-MESES PIC 9(4) VALUE ZERO.

       01 ANIO-CORRIENTE PIC 9(4).
       01 MES-CORRIENTE PIC 9(2).
       01 DIA-CORRIENTE PIC 9(2).
       01 ANIO-LIMITE PIC 9(4).
       01 ANIO-A-IMPRIMIR PIC 9(4).

       01 IND-EMPRESA PIC 9(3).
       01 IND-MES PIC 9(3).
       01 IND-ANIO PIC 9(3).

       01 REP-LINEA1.
           02 FILLER PIC X(8) VALUE "Fecha: ".
           02 REP-LINEA1-FECHA-DD PIC 9(2).
           02 FILLER PIC X(1) VALUE "/".
           02 REP-LINEA1-FECHA-MM PIC 9(2).
           02 FILLER PIC X(1) VALUE "/".
           02 REP-LINEA1-FECHA-AAAA PIC 9(4).


       01 REP-TITULO.
           02 FILLER PIC X(12) VALUE SPACES.
           02 PARTE-1 PIC X(27) VALUE "Listado de Estadístico de ".
           02 PARTE-2 PIC X(30) VALUE "Horas aplicadas por año y mes".
           02 FILLER PIC X(11) VALUE SPACES.

       01 REP-HEADER-TABLA.
           02 FILLER PIC X(7) VALUE "Empresa".
           02 FILLER PIC X(13) VALUE SPACES.
           02 FILLER PIC X(31) VALUE "Año   Ene Feb Mar Abr May Jun ".
           02 FILLER PIC X(29) VALUE "Jul Ago Sep Oct Nov Dic Total".

       01 REP-REG-ANIO.
           02 RAZON PIC X(20) VALUE SPACES.
           02 ANIO PIC 9(4).
           02 FILLER PIC X(3) VALUE SPACES.


       01 LINEA-EN-BLANCO PIC X(80) VALUE SPACES.
       01 LINEA-RECTA.
           02 FILLER PIC X(30) VALUE "------------------------------".
           02 FILLER PIC X(30) VALUE "------------------------------".
           02 FILLER PIC X(20) VALUE "--------------------".

       01 DEC PIC 9(2)V99.
       01 INT PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM INICIALIZAR-TABLA-EMPRESAS.
           PERFORM LEER-TIEMPOS.
           PERFORM OBTENER-ANIO-LIMITE.
           PERFORM AVANZAR-HASTA-FECHA-VALIDA.
           PERFORM PROCESAR-TIEMPOS UNTIL FS-TIEMPOS = '10'.
           PERFORM IMPRIMIR-REPORTE.
           DISPLAY "Hello world"
           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.


           ABRIR-ARCHIVOS.
               OPEN INPUT EMPRESAS.
               IF FS-EMPRESAS NOT = ZERO
                   DISPLAY "ERROR AL ABRIR EMPRESAS FS: " FS-EMPRESAS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.
               OPEN INPUT TIEMPOS.
               IF FS-TIEMPOS NOT = ZERO
                   DISPLAY "ERROR AL ABRIR TIMES FS: " FS-TIEMPOS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.

           INICIALIZAR-TABLA-EMPRESAS.
               PERFORM LEER-EMPRESAS.
               PERFORM CARGAR-VALORES VARYING IND-EMP FROM 1 BY 1
                       UNTIL FS-EMPRESAS = '10'.

           LEER-EMPRESAS.
               READ EMPRESAS.
               IF FS-EMPRESAS NOT = ZERO AND 10
                   DISPLAY "ERROR AL LEER EMPRESAS FS" FS-EMPRESAS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.


           CARGAR-VALORES.
               MOVE EMP-CODIGO TO CODIGO(IND-EMP).
               MOVE EMP-RAZON TO RAZON-SOCIAL(IND-EMP).
               PERFORM LEER-EMPRESAS.

           LEER-TIEMPOS.
               READ TIEMPOS.
               IF FS-TIEMPOS NOT = ZERO AND 10
                   DISPLAY "ERROR AL LEER TIMES FS: " FS-TIEMPOS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.


           OBTENER-ANIO-LIMITE.
               MOVE FUNCTION CURRENT-DATE(1:4) TO ANIO-CORRIENTE.
               SUBTRACT 4 FROM ANIO-CORRIENTE GIVING ANIO-LIMITE.

           AVANZAR-HASTA-FECHA-VALIDA.
               PERFORM LEER-TIEMPOS UNTIL
                      TIE-FECHA-AAAA >= ANIO-LIMITE OR
                      FS-TIEMPOS = '10'.

           PROCESAR-TIEMPOS.
               PERFORM ACUMULAR-EN-TABLAS.
               PERFORM LEER-TIEMPOS.
               PERFORM AVANZAR-HASTA-FECHA-VALIDA.

           ACUMULAR-EN-TABLAS.
              PERFORM OBTENER-INDICE-EMPRESA.
              PERFORM ACTUALIZAR-TABLA-TOTAL-EMPRESAS.
              PERFORM ACTUALIZAR-TABLA-TOTAL-MESES.

           OBTENER-INDICE-EMPRESA.
               SET IND-EMP TO 1.
               SEARCH WS-EMPRESA
                   AT END DISPLAY "NO SE ENCONTRO CODIGO EMPRESA"
                   WHEN CODIGO(IND-EMP) = TIE-EMPRESA
                   NEXT SENTENCE
                   END-SEARCH.

           ACTUALIZAR-TABLA-TOTAL-EMPRESAS.
              SUBTRACT ANIO-LIMITE FROM TIE-FECHA-AAAA GIVING IND-ANIO.
              ADD 1 TO IND-ANIO.
              MOVE TIE-FECHA-MM TO IND-MES.
              ADD TIE-HORAS TO HORAS-ACUMU(IND-EMP,IND-ANIO,IND-MES)
              TOTAL-ANIO(IND-EMP,IND-ANIO).

           ACTUALIZAR-TABLA-TOTAL-MESES.
              ADD TIE-HORAS TO HORAS-ACUM(IND-MES)  TOTAL-MESES.

           IMPRIMIR-REPORTE.
               DISPLAY LINEA-EN-BLANCO.
               PERFORM IMPRIMIR-ENCABEZADO.
               PERFORM IMPRIMIR-TABLA-POR-EMPRESA
                       VARYING IND-EMPRESA FROM 1 BY 1
                       UNTIL IND-EMPRESA > 3.
               DISPLAY LINEA-EN-BLANCO.
               PERFORM IMPRIMIR-TABLA-TOTAL-MESES.
               DISPLAY LINEA-EN-BLANCO.


           IMPRIMIR-ENCABEZADO.
               MOVE FUNCTION CURRENT-DATE(7:2) TO REP-LINEA1-FECHA-DD.
               MOVE FUNCTION CURRENT-DATE(5:2) TO REP-LINEA1-FECHA-MM.
               MOVE FUNCTION CURRENT-DATE(1:4) TO REP-LINEA1-FECHA-AAAA.
               DISPLAY REP-LINEA1.
               DISPLAY REP-TITULO.

           IMPRIMIR-TABLA-POR-EMPRESA.
               PERFORM IMPRIMIR-HEADER-TABLA.
               PERFORM IMPRIMIR-REG-POR-ANIO
                       VARYING IND-ANIO FROM 1 BY 1
                       UNTIL IND-ANIO > 5.

           IMPRIMIR-HEADER-TABLA.
               DISPLAY LINEA-EN-BLANCO.
               DISPLAY REP-HEADER-TABLA.
               DISPLAY LINEA-RECTA.


           IMPRIMIR-REG-POR-ANIO.
               IF IND-ANIO = 1
                   MOVE RAZON-SOCIAL(IND-EMPRESA)
                        TO NOMBRE-EMP(IND-EMPRESA,IND-ANIO).
               ADD ANIO-LIMITE TO IND-ANIO GIVING ANIO-A-IMPRIMIR.
               SUBTRACT 1 FROM ANIO-A-IMPRIMIR.
               MOVE ANIO-A-IMPRIMIR TO ANIO-NRO(IND-EMPRESA,IND-ANIO).
               DISPLAY ANIO-IMP(IND-EMPRESA,IND-ANIO).

           IMPRIMIR-TABLA-TOTAL-MESES.
               DISPLAY MES-IMP.

           CERRAR-ARCHIVOS.
               CLOSE EMPRESAS.
               CLOSE TIEMPOS.

       END PROGRAM TP1-PUNTO-B.
