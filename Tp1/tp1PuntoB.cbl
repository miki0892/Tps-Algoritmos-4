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

           SELECT ESTADISTICAS ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-ESTADISTICAS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPRESAS LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algoritm
      -           'os-4/Tp1/Archivos de prueba/empresas.dat'.

       01 EMP.
           03 EMP-CODIGO PIC 9(03).
           03 EMP-RAZON PIC X(25).
           03 EMP-DIRE PIC X(20).
           03 EMP-TEL PIC X(20).
           03 EMP-CUIT PIC 9(11).

       FD TIEMPOS LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algoritm
      -           'os-4/Tp1/Archivos de prueba/times.dat'.

       01 TIE.
           03 TIE-NUMERO PIC X(5).
           03 TIE-FECHA.
               05 TIE-FECHA-DD PIC 9(2).
               05 TIE-FECHA-MM PIC 9(2).
               05 TIE-FECHA-AAAA PIC 9(4).
           03 TIE-EMPRESA PIC 9(03).
           03 TIE-TAREA PIC X(04).
           03 TIE-HORAS PIC 9(2)V99.

       FD ESTADISTICAS LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS '/home/j/Desktop/Alg4/Tps-Algoritm
      -           'os-4/Tp1/Archivos de prueba/estadisticas.dat'.

       01 LINEA PIC X(80).

       WORKING-STORAGE SECTION.

       77 FS-EMPRESAS PIC XX.
       77 FS-TIEMPOS PIC XX.
       77 FS-ESTADISTICAS PIC XX.

       01 TABLA-EMPRESAS.
           03 WS-EMPRESA OCCURS 3 TIMES INDEXED BY IND-EMP.
               05 CODIGO PIC 9(3).
               05 RAZON-SOCIAL PIC x(25).

       01 TABLA-TOTAL-EMPRESAS.
           03 EMPRESA OCCURS 3 TIMES.
               05 ANIO OCCURS 5 TIMES.
                   07 MES OCCURS 12 TIMES.
                       09 HORAS-ACUMU PIC 9(3) VALUE ZERO.
                   07 TOTAL-ANIO PIC 9(4) VALUE ZERO.

       01 TABLA-TOTAL-MESES.
           03 MES OCCURS 12 TIMES.
              05 HORAS-ACUM PIC 9(3) VALUE ZERO.
           03 TOTAL-MESES PIC 9(4) VALUE ZERO.

       01 ANIO-CORRIENTE PIC 9(4).
       01 MES-CORRIENTE PIC 9(2).
       01 DIA-CORRIENTE PIC 9(2).
       01 ANIO-LIMITE PIC 9(4).
       01 ANIO-A-IMPRIMIR PIC 9(4).

       01 IND-EMPRESA PIC 9(3).
       01 IND-MES PIC 9(3).
       01 IND-ANIO PIC 9(3).

       01 REP-LINEA1.
           02 FILLER PIC X(8) VALUE 'Fecha: '.
           02 REP-LINEA1-FECHA-DD PIC 9(2).
           02 FILLER PIC X(1) VALUE '/'.
           02 REP-LINEA1-FECHA-MM PIC 9(2).
           02 FILLER PIC X(1) VALUE '/'.
           02 REP-LINEA1-FECHA-AAAA PIC 9(4).


       01 REP-TITULO.
           02 FILLER PIC X(12) VALUE SPACES.
           02 PARTE-1 PIC X(56) VALUE 'Listado de Estadístico de Horas a
      -                                'plicadas por año y mes'.
           02 FILLER PIC X(12) VALUE SPACES.

       01 REP-HEADER-TABLA.
           02 FILLER PIC X(80) VALUE 'Empresa             Ano    Ene Feb
      -        ' Mar Abr May Jun Jul Ago Sep Oct Nov Dic Total'.

       01 REP-REG-ANIO.
           02 RAZON PIC X(20) VALUE SPACES.
           02 ANIO PIC 9(4).
           02 FILLER PIC X(3) VALUE SPACES.

       01 REP-LINEA-TABLA.
           02 EMPRESA PIC X(20) VALUE SPACES.
           02 ANIO PIC 9(4).
           02 FILLER PIC X(3) VALUE SPACES.
           02 MESES OCCURS 12 TIMES INDEXED BY IND-MES-REP.
               03 HORA-ACUM-MES-ANIO PIC 9(3).
               03 FILLER PIC X VALUE SPACES.
           02 TOTAL-ANIO-REP PIC 9(4).

       01 REP-LINEA-TOTALES.
           02 FILLER PIC X(7) VALUE 'Totales'.
           02 FILLER PIC X(20) VALUE SPACES.
           02 MESES OCCURS 12 TIMES INDEXED BY IND-MES-TOT.
               03 HORA-ACUM-MES PIC 9(3).
               03 FILLER PIC X VALUE SPACES.
           02 TOTAL-MES-REP PIC 9(4).


       77 LINEA-RECTA PIC X(80) VALUE ALL '-'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM INICIALIZAR-TABLA-EMPRESAS.
           PERFORM LEER-TIEMPOS.
           PERFORM OBTENER-ANIO-LIMITE.
           PERFORM AVANZAR-HASTA-FECHA-VALIDA.
           PERFORM PROCESAR-TIEMPOS UNTIL FS-TIEMPOS = '10'.
           PERFORM IMPRIMIR-REPORTE.
           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.


           ABRIR-ARCHIVOS.
               OPEN INPUT EMPRESAS.
               IF FS-EMPRESAS NOT = ZERO
                   DISPLAY 'ERROR AL ABRIR EMPRESAS FS: ' FS-EMPRESAS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.
               OPEN INPUT TIEMPOS.
               IF FS-TIEMPOS NOT = ZERO
                   DISPLAY 'ERROR AL ABRIR TIMES FS: ' FS-TIEMPOS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.
               OPEN OUTPUT ESTADISTICAS.
               IF FS-ESTADISTICAS NOT = ZERO
                   DISPLAY 'ERROR AL ABRIR ESTADISTICAS FS: '
                           FS-ESTADISTICAS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.

           INICIALIZAR-TABLA-EMPRESAS.
               PERFORM LEER-EMPRESAS.
               PERFORM CARGAR-VALORES VARYING IND-EMP FROM 1 BY 1
                       UNTIL FS-EMPRESAS = '10'.

           LEER-EMPRESAS.
               READ EMPRESAS.
               IF FS-EMPRESAS NOT = ZERO AND 10
                   DISPLAY 'ERROR AL LEER EMPRESAS FS' FS-EMPRESAS
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.


           CARGAR-VALORES.
               MOVE EMP-CODIGO TO CODIGO(IND-EMP).
               MOVE EMP-RAZON TO RAZON-SOCIAL(IND-EMP).
               PERFORM LEER-EMPRESAS.

           LEER-TIEMPOS.
               READ TIEMPOS.
               IF FS-TIEMPOS NOT = ZERO AND 10
                   DISPLAY 'ERROR AL LEER TIMES FS: ' FS-TIEMPOS
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
                   AT END DISPLAY 'NO SE ENCONTRO CODIGO EMPRESA'
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
               PERFORM IMPRIMIR-ENCABEZADO.
               PERFORM IMPRIMIR-HEADER-TABLA.
               PERFORM IMPRIMIR-TABLA-POR-EMPRESA
                       VARYING IND-EMPRESA FROM 1 BY 1
                       UNTIL IND-EMPRESA > 3.
               PERFORM IMPRIMIR-TABLA-TOTAL-MESES.


           IMPRIMIR-ENCABEZADO.
               MOVE FUNCTION CURRENT-DATE(7:2) TO REP-LINEA1-FECHA-DD.
               MOVE FUNCTION CURRENT-DATE(5:2) TO REP-LINEA1-FECHA-MM.
               MOVE FUNCTION CURRENT-DATE(1:4) TO REP-LINEA1-FECHA-AAAA.
               WRITE LINEA FROM REP-LINEA1.
               WRITE LINEA FROM REP-TITULO BEFORE 2.

           IMPRIMIR-TABLA-POR-EMPRESA.
               WRITE LINEA FROM LINEA-RECTA.
               PERFORM IMPRIMIR-REG-POR-ANIO
                       VARYING IND-ANIO FROM 1 BY 1
                       UNTIL IND-ANIO > 5.

           IMPRIMIR-HEADER-TABLA.
               WRITE LINEA FROM REP-HEADER-TABLA BEFORE 1.


           IMPRIMIR-REG-POR-ANIO.
               IF IND-ANIO = 1
                   MOVE RAZON-SOCIAL(IND-EMPRESA)
                        TO EMPRESA IN REP-LINEA-TABLA
               ELSE
                   MOVE SPACES TO EMPRESA IN REP-LINEA-TABLA.
               ADD ANIO-LIMITE TO IND-ANIO GIVING ANIO
                   IN REP-LINEA-TABLA.
               SUBTRACT 1 FROM ANIO IN REP-LINEA-TABLA.
               PERFORM CARGAR-REG-IMP-TABLA VARYING IND-MES-REP
                       FROM 1 BY 1 UNTIL IND-MES-REP > 12.
               MOVE TOTAL-ANIO(IND-EMP,IND-ANIO) TO TOTAL-ANIO-REP.
               WRITE LINEA FROM REP-LINEA-TABLA.

           CARGAR-REG-IMP-TABLA.
               MOVE HORAS-ACUMU(IND-EMP,IND-ANIO,IND-MES-REP)
                    TO HORA-ACUM-MES-ANIO(IND-MES-REP).

           IMPRIMIR-TABLA-TOTAL-MESES.
              PERFORM CARGAR-TABLA-TOTAL-MESES
                      VARYING IND-MES-TOT FROM 1 BY 1
                      UNTIL IND-MES-TOT > 12.
              MOVE TOTAL-MESES TO TOTAL-MES-REP.
              WRITE LINEA FROM REP-LINEA-TOTALES AFTER 1.

           CARGAR-TABLA-TOTAL-MESES.
               MOVE HORAS-ACUM(IND-MES-TOT)
                    TO HORA-ACUM-MES(IND-MES-TOT).

           CERRAR-ARCHIVOS.
               CLOSE EMPRESAS TIEMPOS ESTADISTICAS.

       END PROGRAM TP1-PUNTO-B.
