      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAR-INDEXADOS.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TIEMPOS-SEC     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS FS-TIEM-SEC.

           SELECT CONSULTORES-SEC ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS FS-CONS-SEC.

           SELECT EMPRESAS-SEC    ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS FS-EMP-SEC.

           SELECT TARIFAS-SEC     ASSIGN TO DISK
                                  ORGANIZATION IS LINE SEQUENTIAL
                                  FILE STATUS IS FS-TAR-SEC.
+
           SELECT TIEMPOS-INDEX   ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  RECORD KEY IS TIE-IND-CLAVE
                                  ALTERNATE RECORD KEY IS TIE-IND-CUIT
                                  WITH DUPLICATES
                                  FILE STATUS IS FS-TIE-INDEX.

           SELECT CONSULT-INDEX   ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  RECORD KEY IS CONS-IND-NUM
                                  FILE STATUS IS FS-CONS-INDEX.

           SELECT EMP-INDEX       ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  RECORD KEY IS EMP-IND-COD
                                  ALTERNATE RECORD KEY IS EMP-IND-CUIT
                                  FILE STATUS IS FS-EMP-INDEX.

           SELECT TAR-INDEX       ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  RECORD KEY IS TAR-IND-CLAVE
                                  FILE STATUS IS FS-TAR-INDEX.

       DATA DIVISION.
       FILE SECTION.

       FD TIEMPOS-SEC LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Secuenciales/times.dat'.

       01 TIE-REG.
           03 TIE-NUMERO PIC X(5).
           03 TIE-FECHA.
               05 TIE-DIA PIC 9(2).
               05 TIE-MES PIC 9(2).
               05 TIE-ANIO PIC 9(4).
           03 TIE-EMPRESA PIC 9(03).
           03 TIE-TAREA PIC X(04).
           03 TIE-HORAS PIC 9(2)V99.
           03 TIE-TIPO-TAR PIC X(2).

       FD EMPRESAS-SEC LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Secuenciales/empresas.dat'.

       01 EMP-REG.
           03 EMP-CODIGO PIC 9(3).
           03 EMP-RAZON PIC X(25).
           03 EMP-DIRE PIC X(20).
           03 EMP-TEL PIC X(20).
           03 EMP-CUIT PIC 9(11).

       FD CONSULTORES-SEC LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS '/home/miki/Escritorio/Algos 4/Tps-Algori
      -    'tmos-4/Tp2/Archivos de prueba/Secuenciales/consultores.dat'.

       01 CONS-REG.
           03 CONS-NUMERO PIC X(5).
           03 CONS-DNI PIC 9(8).
           03 CONS-SRT PIC X(2).
           03 CONS-NOMBRE PIC X(25).
           03 CONS-DIRE PIC X(20).
           03 CONS-TEL PIC X(20).

       FD TARIFAS-SEC LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS '/home/miki/Escritorio/Algos 4/Tps-Algori
      -    'tmos-4/Tp2/Archivos de prueba/Secuenciales/tarifas.dat'.

       01 TAR-REG.
           03 TAR-SRT PIC X(02).
           03 TAR-TIPO PIC 99.
           03 TAR-TARIFA PIC 9(5)V99.

       FD TIEMPOS-INDEX LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Indexados/times.dat'.

       01 TIE-IND-REG.
           03 TIE-IND-CLAVE.
               05 TIE-IND-NUM PIC X(5).
               05 TIE-IND-FECHA.
                   07 TIE-IND-FECHA-AAAA PIC 9(4).
                   07 TIE-IND-FECHA-MM   PIC 9(2).
                   07 TIE-IND-FECHA-DD   PIC 9(2).
               05 TIE-IND-CUIT PIC 9(11).
           03 TIE-IND-TAREA PIC X(4).
           03 TIE-IND-HORAS PIC 9(2)V99.
           03 TIE-IND-TIPO  PIC X(2).

       FD CONSULT-INDEX LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Indexados/consultores.dat'.

       01 CONS-IND-REG.
           03 CONS-IND-NUM PIC X(5).
           03 CONS-IND-DNI PIC 9(8).
           03 CONS-IND-SRT PIC X(2).
           03 CONS-IND-NOM PIC X(25).
           03 CONS-IND-DIR PIC X(20).
           03 CONS-IND-TEL PIC X(20).

       FD EMP-INDEX LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Indexados/empresas.dat'.

       01 EMP-IND-REG.
           03 EMP-IND-COD  PIC 9(3).
           03 EMP-IND-RAZ  PIC X(25).
           03 EMP-IND-DIR  PIC X(20).
           03 EMP-IND-TEL  PIC X(20).
           03 EMP-IND-CUIT PIC 9(11).

       FD TAR-INDEX LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Indexados/tarifas.dat'.

       01 TAR-IND-REG.
           03 TAR-IND-CLAVE.
               05 TAR-IND-SRT  PIC X(2).
               05 TAR-IND-TIPO PIC 99.
           03 TAR-IND-TAR PIC 9(5)V99.

       WORKING-STORAGE SECTION.

       77 FS-TIEM-SEC PIC XX.
       77 FS-CONS-SEC PIC XX.
       77 FS-EMP-SEC PIC XX.
       77 FS-TAR-SEC PIC XX.
       77 FS-TIE-INDEX PIC XX.
       77 FS-CONS-INDEX PIC XX.
       77 FS-EMP-INDEX PIC XX.
       77 FS-TAR-INDEX PIC XX.

       01 EMP-TABLA.
           03 EMPRESA OCCURS 3 TIMES INDEXED BY IND-EMP.
               05 COD-EMP PIC 9(3).
               05 CUIT-EMP PIC 9(11).

       01 IND-TABLA PIC 99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM PROCESAR-CONSULTORES.
           PERFORM PROCESAR-EMPRESAS.
           PERFORM PROCESAR-TARIFAS.
           PERFORM PROCESAR-TIMES.
           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.

       ABRIR-ARCHIVOS.
           OPEN INPUT TIEMPOS-SEC.
           IF FS-TIEM-SEC NOT = ZERO
               DISPLAY "ERROR AL ABRIR TIMES SEC FS: " FS-TIEM-SEC
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN INPUT CONSULTORES-SEC.
           IF FS-CONS-SEC NOT = ZERO
               DISPLAY "ERROR AL ABRIR CONSULTORES SEC FS: " FS-CONS-SEC
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN INPUT EMPRESAS-SEC.
           IF FS-EMP-SEC NOT = ZERO
               DISPLAY "ERROR AL ABRIR EMPRESAS SEC FS: " FS-EMP-SEC
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN INPUT TARIFAS-SEC.
           IF FS-TAR-SEC NOT = ZERO
               DISPLAY "ERROR AL ABRIR TARIFAS SEC FS: " FS-TAR-SEC
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN OUTPUT TIEMPOS-INDEX.
           IF FS-TIE-INDEX NOT = ZERO
               DISPLAY "ERROR AL ABRIR TIMES INDEX FS: " FS-TIE-INDEX
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN OUTPUT CONSULT-INDEX.
           IF FS-CONS-INDEX NOT = ZERO
               DISPLAY "ERROR AL ABRIR CONSULTORES IND FS: "
               FS-CONS-INDEX
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN OUTPUT EMP-INDEX.
           IF FS-EMP-INDEX NOT = ZERO
               DISPLAY "ERROR AL ABRIR EMPRESAS INDEX FS: " FS-EMP-INDEX
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.
           OPEN OUTPUT TAR-INDEX.
           IF FS-TAR-INDEX NOT = ZERO
               DISPLAY "ERROR AL ABRIR TARIFAS INDEX FS: " FS-TAR-INDEX
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.

       LEER-TIMES.
           READ TIEMPOS-SEC.
           IF FS-TIEM-SEC NOT = ZERO AND '10'
               DISPLAY "ERROR AL LEER TIMES SEC FS: " FS-TIEM-SEC
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.

       LEER-CONSULTORES.
           READ CONSULTORES-SEC.
           IF FS-CONS-SEC NOT = ZERO AND '10'
               DISPLAY "ERROR AL LEER CONSULT SEC FS: " FS-CONS-SEC
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.

       LEER-EMPRESAS.
           READ EMPRESAS-SEC.
           IF FS-EMP-SEC NOT = ZERO AND '10'
               DISPLAY "ERROR AL LEER EMPRESAS SEC FS: " FS-EMP-SEC
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.

       LEER-TARIFAS.
           READ TARIFAS-SEC.
           IF FS-TAR-SEC NOT = ZERO AND '10'
               DISPLAY "ERROR AL LEER TARIFAS SEC FS: " FS-TAR-SEC
               PERFORM CERRAR-ARCHIVOS
               STOP RUN.

       PROCESAR-CONSULTORES.
           PERFORM LEER-CONSULTORES.
           PERFORM FORMAR-CONS-INDEX-REG UNTIL FS-CONS-SEC = '10'.


       FORMAR-CONS-INDEX-REG.
           MOVE CONS-REG TO CONS-IND-REG.
           WRITE CONS-IND-REG.
           PERFORM LEER-CONSULTORES.

       PROCESAR-EMPRESAS.
           PERFORM LEER-EMPRESAS.
           SET IND-TABLA TO 1.
           PERFORM FORMAR-EMP-INDEX-REG UNTIL FS-EMP-SEC = '10'.

       FORMAR-EMP-INDEX-REG.
           MOVE EMP-REG TO EMP-IND-REG.
           PERFORM CARGAR-TABLA.
           WRITE EMP-IND-REG.
           PERFORM LEER-EMPRESAS.

       CARGAR-TABLA.
           MOVE EMP-CODIGO TO COD-EMP(IND-TABLA).
           MOVE EMP-CUIT TO CUIT-EMP(IND-TABLA).
           ADD 1 TO IND-TABLA.

       PROCESAR-TARIFAS.
           PERFORM LEER-TARIFAS.
           PERFORM FORMAR-TAR-INDEX-REG UNTIL FS-TAR-SEC = '10'.

       FORMAR-TAR-INDEX-REG.
           MOVE TAR-REG TO TAR-IND-REG.
           WRITE TAR-IND-REG.
           PERFORM LEER-TARIFAS.

       PROCESAR-TIMES.
           PERFORM LEER-TIMES.
           PERFORM FORMAR-TIE-INDEX-REG UNTIL FS-TIEM-SEC = '10'.

       FORMAR-TIE-INDEX-REG.
           MOVE TIE-NUMERO TO TIE-IND-NUM.
           MOVE TIE-ANIO TO TIE-IND-FECHA-AAAA.
           MOVE TIE-MES TO TIE-IND-FECHA-MM.
           MOVE TIE-DIA TO TIE-IND-FECHA-DD.
           MOVE TIE-TAREA TO TIE-IND-TAREA.
           MOVE TIE-HORAS TO TIE-IND-HORAS.
           MOVE TIE-TIPO-TAR TO TIE-IND-TIPO.
           PERFORM BUSCAR-EMP-CUIT.
           WRITE TIE-IND-REG.
           PERFORM LEER-TIMES.

       BUSCAR-EMP-CUIT.
           SET IND-EMP TO 1.
           SEARCH EMPRESA
               WHEN COD-EMP(IND-EMP) = TIE-EMPRESA
               MOVE CUIT-EMP(IND-EMP) TO TIE-IND-CUIT
               END-SEARCH.

       CERRAR-ARCHIVOS.
           CLOSE TIEMPOS-SEC CONSULTORES-SEC EMPRESAS-SEC TARIFAS-SEC
           TIEMPOS-INDEX CONSULT-INDEX EMP-INDEX TAR-INDEX.

       END PROGRAM CREAR-INDEXADOS.
