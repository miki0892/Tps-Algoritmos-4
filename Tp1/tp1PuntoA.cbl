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

           SELECT CONSULTORES  ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-CONSULTORES.

           SELECT EMPRESAS     ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-EMPRESAS.

           SELECT TARIFAS      ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                               FILE STATUS IS FS-TARIFAS.

       DATA DIVISION.
       FILE SECTION.
       FD NOVTIMES1 LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID IS '/home/miki/Escritorio/Tps-Algor'
      -             'itmos-4/Tp1/Archivos de prueba/novTimes1.dat'.
       01 NOV1-REG PIC X(33).


       FD NOVTIMES2 LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID IS '/home/miki/Escritorio/Tps-Algor'
      -             'itmos-4/Tp1/Archivos de prueba/novTimes2.dat'.

       01 NOV2-REG PIC X(33).

       FD NOVTIMES3 LABEL RECORD IS STANDARD
                    VALUE OF FILE-ID IS '/home/miki/Escritorio/Tps-Algor'
      -             'itmos-4/Tp1/Archivos de prueba/novTimes3.dat'.

       01 NOV3-REG PIC X(33).

       FD TIEMPOS LABEL RECORD IS STANDARD
                  VALUE OF FILE-ID IS '/home/miki/Escritorio/Tps-Algorit'
      -           'mos-4/Tp1/Archivos de prueba/times.dat'.
       01 TIE-REG.
           03 TIE-NUMERO PIC X(5).
           03 TIE-FECHA PIC 9(8).
           03 TIE-EMPRESA PIC 9(03).
           03 TIE-TAREA PIC X(04).
           03 TIE-HORAS PIC 9(2)V99.

       FD EMPRESAS LABEL RECORD IS STANDARD
                   VALUE OF FILE-ID IS '/home/miki/Escritorio/Tps-Algori'
      -            'tmos-4/Tp1/Archivos de prueba/empresas.dat'.
       01 EMP-REG.
           03 EMP-CODIGO PIC 9(03).
           03 EMP-RAZON PIC X(25).
           03 EMP-DIRE PIC X(20).
           03 EMP-TEL PIC X(20).
           03 EMP-CUIT PIC 9(11).

       FD CONSULTORES LABEL RECORD IS STANDARD
                      VALUE OF FILE-ID IS '/home/miki/Escritorio/Tps-Alg'
      -             'oritmos-4/Tp1/Archivos de prueba/consultores.dat'.
       01 CONS-REG.
           03 CONS-NUMERO PIC X(5).
           03 CONS-DNI PIC 9(8).
           03 CONS-SRT PIC X(2).
           03 CONS-NOMBRE PIC X(25).
           03 CONS-DIRE PIC X(20).
           03 CONS-TEL PIC X(20).

       FD TARIFAS     LABEL RECORD IS STANDARD
                      VALUE OF FILE-ID IS '/home/miki/Escritorio/Tps-Alg'
      -               'oritmos-4/Tp1/Archivos de prueba/tarifas.dat'.
       01 TAR-REG.
           03 TAR-SRT PIC X(02).
           03 TAR-TIPO PIC 9.
           03 TAR-TARIFA PIC 9(5)V99.

       WORKING-STORAGE SECTION.

       77 FS-CONSULTORES PIC XX.
       77 FS-TIEMPOS     PIC XX.
       77 FS-NOVTIMES1   PIC XX.
       77 FS-NOVTIMES2   PIC XX.
       77 FS-NOVTIMES3   PIC XX.
       77 FS-EMPRESAS    PIC XX.
       77 FS-TARIFAS     PIC XX.

       01 TABLA-EMPRESAS.
           03 EMPRESA OCCURS 3 TIMES.
               05 NUMERO-EMP PIC 9(03).
               05 RAZON-SOCIAL PIC X(25).

       01 TABLA-TARIFAS.
           03 ELEMENTO OCCURS 90 TIMES.
               05 CATEGORIA PIC X(02).
               05 TIPO-TAR PIC 9.
               05 TARIFA PIC 9(5)V99.

       01 NOV-REG.
           03 NOV-NUMERO PIC X(5).
           03 NOV-FECHA PIC 9(8).
           03 NOV-EMPRESA PIC 9(03).
           03 NOV-TAREA PIC X(04).
           03 NOV-HORAS PIC 9(2)V99.
           03 NOV-TIPO PIC 9.

       01 NOV-REG-A-TIMESNEW REDEFINES NOV-REG PIC X(32).

       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM ABRIR-ARCHIVOS.
           PERFORM LEER-ARCHIVOS.
           PERFORM CARGAR-TABLAS.
           PERFORM CERRAR-ARCHIVOS.
           STOP RUN.

      *******************************************************************
       ABRIR-ARCHIVOS.
           OPEN INPUT TIEMPOS.
           IF FS-TIEMPOS NOT = ZERO
               DISPLAY "ERROR AL ABRIR TIMES FS: " FS-TIEMPOS
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

       LEER-NOVTIMES2.
           READ NOVTIMES2.
               IF FS-NOVTIMES2 NOT = ZERO AND 10
                   DISPLAY "ERROR AL LEER NOVTIMES2 FS: " FS-NOVTIMES2
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.

       LEER-NOVTIMES3.
           READ NOVTIMES3.
               IF FS-NOVTIMES3 NOT = ZERO AND 10
                   DISPLAY "ERROR AL LEER NOVTIMES3 FS: " FS-NOVTIMES3
                   PERFORM CERRAR-ARCHIVOS
                   STOP RUN.

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





      *******************************************************************
       CERRAR-ARCHIVOS.
           CLOSE TIEMPOS NOVTIMES1 NOVTIMES2 NOVTIMES3 CONSULTORES
           EMPRESAS TARIFAS.








       END PROGRAM TP1-PUNTO-A.
