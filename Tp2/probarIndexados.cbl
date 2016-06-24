      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TIEMPOS         ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE IS SEQUENTIAL
                                  RECORD KEY IS TIE-CLAVE
                                  ALTERNATE RECORD KEY IS TIE-CUIT
                                  WITH DUPLICATES
                                  FILE STATUS IS FS-TIEMPOS.

           SELECT CONSULTORES     ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE IS SEQUENTIAL
                                  RECORD KEY IS CONS-NUM
                                  FILE STATUS IS FS-CONSULTORES.

           SELECT EMPRESAS        ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE IS SEQUENTIAL
                                  RECORD KEY IS EMP-COD
                                  ALTERNATE RECORD KEY IS EMP-CUIT
                                  FILE STATUS IS FS-EMPRESAS.

           SELECT TARIFAS         ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE IS SEQUENTIAL
                                  RECORD KEY IS TAR-CLAVE
                                  FILE STATUS IS FS-TARIFAS.

           SELECT PRUEBA-IND      ASSIGN TO DISK
                                  ORGANIZATION IS SEQUENTIAL
                                  FILE STATUS IS FS-PRUEBA.
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

       FD PRUEBA-IND LABEL RECORD IS STANDARD
         VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algorit
      - 'mos-4/Tp2/Archivos de prueba/Secuenciales/pruebaIndexados.dat'.

       01 LINEA PIC X(80).

       WORKING-STORAGE SECTION.

       77 FS-TIEMPOS PIC XX.
       77 FS-CONSULTORES PIC XX.
       77 FS-EMPRESAS PIC XX.
       77 FS-TARIFAS PIC XX.
       77 FS-PRUEBA PIC XX.

       01 LINEA-VACIA PIC X(80) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN INPUT TIEMPOS CONSULTORES EMPRESAS TARIFAS.
            OPEN OUTPUT PRUEBA-IND.

            READ TIEMPOS.
            PERFORM IMPRIMIR-TIE UNTIL FS-TIEMPOS = '10'.

            WRITE LINEA FROM LINEA-VACIA BEFORE 1.
            READ CONSULTORES.
            PERFORM IMPRIMIR-CONS UNTIL FS-CONSULTORES = '10'.

            WRITE LINEA FROM LINEA-VACIA BEFORE 1.
            READ EMPRESAS.
            PERFORM IMPRIMIR-EMP UNTIL FS-EMPRESAS = '10'.

            WRITE LINEA FROM LINEA-VACIA BEFORE 1.
            READ TARIFAS.
            PERFORM IMPRIMIR-TAR UNTIL FS-TARIFAS = '10'.

            CLOSE TIEMPOS CONSULTORES EMPRESAS TARIFAS PRUEBA-IND.

            STOP RUN.

       IMPRIMIR-TIE.
           WRITE LINEA FROM TIE-REG BEFORE 1.
           READ TIEMPOS.

       IMPRIMIR-CONS.
           WRITE LINEA FROM CONS-REG BEFORE 1.
           READ CONSULTORES.

       IMPRIMIR-EMP.
           WRITE LINEA FROM EMP-REG BEFORE 1.
           READ EMPRESAS.

       IMPRIMIR-TAR.
           WRITE LINEA FROM TAR-REG BEFORE 1.
           READ TARIFAS.

       END PROGRAM YOUR-PROGRAM-NAME.
