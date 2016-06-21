      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OBTENER-EMPRESA.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPRESAS        ASSIGN TO DISK
                                  ORGANIZATION IS INDEXED
                                  ACCESS MODE IS RANDOM
                                  RECORD KEY IS EMP-COD
                                  ALTERNATE RECORD KEY IS EMP-CUIT
                                  FILE STATUS IS FS-EMPRESAS.
       DATA DIVISION.

       FILE SECTION.
       FD EMPRESAS LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS  '/home/miki/Escritorio/Algos 4/Tps-Algor
      -    'itmos-4/Tp2/Archivos de prueba/Indexados/empresas.dat'.

       01 EMP-REG.
           03 EMP-COD  PIC 9(3).
           03 EMP-RAZ  PIC X(25).
           03 EMP-DIR  PIC X(20).
           03 EMP-TEL  PIC X(20).
           03 EMP-CUIT PIC 9(11).

       WORKING-STORAGE SECTION.
       77 FS-EMPRESAS PIC XX.

       LINKAGE SECTION.
           01 OPCION PIC X.
           01 CUIT PIC 9(11).
           01 RAZON-SOCIAL PIC X(25).
       PROCEDURE DIVISION USING OPCION,CUIT,RAZON-SOCIAL.
       MAIN-PROCEDURE.
            IF OPCION = 'A'
                PERFORM ABRIR-EMPRESA.
            IF OPCION = 'P'
                PERFORM PROCESAR-EMPRESA.
            IF OPCION = 'C'
                CLOSE EMPRESAS.
            STOP RUN.

       ABRIR-EMPRESA.
           OPEN INPUT EMPRESAS.
           IF FS-EMPRESAS NOT = ZERO
               DISPLAY "ERROR AL ABRIR EMPRESAS FS: " FS-EMPRESAS
               CLOSE EMPRESAS
               STOP RUN.

       PROCESAR-EMPRESA.
           MOVE CUIT TO EMP-CUIT.
           PERFORM LEER-EMPRESAS.
           IF FS-EMPRESAS = '00'
               MOVE EMP-RAZ TO RAZON-SOCIAL.

       LEER-EMPRESAS.
           READ EMPRESAS RECORD KEY IS EMP-CUIT.
           IF FS-EMPRESAS NOT = ZERO AND '10'
               DISPLAY "ERROR AL LEER EMPRESAS FS: " FS-EMPRESAS
               CLOSE EMPRESAS
               STOP RUN.

       END PROGRAM OBTENER-EMPRESA.
