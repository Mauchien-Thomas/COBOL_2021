      ***************************
      * début du programme  *
      ***************************
        IDENTIFICATION DIVISION. 
       PROGRAM-ID.  prob1.    
       DATE-WRITTEN.  08/03/2021. 
       ENVIRONMENT DIVISION.    
       CONFIGURATION SECTION.    
       SPECIAL-NAMES. 
      * virgule reconnue comme le symbole decimal
            DECIMAL-POINT IS COMMA. 
       INPUT-OUTPUT SECTION.  
       FILE-CONTROL.
      
      * déclaration fichier séquentiel
           SELECT ffilm 
           ASSIGN TO FILE-NAME
           ORGANIZATION SEQUENTIAL
           ACCESS SEQUENTIAL
           FILE STATUS IS FILE-STATUS.
               
      *
       DATA DIVISION.
       FILE SECTION.    
      * déclaration enregistrement d'un film dans le fichier séquentiel
       FD  ffilm.
       01  film-enr.
            02  ffilm-num      PIC 9(6). 
            02  ffilm-nom      PIC X(50). 
            02  ffilm-mot1     PIC X(20). 
            02  ffilm-mot2     PIC X(20). 
           
      
       WORKING-STORAGE SECTION. 
      * statut fichier lors de l'ouverture fichier n'existe pas (code statut 35) ou existe (code statut 0)
       01  FILE-STATUS             PIC X(2).
           88  FILE-NOT-FOUND      VALUE "35".
       01  REDEFINES FILE-STATUS   PIC X.
           88  FILE-OKAY           VALUE "0".
      
      * structure stockee sous forme de chaine
       01  film. 
           02  film-num      PIC 9(6). 
           02  film-nom      PIC X(50). 
           02  film-mot1     PIC X(20). 
           02  film-mot2     PIC X(20).  
      
      * déclaration variable nom fichier séquentiel : 8 caractères maxi + extension   
       77  file-name     PIC X(20) VALUE "film.dat".
               
      * indicateur de fin de fichier
       77  fin-fichier PIC 9.
      * indicateur d'interruption d'affichage
       77  fin-affi    PIC 9.
      
      * declaration couleurs posibles pour texte et fond
       01  color-table USAGE comp-1. 
           03  black            PIC 9(5) VALUE 1. 
           03  blue             PIC 9(5) VALUE 2. 
           03  green            PIC 9(5) VALUE 3. 
           03  cyan             PIC 9(5) VALUE 4. 
           03  red              PIC 9(5) VALUE 5. 
           03  magenta          PIC 9(5) VALUE 6. 
           03  brown            PIC 9(5) VALUE 7. 
           03  white            PIC 9(5) VALUE 8.  
           03  backgrnd-black   PIC 9(5) VALUE 32. 
           03  backgrnd-blue    PIC 9(5) VALUE 64. 
           03  backgrnd-green   PIC 9(5) VALUE 96. 
           03  backgrnd-cyan    PIC 9(5) VALUE 128. 
           03  backgrnd-red     PIC 9(5) VALUE 160. 
           03  backgrnd-magenta PIC 9(5) VALUE 192. 
           03  backgrnd-brown   PIC 9(5) VALUE 224. 
           03  backgrnd-white   PIC 9(5) VALUE 256. 
      
      * lignes preparees pour l'affichage 
       01  ligne-tirets. 
           02  FILLER PIC x(80) VALUE ALL "-". 
       01  ligne-titre. 
           02  FILLER PIC x VALUE "|". 
           02  FILLER PIC x(4) VALUE ALL SPACES. 
           02  FILLER PIC x(70) VALUE 
               "IUT DEPT INFO - fichier FILM.DAT". 
           02  FILLER PIC x(4) VALUE ALL SPACES. 
           02  FILLER PIC x VALUE "|". 
       01  ligne-cadre. 
           02  FILLER PIC x VALUE "|". 
           02  FILLER PIC x(78) VALUE ALL SPACES.  
           02  FILLER PIC x VALUE "|".
            
      * déclaration des variables
       77 nombre PIC 9(6).
       77 autre PIC X.
       77 quitter PIC x.
       77  reponse PIC X.
       77  wchoix   PIC X.
       77  wmsg_erreur PIC x(70).
       77  lig      PIC 99.
       77  lig2      PIC 99.
       77  i        PIC 99.
       77 nbre2  PIC 9(6).
            
      
      * liste des écrans ou des parties d'écran
       SCREEN SECTION. 
       01  ecran-titre. 
           02  LINE 1 COL  1 PIC x(80) FROM ligne-tirets. 
           02  LINE 2 COL  1 PIC x(80) FROM ligne-titre. 
           02  LINE 3 COL  1 PIC x(80) FROM ligne-tirets. 
      
       01  ecran-cadre. 
           02  LINE  4 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE  5 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE  6 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE  7 COL 1 PIC x(80) FROM ligne-cadre.  
           02  LINE  8 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE  9 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 10 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 11 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 12 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 13 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 14 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 15 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 16 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 17 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 18 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 19 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 20 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 21 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 22 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 23 COL 1 PIC x(80) FROM ligne-cadre. 
           02  LINE 24 COL 1 PIC x(80) FROM ligne-cadre.   
           02  LINE 25 COL 1 PIC x(80) FROM ligne-tirets. 
           
       01  ecran-menu. 
           02  LINE  5 COL 39 COLOR green VALUE "MENU". 
           02  LINE  7 COL 22  
               VALUE "Saisie de films...................1". 
           02  LINE  9 COL 22  
               VALUE "Liste des films (un par un).......2". 
           02  LINE 11 COL 22  
               VALUE "Liste des films (paquets de 8)....3". 
           02  LINE 13 COL 22  
               VALUE "Recherche par numero..............4".  
           02  LINE 17 COL 22
               VALUE "Quitter...........................Q".   
           02  LINE 20 COL 22
               VALUE "             ==> Votre choix : ".
      
              02  HIGHLIGHT COLOR blue UPPER PIC X USING wchoix.
      *saisie film
       01  ecran-un-film. 
           02  LINE  5 COL  8 VALUE "NUMERO    : ".  
           02  LINE 5  COL  20 HIGHLIGHT COLOR brown
           PIC 9(6) FROM film-num.
           02  LINE  8 COL  8 VALUE "NOM : ".  
           02  REQUIRED HIGHLIGHT PIC x(50) USING film-nom. 
           02  LINE 10 COL  8 VALUE "MOT CLE 1 : ".  
           02  REQUIRED HIGHLIGHT PIC x(20) USING film-mot1.  
           02  LINE 12 COL  8 VALUE "MOT CLE 2 : ".  
           02  REQUIRED HIGHLIGHT PIC x(20) USING film-mot2. 
      
      *liste de film par paquet de 8    
       01  ecran-affi-film-paquet. 
           02  LINE lig COL 8 COLOR brown PIC 9(6) FROM film-num. 
           02  LINE lig COL 16 PIC x(50) FROM film-nom. 
           02  LINE lig2 COL 40 PIC x(20) FROM film-mot1. 
           02  LINE lig2 COL 20 PIC x(20) FROM film-mot2. 
      
       01  ecran-rech-num. 
           02  LINE  5 COL  8 VALUE "NUMERO   : ". 
           02  REQUIRED HIGHLIGHT COLOR brown  
                 PIC 9(6)  USING nombre. 
      
      * validation creation d'un film 
       01  ecran-valide-create. 
              02  LINE 22 COL 8 
              VALUE "Valider creation (repondre par O/N): ". 
              02 HIGHLIGHT COLOR blue UPPER PIC X USING reponse.
            02  LINE 23 COL 8 
            VALUE "Autre film ?(repondre par O/N): ". 
            02  HIGHLIGHT COLOR blue UPPER PIC X USING autre.  
           
      *si la recherche du numéro est bonne
         01 ecran-retour.
               02  LINE 23 COL  8 VALUE 
           "Appuyer sur la touche Entree pour continuer ...". 
         
        01 ecran-stop-saisie.
            02  COLOR green LINE 17 COL 8 VALUE
           "L'appuie sur Entree stoppe la saisie du film.". 
      
      *si tout le fichier est parcouru
       01  ecran-msg-affi. 
           02  LINE 21 COL 8 VALUE "Il n'y a plus de film.". 
           02  LINE 23 COL  8 VALUE 
           "Appuyer sur la touche Entree pour continuer ...". 
          
       01 ecran-msg-affi-2. 
           02  LINE 23 COL 8 
               VALUE "Quitter l'affichage (repondre par O/N): ". 
           02  HIGHLIGHT UPPER PIC X USING autre.
           
      *recherche d'un numero inexistant
       01  ecran-num-erreur. 
           02  LINE 22 COL 8 VALUE "Le numero n'existe pas.". 
              
      * affichage message d'erreur      
       01  ecran-erreur. 
           02  LINE 22 COL 6 COLOR magenta PIC x(70) FROM wmsg_erreur. 
           02  LINE 23 COL 6 COLOR cyan  
               VALUE "Appuyer sur la touche Entree...".  
                  
       PROCEDURE DIVISION.
      
       DECLARATIVES.
       FILE-ERROR SECTION.
           USE AFTER STANDARD ERROR PROCEDURE
               ON ffilm.   
       END DECLARATIVES.
      
       main.
            PERFORM affi-titre.
           PERFORM WITH TEST AFTER UNTIL wchoix = "Q"
             PERFORM affi-menu
             IF wchoix > "1" AND < "4"
               OPEN INPUT ffilm
               IF file-not-found
                 MOVE ALL SPACES TO wmsg_erreur
                 MOVE
                "Le fichier séquentiel des film n'existe pas !"
                    TO wmsg_erreur
                 DISPLAY ecran-erreur
                 PERFORM get-return
                 MOVE "0" TO wchoix
               ELSE CLOSE ffilm
               END-IF
               END-IF        
      
            EVALUATE wchoix
             WHEN "1"
                  PERFORM saisie2
         WHEN "2"
                  PERFORM affi-fichier-seq    
        WHEN "3"
                   PERFORM affi-fichier-seq-paquet
         WHEN "4"
                   PERFORM affi-rech-num
                              
             END-EVALUATE
           END-PERFORM.    
       fin.
           STOP RUN.
      
       clearscreen.
            DISPLAY "" NO ADVANCING WITH BLANK SCREEN.
      
       get-return.
      * attend l'appui sur la touche Entrée
           ACCEPT OMITTED.
      
       affi-menu.
           PERFORM affi-titre.
           DISPLAY ecran-menu.
           ACCEPT  ecran-menu.
       affi-titre.
           DISPLAY ecran-titre.    
           DISPLAY ecran-cadre.
      
       saisie2.
             PERFORM WITH TEST AFTER UNTIL autre = 'N'
              PERFORM clearscreen
              PERFORM saisie
             END-PERFORM.
      
       saisie.
               OPEN INPUT ffilm. 
               MOVE 0 TO fin-fichier i.        
               PERFORM WITH TEST AFTER UNTIL fin-fichier = 1 
                READ ffilm NEXT
                AT END MOVE 1 TO fin-fichier,MOVE ffilm-num TO nbre2             
                END-READ 
               END-PERFORM
               CLOSE ffilm.
               ADD 1 TO nbre2 
                
                    OPEN INPUT ffilm.
                  IF FILE-NOT-FOUND
                    OPEN OUTPUT ffilm
                  ELSE
                    CLOSE ffilm
                    OPEN EXTEND ffilm      
                  END-IF.
                  MOVE ALL SPACES TO film.
                  PERFORM affi-titre.
                  MOVE nbre2 TO film-num           
                  DISPLAY ecran-un-film.
                  DISPLAY ecran-stop-saisie.
                  ACCEPT  ecran-un-film.
                  DISPLAY ecran-valide-create.
                  ACCEPT  ecran-valide-create.
                  IF reponse ='O'                    
                  MOVE film TO film-enr                                             
                    WRITE film-enr
                  END-IF
                 CLOSE ffilm.
       affi-fichier-seq.
           OPEN INPUT ffilm.
           MOVE 0 TO fin-fichier i.
           DISPLAY ecran-cadre.   
               
           PERFORM WITH TEST AFTER UNTIL fin-fichier = 1
             READ ffilm NEXT
               AT END MOVE 1 TO fin-fichier
               NOT AT END 
                 MOVE film-enr TO film
                 ADD 1 TO i
                 COMPUTE lig = 5 + i
                 DISPLAY ecran-un-film
                 IF i = 1
                   DISPLAY ecran-msg-affi-2
                   ACCEPT ecran-msg-affi-2            
                   IF autre = 'N'
                     MOVE 1 TO fin-fichier
                      
                   END-IF
                   MOVE 0 TO i                   
                   DISPLAY ecran-cadre      
                      
                 END-IF
             END-READ
           END-PERFORM.
           DISPLAY ecran-msg-affi.
           PERFORM get-return.
           CLOSE ffilm.
      
       affi-fichier-seq-paquet.
      
           OPEN INPUT ffilm. 
           MOVE 0 TO fin-fichier i. 
           DISPLAY ecran-cadre.    
           MOVE 0 TO lig  
           PERFORM WITH TEST AFTER UNTIL fin-fichier = 1 
             READ ffilm NEXT
               AT END MOVE 1 TO fin-fichier
               NOT AT END   
                 MOVE film-enr TO film
                 ADD 2 TO i
                 
                 COMPUTE lig = 3 + i
                 COMPUTE lig2 = 4 + i
                 
                 DISPLAY ecran-affi-film-paquet
                 IF i = 16
                   DISPLAY ecran-msg-affi-2
                   ACCEPT ecran-msg-affi-2            
                   IF autre = 'N'
                     MOVE 1 TO fin-fichier
                      
                   END-IF 
         
                   MOVE 0 TO i
                   DISPLAY ecran-cadre      
                   MOVE 5 TO lig
                   MOVE 5 TO lig2
                 END-IF
             END-READ
           END-PERFORM.
           DISPLAY ecran-msg-affi.
           PERFORM get-return.
           CLOSE ffilm.
      
       affi-rech-num.
            OPEN INPUT ffilm.
           MOVE ALL SPACES TO film-num.
           MOVE 0 TO fin-fichier i.
           MOVE "" TO nombre.
           DISPLAY ecran-cadre.
           DISPLAY ecran-rech-num.
           DISPLAY ecran-retour
            ACCEPT  ecran-rech-num.
                      
              PERFORM WITH TEST AFTER UNTIL fin-fichier = 1 
                READ ffilm   
                  AT END MOVE 1 TO fin-fichier
                     NOT AT END
                       MOVE film-enr TO film
                        IF nombre=film-num
                          MOVE 1 TO fin-fichier 
                           DISPLAY ecran-un-film 
                            ACCEPT ecran-un-film                                    
                        END-IF
                      END-READ
                 END-PERFORM. 
                    
                IF NOT nombre = film-num
                  DISPLAY ecran-num-erreur
                  PERFORM get-return
                 END-IF.
                CLOSE ffilm.
