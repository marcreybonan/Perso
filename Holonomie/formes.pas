{$A+,B-,D-,E-,F-,I-,L-,N-,O-,R-,S+,V-}
{$M 16384,0,655360}

PROGRAM ESSAI_FORMES;

USES  CRT , DOS ;

CONST    MAX=9; {n'accepte pas 10, très long pour dim=8}

TYPE
          LISTEPTR=^noeud;
          WORD_BIS=ARRAY[0..1] OF WORD;
          noeud  =  RECORD
                           INFO: WORD_BIS;
                           COEF: LONGINT;
                           NEXT: listeptr
                      END;
          ARSHORTINT=ARRAY[0..MAX] OF SHORTINT;
VAR
         ORIGINAL, PREMIER: LISTEPTR;

     cod        : INTEGER      ;
     Timeo      : REAL         ;

{ON CHANGE LA DEFINITION DE K_0 EN -0.5K_0?}
{attention à oui67/ choisir 67 non}

CONST   DIMENSION:SHORTINT=6;
        DEGRE:SHORTINT=3;
        SPN:BOOLEAN=FALSE;
        FI:WORD_BIS=(0,0);
        INFI:WORD_BIS=($FFFF,$FFFF);

{$I saisie8.INC}

FUNCTION TIME  :  REAL  ;
VAR
     h ,
     m ,
     s ,
     c : WORD ;
BEGIN
      GetTime ( h , m , s , c );
      Time :=  c / 100.0  +  s  +  60 * m
END;

FUNCTION GCD(P1,P2:LONGINT):LONGINT;
VAR G:LONGINT;
BEGIN
     P2:=ABS(P2);
     P1:=ABS(P1);
     REPEAT
            IF P2>P1 THEN
            BEGIN
                 G:=P2;
                 P2:=P1;
                 P1:=G
            END;
            P1:=P1 MOD P2
      UNTIL P1=0;
      GCD:=P2
END;

PROCEDURE WAIT;
BEGIN
     IF READKEY=' ' THEN  VIDERCLE
END;


PROCEDURE WORD_SHORTINT(W:WORD_BIS;VAR H:ARSHORTINT);
VAR  P:SHORTINT;
BEGIN
     FOR P:=0 TO PRED(DIMENSION) DO
     H[P]:=(W[P DIV 5] SHR (3*(P MOD 5))) AND 7
END;


PROCEDURE DROW(W:WORD_BIS;D:WORD_BIS);
VAR H:ARSHORTINT;
    P:SHORTINT;
BEGIN
     D[0]:=0; D[1]:=0;
     WORD_SHORTINT(W,H);
     FOR P:=0 TO PRED(DIMENSION) DO
     INC(D[P DIV 5], H[P] SHL (3*((PRED(DIMENSION)-P) MOD 5)))
END;

FUNCTION PLUS_GRAND(INF,SUP:WORD_BIS):BOOLEAN;
VAR A,B:ARSHORTINT;
    R:SHORTINT;
BEGIN   
	WORD_SHORTINT(INF,A);
	WORD_SHORTINT(SUP,B);
      PLUS_GRAND:=TRUE;
      R:=0;
      REPEAT 
		IF A[R]>B[R] THEN EXIT
		ELSE IF A[R]<B[R] THEN R:=DIMENSION
		ELSE INC(R)
	UNTIL R=DIMENSION;        
	PLUS_GRAND:=FALSE   
END;

FUNCTION PLUS_PETIT(INF,SUP:WORD_BIS):BOOLEAN;
VAR A,B:ARSHORTINT;
    R:SHORTINT;
BEGIN   
	WORD_SHORTINT(INF,A);
	WORD_SHORTINT(SUP,B);
      PLUS_PETIT:=TRUE;
      R:=0;
      REPEAT 
		IF A[R]<B[R] THEN EXIT
		ELSE IF A[R]>B[R] THEN R:=DIMENSION
		ELSE INC(R)
	UNTIL R=DIMENSION;        
	PLUS_PETIT:=FALSE   
END;



PROCEDURE DECODAGE(A:ARSHORTINT);
VAR  D:SHORTINT;
BEGIN
      TextColor(14);
     D:=0;
     REPEAT
             IF d=DEGRE THEN TextColor(15);
             WRITE(A[D]);
             INC(D)
     UNTIL D=dimension;
     TextColor(7)
END;


PROCEDURE DEVOILER(INFO:WORD_BIS);
VAR  P:SHORTINT;
     STR:STRING[100];
     A:ARSHORTINT;
BEGIN
      WORD_SHORTINT(INFO,A);
      DECODAGE(A);
      TextColor(7);
      WRITE(#240);

      P:=0;
      STR:='';

      WHILE P<DEGRE DO
      BEGIN
            STR:=STR+CHAR(ORD('A')+P);
            CASE A[P] OF
            0: STR:=STR+'0';
            1: STR:=STR+'1';
            2: STR:=STR+'2';
            3: STR:=STR+'3';
            4: STR:=STR+'012';
            5: STR:=STR+'013';
            6: STR:=STR+'023';
            7: STR:=STR+'123'
            END;
            INC(P)
      END;
      TextColor(12);
      WRITE(STR);
      STR:='';

      WHILE P<DIMENSION DO
      BEGIN
            IF  A[P]=0 THEN STR:=STR+'&'
            ELSE BEGIN
                      STR:=STR+CHAR(ORD('A')+P);
                      CASE A[P] OF
                      1: STR:=STR+'01';
                      2: STR:=STR+'02';
                      3: STR:=STR+'03';
                      4: STR:=STR+'12';
                      5: STR:=STR+'13';
                      6: STR:=STR+'23';
                      7: STR:=STR+'0123'
                      END;
            END;
            INC(P)
      END;
      TextColor(13);
      WRITE(STR);
      TEXTCOLOR(7)
END;

FUNCTION OUI67(INFO:WORD_BIS):BOOLEAN;
VAR  P:SHORTINT;
     A:ARSHORTINT;
BEGIN  
{OUI67:=TRUE;exit;;}

      OUI67:=FALSE;
      WORD_SHORTINT(INFO,A);
      P:=0;
      WHILE P<DEGRE DO
      BEGIN
            CASE A[P] OF
            0: ;
            1: EXIT;
            2: EXIT;
            3: EXIT;
            4: EXIT;
            5: {EXIT;k<0;}EXIT;
            6: ;
            7: EXIT;
            END;
            INC(P)
      END;
      
      WHILE P<DIMENSION DO
      BEGIN
            CASE A[P] OF
            0: ;
            1: EXIT;
            2: {EXIT;k>0;}EXIT;
            3: EXIT;
            4: EXIT;
            5: EXIT;
            6: EXIT;
            7: ;
            END;
            INC(P)
      END;   
      OUI67:=TRUE
END;

FUNCTION EGALE(SUP,INF:WORD_BIS):BOOLEAN;
BEGIN
      EGALE:=((SUP[1]=INF[1]) AND(SUP[0]=INF[0]))
END;


FUNCTION REPRESENTANT(INFO:WORD_BIS):BOOLEAN;
VAR A:ARSHORTINT;
    P,R:SHORTINT;
BEGIN
      REPRESENTANT:=FALSE;
      WORD_SHORTINT(INFO,A);

      FOR R:=0 TO PRED(DEGRE) DO
      FOR P:=SUCC(R) TO PRED(DEGRE) DO
          IF A[P]>A[R] THEN EXIT;

      FOR R:=DEGRE TO PRED(DIMENSION) DO
      FOR P:=SUCC(R) TO PRED(DIMENSION) DO
          IF A[P]>A[R] THEN EXIT;

      REPRESENTANT:=TRUE
END;


FUNCTION FILTRE(INFO:WORD_BIS):BOOLEAN;
VAR A:ARSHORTINT;
    R:SHORTINT;
BEGIN
        WORD_SHORTINT(INFO,A);
        FILTRE:=FALSE;
        IF A[0]<4 THEN EXIT;
        IF A[1]>3 THEN EXIT;
        FOR R:=2 TO 5 DO
        IF (A[R]=7) OR (A[R]=0)  THEN EXIT;
        FILTRE:=TRUE
END;


PROCEDURE CREER_PTR(VAR LISTE:LISTEPTR);
BEGIN
        NEW(LISTE);
        LISTE^.INFO:=INFI;
        LISTE^.COEF:=0;
        LISTE^.NEXT:=NIL
END;

PROCEDURE SWAP_PTR(VAR PREMIER:LISTEPTR;VAR SECOND:LISTEPTR);
VAR q:listeptr;
BEGIN
        CREER_PTR(Q);
        q^.NEXT:=PREMIER^.NEXT;
        PREMIER^.NEXT:=SECOND^.NEXT;
        SECOND^.NEXT:=q^.NEXT;
        DISPOSE(Q)
END;


PROCEDURE SWAP;
VAR q:listeptr;
BEGIN     {swap_ptr(premier,original)}
        CREER_PTR(q);
        q^.NEXT:=original^.NEXT;
        original^.NEXT:=premier^.NEXT;
        premier^.NEXT:=q^.NEXT;
        DISPOSE(q)
END;


PROCEDURE READ_AND_DESTROY(VAR INFO:WORD_BIS;VAR COEF:LONGINT);
VAR q:listeptr;
BEGIN
     INFO:=original^.NEXT^.INFO;
     COEF:=original^.NEXT^.COEF;
     IF original^.NEXT<>NIL THEN
     BEGIN
          q:=original^.NEXT^.NEXT;
          DISPOSE(original^.NEXT);
          original^.NEXT:=q
     END
END;


PROCEDURE ANIL_PTR(VAR LISTE:LISTEPTR);
VAR q:listeptr;
BEGIN
       WHILE LISTE^.NEXT<>NIL DO
       BEGIN
            Q:=LISTE^.NEXT;
            LISTE^.NEXT:=LISTE^.NEXT^.NEXT;
            DISPOSE(Q);
            Q:=NIL
       END
END;

PROCEDURE DETRUIRE_PTR(VAR LISTE:LISTEPTR);
BEGIN
      ANIL_PTR(LISTE);
      DISPOSE(LISTE)
END;


PROCEDURE INITIER_PTR;
BEGIN
      CREER_PTR(PREMIER);
      CREER_PTR(ORIGINAL)
END;

PROCEDURE ZERO_PTR;
BEGIN
      ANIL_PTR(ORIGINAL);
      ANIL_PTR(PREMIER);
END;

PROCEDURE INSERER(INFO:WORD_BIS;COEF:LONGINT);
VAR  q,r:listeptr;
BEGIN
       IF COEF=0 THEN EXIT;
       r:=premier;

       WHILE ((r^.NEXT<>NIL) AND PLUS_PETIT(r^.NEXT^.INFO,INFO))
            DO r:=r^.NEXT;
       IF EGALE(r^.NEXT^.INFO,INFO)
       THEN BEGIN
                  INC(r^.NEXT^.COEF,COEF);
                  IF r^.NEXT^.COEF=0
                  THEN BEGIN
                         q:= r^.NEXT^.NEXT;
                         DISPOSE(r^.NEXT);
                         r^.NEXT:=q
                       END
              END
       ELSE
              BEGIN
                  NEW(q);
                  q^.INFO:=INFO;
                  q^.COEF:=COEF;
                  q^.NEXT:=r^.NEXT;
                  r^.NEXT:=q
             END
END;

PROCEDURE INSERER_BIS(INFO:WORD_BIS;COEF:LONGINT);
VAR  q,r:listeptr;
BEGIN
       r:=premier;
       WHILE ((r^.NEXT<>NIL) AND PLUS_GRAND(r^.NEXT^.INFO,INFO))
            DO  r:=r^.NEXT;

       NEW(q);
       q^.INFO:=INFO;
       q^.COEF:=COEF;
       q^.NEXT:=r^.NEXT;
       r^.NEXT:=q
END;

PROCEDURE FILTRER_BIS;
VAR   INFO:WORD_BIS;
      COEF:LONGINT;
BEGIN
       WRITELN;
       WRITE('JE FILTRE...',#7);
       CLREOL;
       SWAP;

       WHILE ORIGINAL^.NEXT<>NIL DO
       BEGIN
            READ_AND_DESTROY(INFO,COEF);
            IF FILTRE(INFO) THEN
            INSERER_BIS(INFO,COEF)
       END
END;

PROCEDURE INSERER_NET(INFO:WORD_BIS;COEF:LONGINT);
VAR  Q:LISTEPTR;
BEGIN
            NEW(Q);
            Q^.INFO:=INFO;
            Q^.COEF:=COEF;
            Q^.NEXT:=PREMIER^.NEXT;
            PREMIER^.NEXT:=Q
END;

PROCEDURE LECTURE(DEPART:LISTEPTR);
BEGIN

       IF DEPART=PREMIER THEN
        DEPART:=PREMIER^.NEXT;

       IF DEPART=NIL THEN WRITELN('ZERO');
       WHILE DEPART<>NIL DO
       BEGIN
             WITH DEPART^ DO
             IF REPRESENTANT(INFO)AND Oui67(INFO) 
             THEN
             BEGIN
                   WRITELN;
                   IF COEF>0 THEN TextColor(10)
                   ELSE IF COEF<0 THEN TextColor(12)
                   ELSE TextColor(15);

                    WRITE(COEF: 8,' ');

                    DEVOILER(INFO)

             END;

             DEPART:=DEPART^.NEXT

       END;
       VIDERCLE;
		WAIT

END;

PROCEDURE  EXHIBER;
BEGIN
                 WRITELN('R‚sultat');
                 CLREOL;
                 TEXTCOLOR(WHITE);
                 LECTURE(premier)
END;



PROCEDURE CLASSER;
VAR   INFO:WORD_BIS;COEF:LONGINT;
BEGIN
       WRITELN;
       WRITE('JE CLASSE...',#7);
       CLREOL;
       SWAP;

       WHILE ORIGINAL^.NEXT<>NIL DO
       BEGIN
            READ_AND_DESTROY(INFO,COEF);
            INSERER(INFO,COEF)
       END;
       {EXHIBER}
END;


PROCEDURE CHOISIR;
VAR   INFO:WORD_BIS;
      COEF:LONGINT;
BEGIN
       WRITELN;
       WRITELN('JE CHOISIS UN REPRESENTANT...');
       CLREOL;
       SWAP;

       WHILE ORIGINAL^.NEXT<>NIL DO
       BEGIN
            READ_AND_DESTROY(INFO,COEF);
            IF REPRESENTANT(INFO) THEN
            INSERER(INFO,COEF)
       END
END;

PROCEDURE FILTRER67;
VAR  Q,R:LISTEPTR;
BEGIN  
       WRITELN;
       WRITE('JE FILTRE..67',#7);
       CLREOL;
       R:=PREMIER;
       WHILE R^.NEXT<>NIL DO
       BEGIN
            IF oui67(R^.NEXT^.INFO)
            THEN  R:=R^.NEXT
            ELSE  BEGIN
                 Q:=R^.NEXT^.NEXT;
                 DISPOSE(R^.NEXT);
                 R^.NEXT:=Q
            END
       END
END;


PROCEDURE FILTRER;
VAR  Q,R:LISTEPTR;
BEGIN  
       WRITELN;
       WRITE('JE FILTRE...',#7);
       CLREOL;
       R:=PREMIER;
       WHILE R^.NEXT<>NIL DO
       BEGIN
            IF FILTRE(R^.NEXT^.INFO)
            THEN  R:=R^.NEXT
            ELSE  BEGIN
                 Q:=R^.NEXT^.NEXT;
                 DISPOSE(R^.NEXT);
                 R^.NEXT:=Q
            END
       END
END;

PROCEDURE Y_a(INFO:WORD_BIS;COEF:LONGINT);
VAR  A,P,Q,BIS:SHORTINT;
PROCEDURE SUITE(INF:WORD;NCOEF:LONGINT);
VAR NINFO:WORD_BIS;
BEGIN
       NINFO[BIS]:=INF;
       NINFO[1-BIS]:=INFO[1-BIS];
       INSERER(NINFO,NCOEF)
END;
BEGIN
        write('');
        IF COEF=0 THEN EXIT;

        FOR Q:=0 TO PRED(DIMENSION) DO
        BEGIN
              BIS:=Q DIV 5;
              P:=Q MOD 5;
              A:=(INFO[BIS]  SHR  (3*P)) AND 7;

              IF Q<DEGRE THEN BEGIN
              IF A=1 THEN  SUITE(INFO[BIS]+(3  SHL  (3*P)),-COEF) ELSE
              IF A=3 THEN  SUITE(INFO[BIS]+(3  SHL  (3*P)),COEF);
              END

              ELSE BEGIN
              IF A=0 THEN  SUITE(INFO[BIS]+(2  SHL  (3*P)),COEF) ELSE
              IF A=5 THEN  SUITE(INFO[BIS]+(2  SHL  (3*P)),-COEF);
              END
        END
END;

PROCEDURE Y_a_b(INFO:WORD_BIS;COEF:LONGINT);
VAR  A,P,Q,BIS:SHORTINT;
PROCEDURE SUITE(INF:WORD;NCOEF:LONGINT);
VAR NINFO:WORD_BIS;
BEGIN
       write('');
       NINFO[BIS]:=INF;
       NINFO[1-BIS]:=INFO[1-BIS];
       INSERER(NINFO,NCOEF)
END;
BEGIN
        IF COEF=0 THEN EXIT;

        FOR Q:=0 TO PRED(DIMENSION) DO
        BEGIN
              BIS:=Q DIV 5;
              P:=Q MOD 5;
              A:=(INFO[BIS]  SHR  (3*P)) AND 7;

              IF Q<DEGRE THEN BEGIN
              IF (A=0) OR (A=1) THEN  SUITE(INFO[BIS]+(6  SHL  (3*P)),COEF);
              IF (A=2) OR (A=3) THEN  SUITE(INFO[BIS]+(2  SHL  (3*P)),COEF);
              END

              ELSE BEGIN
              IF (A=0) OR (A=6) THEN  SUITE(INFO[BIS]+(1  SHL  (3*P)),COEF);
              IF (A=0) OR (A=1) THEN  SUITE(INFO[BIS]+(6  SHL  (3*P)),COEF);
              END
        END
END;

PROCEDURE Y_a_2b(INFO:WORD_BIS;COEF:LONGINT);
VAR  A,P,Q,BIS:SHORTINT;
PROCEDURE SUITE(INF:WORD;NCOEF:LONGINT);
VAR NINFO:WORD_BIS;
BEGIN
       NINFO[BIS]:=INF;
       NINFO[1-BIS]:=INFO[1-BIS];
       INSERER(NINFO,NCOEF)
END;
BEGIN
        write('');
        IF COEF=0 THEN EXIT;

        FOR Q:=0 TO PRED(DIMENSION) DO
        BEGIN
              BIS:=Q DIV 5;
              P:=Q MOD 5;
              A:=(INFO[BIS]  SHR  (3*P)) AND 7;
              IF A=0 THEN  SUITE(INFO[BIS]+(5  SHL  (3*P)),COEF)  ELSE
              IF A=2 THEN  SUITE(INFO[BIS]+(5  SHL  (3*P)),-COEF);
        END
END;

PROCEDURE Y_b(INFO:WORD_BIS;COEF:LONGINT);
VAR  A,P,Q,BIS:SHORTINT;
PROCEDURE SUITE(INF:WORD;NCOEF:LONGINT);
VAR NINFO:WORD_BIS;
BEGIN
       NINFO[BIS]:=INF;
       NINFO[1-BIS]:=INFO[1-BIS];
       INSERER(NINFO,NCOEF)
END;
BEGIN
        write('');
        IF COEF=0 THEN EXIT;

        FOR Q:=0 TO PRED(DIMENSION) DO
        BEGIN
              BIS:=Q DIV 5;
              P:=Q MOD 5;
              A:=(INFO[BIS]  SHR  (3*P)) AND 7;

              IF Q<DEGRE THEN BEGIN
              IF (A=0) OR (A=4) THEN  SUITE(INFO[BIS]+(3  SHL  (3*P)),COEF);
              IF (A=2) OR (A=6) THEN  SUITE(INFO[BIS]-(1  SHL  (3*P)),-COEF);
              END

              ELSE BEGIN
              IF (A=1) OR (A=2) THEN  SUITE(INFO[BIS]+(4  SHL  (3*P)),-COEF);
              IF (A=2) OR (A=6) THEN  SUITE(INFO[BIS]-(1  SHL  (3*P)),-COEF);
              END
        END
END;

PROCEDURE X_a(INFO:WORD_BIS;COEF:LONGINT);
VAR  A,P,Q,BIS:SHORTINT;
PROCEDURE SUITE(INF:WORD;NCOEF:LONGINT);
VAR NINFO:WORD_BIS;
BEGIN

       NINFO[BIS]:=INF;
       NINFO[1-BIS]:=INFO[1-BIS];
       INSERER(NINFO,NCOEF)
END;
BEGIN
        write('');
        IF COEF=0 THEN EXIT;

        FOR Q:=0 TO PRED(DIMENSION) DO
        BEGIN
              BIS:=Q DIV 5;
              P:=Q MOD 5;
              A:=(INFO[BIS]  SHR  (3*P)) AND 7;

              IF Q<DEGRE THEN BEGIN
              IF A=4 THEN  SUITE(INFO[BIS]-(3  SHL  (3*P)),-COEF) ELSE
              IF A=6 THEN  SUITE(INFO[BIS]-(3  SHL  (3*P)),COEF);
              END

              ELSE BEGIN
              IF A=2 THEN  SUITE(INFO[BIS]-(2  SHL  (3*P)),COEF) ELSE
              IF A=7 THEN  SUITE(INFO[BIS]-(2  SHL  (3*P)),-COEF);
              END
        END
END;


PROCEDURE X_a_b(INFO:WORD_BIS;COEF:LONGINT);
VAR  A,P,Q,BIS:SHORTINT;
PROCEDURE SUITE(INF:WORD;NCOEF:LONGINT);
VAR NINFO:WORD_BIS;
BEGIN
       NINFO[BIS]:=INF;
       NINFO[1-BIS]:=INFO[1-BIS];
       INSERER(NINFO,NCOEF)
END;
BEGIN
        write('');
        IF COEF=0 THEN EXIT;

        FOR Q:=0 TO PRED(DIMENSION) DO
        BEGIN
              BIS:=Q DIV 5;
              P:=Q MOD 5;
              A:=(INFO[BIS]  SHR  (3*P)) AND 7;

              IF Q<DEGRE THEN BEGIN
              IF (A=6) OR (A=7) THEN  SUITE(INFO[BIS]-(6  SHL  (3*P)),COEF);
              IF (A=4) OR (A=5) THEN  SUITE(INFO[BIS]-(2  SHL  (3*P)),COEF);
              END

              ELSE BEGIN
              IF (A=1) OR (A=7) THEN  SUITE(INFO[BIS]-(1  SHL  (3*P)),COEF);
              IF (A=6) OR (A=7) THEN  SUITE(INFO[BIS]-(6  SHL  (3*P)),COEF);
              END
        END
END;

PROCEDURE X_a_2b(INFO:WORD_BIS;COEF:LONGINT);
VAR  A,P,Q,BIS:SHORTINT;
PROCEDURE SUITE(INF:WORD;NCOEF:LONGINT);
VAR NINFO:WORD_BIS;
BEGIN
       NINFO[BIS]:=INF;
       NINFO[1-BIS]:=INFO[1-BIS];
       INSERER(NINFO,NCOEF)
END;
BEGIN
        write('');
        IF COEF=0 THEN EXIT;

        FOR Q:=0 TO PRED(DIMENSION) DO
        BEGIN
              BIS:=Q DIV 5;
              P:=Q MOD 5;
              A:=(INFO[BIS]  SHR  (3*P)) AND 7;
              IF A=5 THEN  SUITE(INFO[BIS]-(5  SHL  (3*P)),COEF)  ELSE
              IF A=7 THEN  SUITE(INFO[BIS]-(5  SHL  (3*P)),-COEF);
        END
END;

PROCEDURE X_b(INFO:WORD_BIS;COEF:LONGINT);
VAR  A,P,Q,BIS:SHORTINT;
PROCEDURE SUITE(INF:WORD;NCOEF:LONGINT);
VAR NINFO:WORD_BIS;
BEGIN
       NINFO[BIS]:=INF;
       NINFO[1-BIS]:=INFO[1-BIS];
       INSERER(NINFO,NCOEF)
END;
BEGIN
        write('');
        IF COEF=0 THEN EXIT;

        FOR Q:=0 TO PRED(DIMENSION) DO
        BEGIN
              BIS:=Q DIV 5;
              P:=Q MOD 5;
              A:=(INFO[BIS]  SHR  (3*P)) AND 7;

              IF Q<DEGRE THEN BEGIN
              IF (A=3) OR (A=7) THEN  SUITE(INFO[BIS]-(3  SHL  (3*P)),COEF);
              IF (A=1) OR (A=5) THEN  SUITE(INFO[BIS]+(1  SHL  (3*P)),-COEF);
              END

              ELSE BEGIN
              IF (A=5) OR (A=6) THEN  SUITE(INFO[BIS]-(4  SHL  (3*P)),-COEF);
              IF (A=1) OR (A=5) THEN  SUITE(INFO[BIS]+(1  SHL  (3*P)),-COEF);
              END
        END
END;


PROCEDURE DUPLIQUER_PTR(ORIGINAL:LISTEPTR;VAR LISTE:LISTEPTR);
VAR p,q,r:listeptr;
BEGIN
       R:=LISTE;
       Q:=ORIGINAL^.NEXT;
       IF Q=NIL THEN WRITELN('ZERO');
       WHILE Q<>NIL DO
       BEGIN
            NEW(P);
            P^.INFO:=Q^.INFO;
            P^.COEF:=Q^.COEF;
            P^.NEXT:=R^.NEXT;
            R^.NEXT:=P;
            Q:=Q^.NEXT
       END;
       P:=NIL;
       Q:=NIL
END;

PROCEDURE AMPLIFIER(C:LONGINT);
VAR q:listeptr;
BEGIN
       write('');
       Q:=ORIGINAL^.NEXT;
       IF Q=NIL THEN WRITELN('ZERO');
       WHILE Q<>NIL DO
       BEGIN
            Q^.COEF:=C*Q^.COEF;
            Q:=Q^.NEXT
       END
END;

PROCEDURE DIVISER(G:LONGINT);
VAR Q:LISTEPTR;
BEGIN  
        IF G>0 THEN TextColor(10) ELSE TextColor(12);
        WRITELN('Division par ',G);
        Q:=PREMIER^.NEXT;
        WHILE Q<>NIL DO
        BEGIN
              Q^.COEF:=Q^.COEF DIV G;
              Q:=Q^.NEXT
        END;
   {wait}

END;


PROCEDURE MODULO(G:LONGINT);
VAR Q:LISTEPTR;
BEGIN
        TextColor(11);
        WRITELN('MODULO',G);
        Q:=PREMIER^.NEXT;
        WHILE Q<>NIL DO
        BEGIN
              Q^.COEF:=Q^.COEF MOD G;
              Q:=Q^.NEXT
        END;
   {wait}

END;


PROCEDURE REDUCTION;
VAR q:listeptr;
    G:LONGINT;
BEGIN   (*exit;*)
       Q:=PREMIER^.NEXT;
       IF Q=NIL THEN BEGIN
          WRITELN('ZERO');
          EXIT
       END;
       G:=ABS(Q^.COEF);
       WHILE (Q<>NIL) DO
       BEGIN
            IF  REPRESENTANT(Q^.INFO) THEN
            G:=GCD(G,ABS(Q^.COEF));
            Q:=Q^.NEXT
       END;
       IF G>1 THEN DIVISER(G)
END;

PROCEDURE ELIMINER;
VAR   INFO:WORD_BIS;
      COEF:LONGINT;
VAR A:ARSHORTINT;
    P,R:SHORTINT;
BEGIN         
       SWAP;
       WHILE ORIGINAL^.NEXT<>NIL DO
       BEGIN
            READ_AND_DESTROY(INFO,COEF);
            WORD_SHORTINT(INFO,A);
            FOR R:=0 TO PRED(DIMENSION) DO
            IF A[R]=5 THEN COEF:=0;
            INSERER(INFO,COEF)
       END;
       REDUCTION;
END; 

PROCEDURE ELIMINER67;
VAR   INFO:WORD_BIS;
      COEF:LONGINT;
VAR A:ARSHORTINT;
    P,R:SHORTINT;
BEGIN         
       SWAP;
       WHILE ORIGINAL^.NEXT<>NIL DO
       BEGIN
            READ_AND_DESTROY(INFO,COEF);
            WORD_SHORTINT(INFO,A);
            
            IF oui67(INFO) THEN
            INSERER(INFO,COEF)
       END;
       REDUCTION;
END; 


PROCEDURE OPERER(O:BYTE);
VAR INFO: WORD_BIS;
    COEF: LONGINT;
BEGIN
        WHILE ORIGINAL^.NEXT<>NIL DO
        BEGIN
              READ_AND_DESTROY(INFO,COEF);
              CASE O OF
              0: INSERER(INFO,COEF);
              1: Y_A(INFO,COEF);
              2: Y_A_B(INFO,COEF);
              3: Y_A_2B(INFO,COEF);
              4: Y_B(INFO,COEF);
              5: X_A(INFO,COEF);
              6: X_A_B(INFO,COEF);
              7: X_A_2B(INFO,COEF);
              8: X_B(INFO,COEF)
              END
        END
END;

PROCEDURE K1(H_b:SHORTINT);
VAR P,Q:LISTEPTR;
BEGIN
      CREER_PTR(P);
      CREER_PTR(Q);

      DUPLIQUER_PTR(ORIGINAL,P);
      AMPLIFIER(2);
      OPERER(4);
      SWAP;

      OPERER(1);

      SWAP_PTR(PREMIER,Q);

      DUPLIQUER_PTR(P,ORIGINAL);
      AMPLIFIER(-2); (*?*)
      OPERER(8);

      SWAP;
      OPERER(3);

      SWAP_PTR(ORIGINAL,P);
      AMPLIFIER(H_b);
      OPERER(2);

      SWAP_PTR(ORIGINAL,Q);
      OPERER(0);

      DETRUIRE_PTR(Q);
      DETRUIRE_PTR(P)
END;

PROCEDURE K3(H_b:SHORTINT);
VAR P,Q:LISTEPTR;
BEGIN
      CREER_PTR(P);
      CREER_PTR(Q);

      DUPLIQUER_PTR(ORIGINAL,P);
      AMPLIFIER(2);
      OPERER(4);
      SWAP;

      OPERER(7);

      SWAP_PTR(PREMIER,Q);

      DUPLIQUER_PTR(P,ORIGINAL);
      AMPLIFIER(-2);
      OPERER(8);

      SWAP;
      OPERER(5);

      SWAP_PTR(ORIGINAL,P);
      AMPLIFIER(-H_b);
      OPERER(6);

      SWAP_PTR(ORIGINAL,Q);
      OPERER(0);
       DETRUIRE_PTR(Q);
       DETRUIRE_PTR(P)
END;


{2YaYb+HbYab}

PROCEDURE oK1(Hb:SHORTINT);
VAR P:LISTEPTR;
BEGIN
      CREER_PTR(P);

      DUPLIQUER_PTR(ORIGINAL,P);
      AMPLIFIER(2);
      OPERER(4);  {Yb}
      SWAP;

      OPERER(1);   {Ya}

      SWAP_PTR(ORIGINAL,P);
      SWAP_PTR(PREMIER,P);
      AMPLIFIER(Hb);
      OPERER(2); {Yab}

      SWAP_PTR(ORIGINAL,P);
      OPERER(0);

      DETRUIRE_PTR(P)
END;


{2XabbYb-HbXab}

PROCEDURE oK3(Hb:SHORTINT);
VAR P:LISTEPTR;
BEGIN
      CREER_PTR(P);

      DUPLIQUER_PTR(ORIGINAL,P);
      AMPLIFIER(2);
      OPERER(4);  {Yb}
      SWAP;

      OPERER(7);   {Xabb}

      SWAP_PTR(ORIGINAL,P);
      SWAP_PTR(PREMIER,P);
      AMPLIFIER(-Hb);
      OPERER(6); {Xab}

      SWAP_PTR(ORIGINAL,P);
      OPERER(0);

      DETRUIRE_PTR(P)
END;




PROCEDURE K0;
VAR P:LISTEPTR;
BEGIN
      CREER_PTR(P);

      DUPLIQUER_PTR(ORIGINAL,P);
      AMPLIFIER(-4);
      OPERER(1);
      SWAP;

      OPERER(3);

      SWAP_PTR(ORIGINAL,P);
      SWAP_PTR(PREMIER,P);

      OPERER(2);

      SWAP;
      OPERER(2);

      SWAP_PTR(ORIGINAL,P);
      OPERER(0);

      DETRUIRE_PTR(P)
END;

PROCEDURE K4;
VAR P:LISTEPTR;
BEGIN
      CREER_PTR(P);

      DUPLIQUER_PTR(ORIGINAL,P);
      AMPLIFIER(+4);
      OPERER(7);
      SWAP;

      OPERER(5);

      SWAP_PTR(ORIGINAL,P);
      SWAP_PTR(PREMIER,P);

      AMPLIFIER(-1);
      OPERER(6);

      SWAP;
      OPERER(6);

      SWAP_PTR(ORIGINAL,P);
      OPERER(0);

      DETRUIRE_PTR(P)
END;

{21.11.99}
PROCEDURE Z(H_b:SHORTINT);
VAR P,Q:LISTEPTR;
BEGIN
      CREER_PTR(P);
      CREER_PTR(Q);
      DUPLIQUER_PTR(ORIGINAL,P);
      OPERER(4);
      SWAP;
      DUPLIQUER_PTR(ORIGINAL,Q);

      OPERER(4);
      SWAP;

      OPERER(1);
      SWAP;
      SWAP_PTR(ORIGINAL,Q);
      AMPLIFIER(H_b-1);
      OPERER(2);

      SWAP_PTR(ORIGINAL,P);
      AMPLIFIER((H_b-1)*H_b);
      OPERER(3);

      SWAP_PTR(ORIGINAL,Q);
      OPERER(0);

      DETRUIRE_PTR(Q);
      DETRUIRE_PTR(P)
END;



PROCEDURE OPERER_BIS(O:BYTE;H_b:SHORTINT);
BEGIN


              CASE O OF
              0: K0;
              1: K1(H_b);
              3: K3(H_b);
              4: K4
              END
END;

PROCEDURE COMMUTER_BIS(X,Y:BYTE;H_b:SHORTINT);
VAR P,Q:LISTEPTR;
BEGIN
      CREER_PTR(P);
      CREER_PTR(Q);

      DUPLIQUER_PTR(ORIGINAL,P);
      AMPLIFIER(-1);
      OPERER_BIS(X,H_b);
      SWAP;

      OPERER_BIS(Y,H_b);

      SWAP_PTR(PREMIER,Q);

      SWAP_PTR(ORIGINAL,P);

      OPERER_BIS(Y,H_b);

      SWAP;
      OPERER_BIS(X,H_b);

      SWAP_PTR(ORIGINAL,Q);
      OPERER(0);
       DETRUIRE_PTR(Q);
       DETRUIRE_PTR(P)
END;

PROCEDURE COMMUTER(X,Y:BYTE);
VAR P:LISTEPTR;
BEGIN
      CREER_PTR(P);
      DUPLIQUER_PTR(ORIGINAL,P);
      AMPLIFIER(-1);
      OPERER(X);
      SWAP;

      OPERER(Y);

      SWAP_PTR(ORIGINAL,P);
      SWAP_PTR(PREMIER,P);


      OPERER(Y);

      SWAP;
      OPERER(X);

      SWAP_PTR(ORIGINAL,P);
      OPERER(0);

      DETRUIRE_PTR(P)
END;

          {[[X,Y],Z]}
PROCEDURE COMMUTER_TER(X,Y,Z:BYTE);
VAR P:LISTEPTR;
BEGIN
     CREER_PTR(P);

     DUPLIQUER_PTR(ORIGINAL,P);
     AMPLIFIER(-1);
     COMMUTER(X,Y);
     SWAP;

     OPERER(Z);

     SWAP_PTR(ORIGINAL,P);
     SWAP_PTR(PREMIER,P);

     OPERER(Z);

     SWAP;
     COMMUTER(X,Y);
     SWAP_PTR(ORIGINAL,P);
     OPERER(0);

     DETRUIRE_PTR(P)
END;


PROCEDURE EXECUTER(A,B,C,D:SHORTINT);
VAR  O:BYTE;
     F:BYTE;
BEGIN
      IF premier^.next<>NIL THEN
             BEGIN
                  WRITE(premier^.next^.COEF:8{:2},' ');
                  devoiler(premier^.next^.INFO);
             END;

      WRITELN;
      WRITELN;
      FOR F:=1 TO A+B+C+D DO
      BEGIN
             SWAP;
             WRITELN;
             WRITE('J''EXECUTE ');
             TEXTCOLOR(WHITE);
              IF D>0 THEN BEGIN
                     WRITELN('Y'++'b');
                     O:=4;
                     DEC(D)
              END
                     ELSE IF B>0 THEN BEGIN
                                 WRITELN('Y'+'a'+'+'++'b');
                                 O:=2;
                                 DEC(B)
                           END
                                 ELSE IF C>0 THEN BEGIN
                                              WRITELN('Y'+'a'+'+'++'2'++'b');
                                              O:=3;
                                              DEC(C)
                                 END
                                             ELSE IF A>0 THEN BEGIN
                                                         WRITELN('Y'+'a');
                                                         O:=1;
                                                         DEC(A)
                                             END;

                       OPERER(O);
                       {IF A+B+C+D=0 THEN EXIT;}
                       REDUCTION;
                       LECTURE(premier);
                       WRITELN
      END
END;

PROCEDURE EXEQUTER(A,B,C,D:SHORTINT);
VAR  O:BYTE;
     F:BYTE;
BEGIN
      IF premier^.next<>NIL THEN
             BEGIN
                  WRITE(premier^.next^.COEF:8{:2},' ');
                  devoiler(premier^.next^.INFO);
             END;
      WRITELN;
      WRITELN;
      FOR F:=1 TO A+B+C+D DO
      BEGIN
             SWAP;
             WRITELN;
             WRITE('J''EXECUTE ');
             TEXTCOLOR(WHITE);
              IF A>0 THEN BEGIN
                     WRITELN('YaYc');
                     OPERER(3);
                     SWAP;
                     OPERER(1);
                     DEC(A)
              END
                     ELSE IF B>0 THEN BEGIN
                                 WRITELN('Yb');
                                 OPERER(2);
                                 DEC(B)
                           END
                                 ELSE IF C>0 THEN BEGIN
                                              WRITELN('YaYd');
                                              OPERER(4);
                                              SWAP;
                                              OPERER(1);
                                              DEC(C)
                                 END
                                             ELSE IF D>0 THEN BEGIN
                                                         WRITELN('Yab');
                                                         OPERER(2);
                                                         DEC(D)
                                             END;

                       IF A+B+C+D=0 THEN EXIT;
                       REDUCTION;
                       LECTURE(premier);
                       WRITELN
      END
END;


PROCEDURE EXEqUTER_BIS(G,H,B,U:SHORTINT);
VAR  H_b:SHORTINT;
     F:BYTE;
BEGIN
      IF U+U>DEGRE THEN U:=DEGRE DIV 2;
      H_b:=DEGRE;

      IF premier^.next<>NIL THEN
             BEGIN
                  WRITE(premier^.next^.COEF:8{:2},' ');
                  devoiler(premier^.next^.INFO);
             END;
      WRITELN;
      WRITELN;

      FOR F:=1 TO B+U+G+H DO
      BEGIN
             SWAP;
             WRITELN;
             WRITE('J''EXECUTE ');
             TEXTCOLOR(WHITE);
              IF U>0 THEN BEGIN
                     WRITELN('Z'+'a'+'+'++'2'++'b',' H'++'b','=',H_b);
                     Z(H_b);
                     DEC(U);
                     DEC(H_b,2)
              END
                     ELSE IF B>0 THEN BEGIN
                                 WRITELN('Yb');
                                 OPERER(2);
                                 DEC(B);
                                 INC(H_b,2)
                           END
                                 ELSE IF H>0 THEN BEGIN
                                                         WRITELN('YaYd');
                                                         OPERER(4);
                                                         SWAP;
                                                         OPERER(1);
                                                         DEC(H)
                                             END
                                 ELSE IF G>0 THEN BEGIN
                                              WRITELN('YaYc');
                                              OPERER(3);
                                              SWAP;
                                              OPERER(1);
                                              DEC(G)
                                 END
                                             ;
                       IF B+U+G+H=0 THEN EXIT;
                       REDUCTION;

                       LECTURE(PREMIER);
                       WRITELN
      END;

END;

PROCEDURE EXECUTER_sp1{K3K1K0}(G,H,h2,h3:SHORTINT);
VAR  H_b:SHORTINT;
     F:BYTE;
     
BEGIN
h2:=0;h3:=0;
      H_b:=DEGRE;
	

      IF premier^.next<>NIL THEN
             BEGIN
                  WRITE(premier^.next^.COEF:8,' ');
                  devoiler(premier^.next^.INFO);
             END;
      WRITELN;
      WRITELN;

      FOR F:=1 TO G+H+1 DO
      BEGIN
             SWAP;
             WRITELN;
             WRITE('J''EXECUTE ');
             TEXTCOLOR(WHITE);
              IF G>0 THEN BEGIN
                                              WRITELN('K_0');
                                                         K0;
                                                       DEC(G);
                                                   reduction;

                                 END
                                             ELSE IF H>0 THEN BEGIN
                                                         WRITELN('K_1');
                                                         K1(H_b);
                                                         DEC(H)
                                             END

               ELSE  BEGIN
                     WRITELN('K_3');
                     K3(H_b);
                                         
                 END;

                       REDUCTION;
                       LECTURE(PREMIER);
                       WRITELN;
	           
      END;
	                { filtrer67;reduction;
                       LECTURE(PREMIER);
                       WRITELN;
				}	
END;


PROCEDURE EXECUTERSP1(G,H,U,A:SHORTINT);
VAR  H_b:SHORTINT;
     F:BYTE;
BEGIN

      IF U+U>DEGRE THEN U:=DEGRE DIV 2;
      H_b:=DEGRE;



      IF premier^.next<>NIL THEN
             BEGIN
                  WRITE(premier^.next^.COEF:8{:2},' ');
                  devoiler(premier^.next^.INFO);
             END;
      WRITELN;
      WRITELN;

      FOR F:=1 TO A+U+G+H DO
      BEGIN
             SWAP;
             WRITELN;
             WRITE('J''EXECUTE ');
             TEXTCOLOR(WHITE);
              IF U>0 THEN BEGIN
                     WRITELN('Z'+'a'+'+'+'2'+'b',' H'+'b','=',H_b);
                     Z(H_b);
                     DEC(U);
                     DEC(H_b,2)
              END
                     ELSE IF A>0 THEN BEGIN
                                 WRITELN('Y'+'a');
                                 OPERER(1);
                                 DEC(A);
                                 INC(H_b,2)
                           END
                                 ELSE IF G>0 THEN BEGIN
                                              WRITELN('K_0');
                                                         K0;
                                                         DEC(G)

                                 END
                                             ELSE IF H>0 THEN BEGIN
                                                         WRITELN('K_1');
                                                         K1(H_b);
                                                         DEC(H)
                                             END;

                       REDUCTION;
                       LECTURE(PREMIER);
                       WRITELN
      END;

END;




PROCEDURE EXECUTER_TER(G,H,U,A:SHORTINT);
VAR  H_b:SHORTINT;
     F:BYTE;
BEGIN
      IF U+U>DEGRE THEN U:=DEGRE DIV 2;
      IF U>0 THEN H_b:=DEGRE;


      IF premier^.next<>NIL THEN
             BEGIN
                  WRITE(premier^.next^.COEF:8{:2},' ');
                  devoiler(premier^.next^.INFO);
             END;
      WRITELN;
      WRITELN;

      FOR F:=1 TO A+U+G+H DO
      BEGIN
             SWAP;
             WRITELN;
             WRITE('J''EXECUTE ');
             TEXTCOLOR(WHITE);
              IF U>0 THEN BEGIN
                     WRITELN('Z'+'a'+'+'++'2'++'b',' H'++'b','=',H_b);
                     Z(H_b);
                     DEC(U);
                     DEC(H_b,2)
              END
                     ELSE IF A>0 THEN BEGIN
                                 WRITELN('Y'+'a');
                                 OPERER(1);
                                 DEC(A)
                           END
                                 ELSE IF G>0 THEN BEGIN
                                              WRITELN('K_4');
                                              K4;
                                              DEC(G)
                                 END
                                             ELSE IF H>0 THEN BEGIN
                                                         WRITELN('K_3');
                                                         K3(H_b);
                                                         DEC(H)
                                             END;
                       REDUCTION;
                       LECTURE(PREMIER);
                       WRITELN
      END;

END;

PROCEDURE APRES_SAISIE;
BEGIN
             IF SPN THEN
             EXECUTER(A[1],A[2],A[3],A[4])
             {ELSE EXECUTER_TER(A[1],A[2],A[3],A[4]);}
             ELSE EXECUTERSP1(A[1],A[2],A[3],A[4]);

             {EXECUTER_K3K1(A[1]);}
             WRITELN;
             WRITE('C''EST LE RESULTAT');
             Wait
END;

PROCEDURE ROUTINE;
BEGIN
      REPEAT
             ZERO_PTR;
             WRITELN;
             INSERER(FI,1);

             IF SAISIE THEN APRES_SAISIE
      UNTIL UPCASE(READKEY)='Q'
END;

PROCEDURE ALIMENTATION;
VAR D:SHORTINT;
BEGIN             exit;
     DEGRE:=2;
     DIMENSION:=6;
     SPN:=TRUE;
     D:=1;
     REPEAT
             ZERO_PTR;
             WRITELN;
             INSERER(FI,1);
           CASE D OF
           1:BEGIN A[1]:=0;A[2]:=4;A[3]:=1;A[4]:=0 END;
           2:BEGIN A[1]:=1;A[2]:=2;A[3]:=2;A[4]:=0 END;
           3:BEGIN A[1]:=2;A[2]:=0;A[3]:=3;A[4]:=0 END;
           4:BEGIN A[1]:=1;A[2]:=3;A[3]:=1;A[4]:=1 END;
           5:BEGIN A[1]:=2;A[2]:=1;A[3]:=2;A[4]:=1 END;
           6:BEGIN A[1]:=1;A[2]:=4;A[3]:=0;A[4]:=2 END;
           7:BEGIN A[1]:=2;A[2]:=2;A[3]:=1;A[4]:=2 END;
           8:BEGIN A[1]:=3;A[2]:=0;A[3]:=2;A[4]:=2 END;
           9:BEGIN A[1]:=0;A[2]:=5;A[3]:=0;A[4]:=1;write(#7) END
           END;
           AFFICHER;
           REPEAT UNTIL UPCASE(READKEY)=' ';
           VIDERCLE;
           APRES_SAISIE;

           FILTRER;     (*fonctionne aussi avec _BIS*)
           WRITELN;
           WRITELN;
           WRITE('':11);
           DEVOILER(PREMIER^.NEXT^.INFO);
           WAIT;
           INC(D)
     UNTIL D=10;
     SPN:=FALSE;
     REPEAT UNTIL UPCASE(READKEY)='A';
     VIDERCLE;
     ROUTINE
END;

PROCEDURE  SENVA ;
BEGIN
     GOTOXY ( 1 , 20 );
     WRITELN ( Time - Timeo :6:2) ;
     GOTOXY ( 1 , 25 );
     TEXTCOLOR ( 11 ) ;
     WRITE ( 'Toucher ENTER pour continuer ...' );
     CLREOL;
     REPEAT
     UNTIL READKEY = #13
END;


BEGIN
           ClrScr;
           INITIER_PTR;
           INSERER(FI,1);
           LECTURE(PREMIER);

            ALIMENTATION;
            ROUTINE
END.
END.
