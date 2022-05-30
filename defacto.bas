'                                                                              
'                        Defacto 2 Intro By Widow Maker                          
'                                                                              
'                                                                              
'-------------------------------------------------------------------------------
' Screen Size;
'-------------------------------------------------------------------------------
    CONST XRES  =   640
    CONST YRES  =   480
    
    option static
    option explicit
'-------------------------------------------------------------------------------
' Includes and pre-processor defs;
'-------------------------------------------------------------------------------

'    #DEFINE PTC_WIN

    ' Large font;
    #INCLUDE "a256nickpal.bas"
    #INCLUDE "a256nickraw.bas"
    ' Logo;
    #INCLUDE "deflogopal.bas"
    #INCLUDE "deflogoraw.bas"

    #INCLUDE "TINYPTC_ext.BI"
    #define alphaa(s,d) ((((s and &hff00ff)*(s shr 24)+(d and &hff00ff)*(256-(s shr 24)))and &hff00ff00)or(((s and &hff00)*(s shr 24)+(d and &hff00)*(256-(s shr 24)))and &hff0000))shr 8
    #include "windows.bi"

    #include "ufmod.bi"
    #include "craxxor.bas"
    Dim hWave As HWAVEOUT   

'-------------------------------------------------------------------------------
' INITIALISE LOGO!!
'-------------------------------------------------------------------------------
    '--------------
    '--Image size--
    '--------------
    
    Const imgX = 520
    Const imgY = 86
    

    Declare Sub DrawImage(byval imxpos as integer,byval imypos as integer)
    Declare Sub LoadDataImage()    
    'Picture buffer
    Dim Shared img_buffer( imgx * imgy ) as integer    
    'RGB color palette buffer
    Dim Shared img_r(256), img_g(256), img_b(256) as short    
    LoadDataImage()



'-------------------------------------------------------------------------------
' INITIALISE LARGE FONT!!
'-------------------------------------------------------------------------------
    '--------------
    '--Image size--
    '--------------
    
    Const LfimgX = 1800
    Const LfimgY = 31
    

    Declare Sub LfDrawImage (byval imxpos as integer,byval imypos as integer,byval SX as integer,byval SY as integer)
    Declare Sub LfDrawImage2(byval imxpos as integer,byval imypos as integer,byval SX as integer,byval SY as integer)
    Declare Sub LFLoadDataImage()    
    'Picture buffer
    Dim Shared LFimg_buffer( lfimgx * lfimgy ) as integer    
    'RGB color palette buffer
    Dim Shared LFimg_r(256), LFimg_g(256), LFimg_b(256) as short    
    LFLoadDataImage()

'-------------------------------------------------------------------------------    
' General Purpose Variables;
'-------------------------------------------------------------------------------
    
    DIM SHARED AS UINTEGER HALFX , HALFY
    DIM SHARED AS UINTEGER MARGIN = 1
    
    
    HALFX= XRES/2 : HALFY=YRES/2
    
    DIM SHARED AS UINTEGER COPCYC = 0
    DIM SHARED AS UINTEGER BUBBLES = 700
    dim shared as UINTEGER SINEWAVE(XRES)
'-------------------------------------------------------------------------------    
' Arrays;
'-------------------------------------------------------------------------------
    DIM SHARED AS UINTEGER COPPALIST ( 5000 )
    DIM SHARED AS UINTEGER COPPALIST2( 5000 )    
    DIM SHARED AS UINTEGER COPPALIST3( 5000 )
    DIM SHARED AS UINTEGER COPPALIST4( 5000 )    
    
    DIM SHARED AS UINTEGER BUFFER ( XRES * YRES )
    dim shared as UINTEGER INTERLACE =0
    DIM SHARED AS DOUBLE   BBZ  ( BUBBLES ):'
    DIM SHARED AS DOUBLE   BBX  ( BUBBLES ):'
    DIM SHARED AS DOUBLE   BBY  ( BUBBLES ):'
    DIM SHARED AS integer  BBT  ( BUBBLES ):'
    DIM SHARED AS DOUBLE   MOFFY( BUBBLES ):'
    DIM SHARED AS DOUBLE   MOFFZ( BUBBLES ):'
    DIM SHARED AS DOUBLE   MOFFX( BUBBLES ):'
    dim shared as integer pause = 0
'-------------------------------------------------------------------------------    
' Sub Defs;
'-------------------------------------------------------------------------------

    DECLARE SUB SET_BUBBLES()
    DECLARE SUB DO_BUBBLES ()
    
    declare SUB DRAWBOB (BYVAL BX AS INTEGER , BYVAL BY AS INTEGER,  BYVAL CL AS INTEGER)
    declare SUB DRAWBOB2(BYVAL BX AS INTEGER , BYVAL BY AS INTEGER,  BYVAL CL AS INTEGER)
    DECLARE SUB BORDER ()
    DECLARE SUB LARGETEXT (BYVAL LTX AS INTEGER , BYVAL LTY AS INTEGER ,BYVAL LTS AS STRING)
    DECLARE SUB LARGETEXT2(BYVAL LTX AS INTEGER , BYVAL LTY AS INTEGER ,BYVAL LTS AS STRING)
    DECLARE SUB SCROLLERR()
    DECLARE SUB INFOS()
    DECLARE SUB DRUGGIES()
'-------------------------------------------------------------------------------    
' Open Screen, Start music Initialise;
'-------------------------------------------------------------------------------
    ptc_allowclose(0)
    ptc_setdialog(0,"",0,1)
    
    IF (PTC_OPEN("WWW.DEFACTO2.NET",XRES,YRES)=0) THEN
    END-1
    END IF  
hWave = uFMOD_PlaySong(@craxxor.xm(0),19334,XM_MEMORY)

    SET_BUBBLES()
    
    
        dim shared BOB (144) AS INTEGER
        DIM LP AS INTEGER
        
        FOR LP=1 TO (144)
            READ BOB(LP)
            'BOB(LP)=BOB(LP)
        NEXT         
        
        dim shared BOB2 (36) AS INTEGER
        FOR LP=1 TO (36)
            READ BOB2(LP)
        NEXT   
        DIM SHARED EDLOGO (65*5) AS UINTEGER
        FOR LP=1 TO (65*5)
            READ EDLOGO(LP)
        NEXT   
        DIM SHARED GADD AS DOUBLE
        DIM SHARED GADD2 AS DOUBLE
        DIM SHARED GADD3 AS DOUBLE
        DIM SHARED GADD4 AS DOUBLE
'-------------------------------------------------------------------------------    
' Main Gubbins;
'-------------------------------------------------------------------------------
DIM D

DIM SHARED SCROLLTEXT AS STRING
dim shared title(8) as string

SCROLLTEXT="                     "

#include "nfoz.txt"




dim shared mp as integer
dim shared SOFF as integer
MP=1
SOFF=0

DIM SHARED AS INTEGER I
DIM SHARED AS INTEGER SS
I=3

while(GetAsyncKeyState(VK_ESCAPE)<>-32767)    
    COPCYC=COPCYC+2
    IF COPCYC>1440 THEN COPCYC=COPCYC-1440
    FOR D=0 TO XRES
        SINEWAVE(D) = 80+(49*SIN((D/93)+GADD2))
        'SINEWAVE(D) = 80+(19*SIN((D/63)+GADD2))+(10*SIN((D/74)+GADD3))-(20*Sin((D/34)+GADD))
    NEXT
    GADD=GADD+.1
    GADD2=GADD2-.05
    GADD3=GADD3+.02
    GADD4=GADD4-6.02

    DO_BUBBLES()
    INFOS()
    SCROLLERR()
    DRUGGIES()
    drawimage(60,35)
    
    PTC_UPDATE@BUFFER(0)
   ' ERASE BUFFER
    BORDER()
    INTERLACE=INTERLACE+1
    IF INTERLACE>1 THEN INTERLACE=0
WEND

    uFMOD_StopSong()
   PTC_CLOSE()


   SLEEP 1
   
END


Sub LoadDataImage()
    dim i as integer
    'Loads Color palette
    for i = 0 to 255
         img_r( i ) = deflogo.bmp.pal (i*3)'Red color
         img_g( i ) = deflogo.bmp.pal (i*3+1)'Green color
         img_b( i ) = deflogo.bmp.pal (i*3+2)'Blue color
         
         img_r( i ) =(img_r(i) Shl 16) Or (img_g(i) Shl 8 )  Or img_b(i)
         
    Next    
    
    for i = 1 to (imgx*imgy) - 1
         img_buffer(i) = deflogo.bmp.raw (i)
    next  
        
End Sub

Sub DrawImage(byval xpos as integer,byval ypos as integer)
    dim as uinteger x,y,pixel,mong,intx,inty,bast
    bast=rgb(4,2,4)
    for Y = 0 to imgy-1
        for X = 1 to imgx-1
            pixel = img_buffer(x+(y*imgx))
            'mong = (img_r(pixel) Shl 16) Or (img_g(pixel) Shl 8 )  Or img_b(pixel)
            mong = (img_r(pixel) )
            if mong <> bast then 
                intx = (x)+xpos
                inty = (y)+ypos
                Buffer( intX  +(intY * XRES  )) = mong
                
            end if
        next
    next
    
End Sub


SUB DRUGGIES()
    DIM AS INTEGER X,Y,P,XXP,YYP,CUNT
    Y=0
    X=0
    XXP=(XRES-65)/2
    YYP=YRES-6
    FOR P=1 TO 65*5
                CUNT=RGB(50+49*SIN(((X+(15*Y)+GADD4)/93)),50+49*SIN((X+(15*Y)+40+GADD4)/93),250)
        IF EDLOGO(P) = 1 THEN BUFFER (X+XXP+((Y+YYP)*XRES)) = CUNT
        X=X+1

        IF X>=65 THEN 
            X=0
            Y=Y+1
        END IF
    NEXT
    
END SUB

SUB INFOS()
    DIM B,CV
    
    for B=1 to 8
    
    CV= (XRES- (LEN(TITLE(B)) * 31)) / 2
    LARGETEXT(CV,160+B*17,title(B))
    next

END SUB

SUB SCROLLERR()
    LARGETEXT2(0-SOFF,YRES-180,MID(SCROLLTEXT,MP,22))
    if pause>0 then pause=pause-1
    if pause<=0 then SOFF=SOFF+3
if soff>=16 and soff<20 and pause=0 then
        
        if MID(Scrolltext,MP,1)="a" then 
            pause=200
            SOFF=18
        end if
        if MID(Scrolltext,MP,1)="b" then 
            pause=300
            SOFF=18
        end if

if MID(Scrolltext,MP,1)="c" then 
    pause=400
            SOFF=18
        end if

if MID(Scrolltext,MP,1)="d" then 
    pause=400
            SOFF=18
        end if

end if
    IF SOFF>=31 THEN
        
        SOFF=SOFF-31
        MP=MP+1
        
        IF MP>LEN(SCROLLTEXT) THEN MP=1
    END IF
END SUB
'-------------------------------------------------------------------------------
' LARGE FONT;
'-------------------------------------------------------------------------------

SUB LARGETEXT(BYVAL LTX AS INTEGER , BYVAL LTY AS INTEGER ,BYVAL LTS AS STRING)
    
    DIM AS INTEGER A,MMM,NNN
'    lts=UCASE(LTS)
    FOR A=1 TO LEN(LTS)
    NNN=(ASC(MID(LTS,A,1))-33)
    
    
    IF NNN>63 THEN NNN=-1
    if nnn=0 then nnn=1
    MMM=NNN*31

    if nnn>0 then LFDRAWIMAGE( LTX,LTY, MMM , 0 )
    
    LTX=LTX+31

    NEXT

END SUB


SUB LARGETEXT2(BYVAL LTX AS INTEGER , BYVAL LTY AS INTEGER ,BYVAL LTS AS STRING)
    
    DIM AS INTEGER A,MMM,NNN
'    lts=UCASE(LTS)
    FOR A=1 TO LEN(LTS)
    NNN=(ASC(MID(LTS,A,1))-33)
    
    
    IF NNN>63 THEN NNN=-1
    if nnn=0 then nnn=1
    MMM=NNN*31

    if nnn>0 then LFDRAWIMAGE2( LTX,LTY, MMM , 0 )
    
    LTX=LTX+31

    NEXT

END SUB


  Sub LFLoadDataImage()
    dim i as integer
    'Loads Color palette
    for i = 0 to 255
         LFimg_r( i ) = a256nick.bmp.pal (i*3)'Red color
         LFimg_g( i ) = a256nick.bmp.pal (i*3+1)'Green color
         LFimg_b( i ) = a256nick.bmp.pal (i*3+2)'Blue color
         
         LFimg_r( i ) =(LFimg_r(i) Shl 16) Or (LFimg_g(i) Shl 8 )  Or LFimg_b(i)
         
    Next    
    
    for i = 1 to (LFimgx*LFimgy) - 1
         LFimg_buffer(i) = a256nick.bmp.raw (i)
    next  
        
End Sub


'
' THIS ONE!!!!!!!!!!!!!!!!!! \/
'
'

Sub LFDrawImage2(byval xpos as integer,byval ypos as integer,byval SX as integer,byval SY as integer)
    dim as integer x,y,pixel,mong,intx,inty,xxx,yyy,corn,LAMER,MV,corns
    LAMER = RGB(240,240,240)
    MV=0
    xxx=xpos
    yyy=0
    
    for Y =0 to 30 
    MV=0        
        for X = SX+1 to SX+31
            
            pixel = LFimg_buffer(x+(y*lfimgx))            
            mong = (LFimg_r(pixel) )            
                
                
                corn=SINEWAVE(XPOS+MV)
                inty = yyy+ypos+corn
                corns=7*sin((inty-(gadd3*85))/17)
                intx = (xxx+corns)
                
                if intX > 8  AND intX<XRES-8 AND MONG<>&H000000 then 
                    Buffer( intX  +(intY * XRES  )) = COPPALIST(INTY+COPCYC)                
                    Buffer( intX  +((intY+2) * XRES  )) = COPPALIST2(INTY+COPCYC)
                
                END IF
            
            xxx=xxx+1
            mv=mv+1
        next
        
            yyy=yyy+1
            xxx=xpos
    next
    
End Sub


Sub LFDrawImage(byval xpos as integer,byval ypos as integer,byval SX as integer,byval SY as integer)
    dim as integer x,y,pixel,mong,intx,inty,xxx,yyy,LAMER,MV
    LAMER = RGB(240,240,240)
    MV=0
    xxx=xpos
    yyy=0
    
    for Y = INTERLACE to 30 step 2
    MV=0        
        for X = SX+1 to SX+31 
            
            pixel = LFimg_buffer(x+(y*lfimgx))            
            mong = (LFimg_r(pixel) )            
            
                intx = xxx
                
                inty = yyy+ypos
                
                if intX > 0  AND intX<XRES AND MONG<>&H000000 then 
                    Buffer( intX  +(intY * XRES  )) = COPPALIST3(INTY+COPCYC)
                
                Buffer( intX  +((intY+2) * XRES  )) = COPPALIST4(INTY+COPCYC)
                'Buffer( intX  +(intY * XRES  )) = COPPALIST(INTY+COPCYC)
                
                
            END IF
            
            xxx=xxx+1
            mv=mv+1
        next
        
            yyy=yyy+1
            xxx=xpos
    next
    
End Sub


SUB BORDER()
    
    DIM LENG AS INTEGER
    DIM AS INTEGER Y,TC,Y2
    DIM PP AS UINTEGER PTR
    
    LENG = xres
    
    
    
    FOR Y=0 TO YRES-1
    PP = @BUFFER(Y*XRES)     
    TC=RGBA(INT(Y/15),INT(Y/12),INT(Y/8),10)
    asm
        mov eax,dword ptr[TC]
        mov ecx, [LENG]
        mov edi, [PP]
        rep stosd
    end asm   
    NEXT

  

END SUB

SUB DO_BUBBLES()
    
    DIM AS INTEGER A,TX,TY,CV
    
    FOR A=1 TO BUBBLES
   
        BBY(A)=BBY(A)-(2+(1.2*SIN(MOFFY(A))))
        BBX(A)=BBX(A)+.9*SIN(MOFFX(A)+A)
        
        MOFFZ(A)=MOFFZ(A)+.007        
        MOFFY(A)=MOFFY(A)+.0077
        BBZ(A)=11.5+10*sin(MOFFZ(A))
        MOFFX(A)=MOFFX(A)+.015
        IF BBY(A) < -300 THEN BBY(A)= - BBY(A)
   
        TX = (BBX(A) / (BBZ(A)/15)) + HALFX
        TY = (BBY(A) / (BBZ(A)/18)) + HALFY
        
        IF TX>-10 AND TX<XRES AND TY>-10 AND TY<YRES AND BBZ(A) >0 THEN
            
            CV = (( -(BBZ(A)*4) ) + 90 ) 
            
                IF BBT(A)=1 THEN DRAWBOB2(TX,TY,CV)
                IF BBT(A)=0 THEN DRAWBOB (TX,TY,CV)
        END IF
        
    NEXT
    
END SUB

SUB SET_BUBBLES()
    
    DIM AS INTEGER A,B,C
    
    
    
    FOR A=1 TO BUBBLES
        BBX(A) =  (RND(1)*300)-150
        BBY(A) =  (RND(1)*600)-300
        BBZ(A) =  (RND(1)*6)+5
        MOFFY(A) =(RND(1)*6000)
        MOFFZ(A) =(RND(1)*6000)
        MOFFX(A) =(RND(1)*6000)
        if rnd(1) <.7 then 
            BBT(A)=1
        ELSE
            BBT(A)=0
        END IF
            
    NEXT
    B=100
    C=200
    FOR A=0 TO 5000
        
        COPPALIST(A) = RGB(150+50*SIN((A+100)*3.14/180),150+50*SIN((A+50)*3.14/180),150+50*SIN(A*3.14/180))                
        COPPALIST2(A) = RGB(75+50*SIN((A+100)*3.14/180),75+50*SIN((A+50)*3.14/180),75+50*SIN(A*3.14/180))        
        
        COPPALIST3(A) = RGB(150+50*SIN((A+100)*3.14/360),150+50*SIN((A+50)*3.14/180),150+50*SIN(A*3.14/180))                
        COPPALIST4(A) = RGB(75+50*SIN((A+100)*3.14/360),75+50*SIN((A+50)*3.14/180),75+50*SIN(A*3.14/180))        
        
        
        B=B+3
        C=C+2
    NEXT
    
END SUB


SUB DRAWBOB (BYVAL BX AS INTEGER , BYVAL BY AS INTEGER,  BYVAL CL AS INTEGER)
    
    DIM BLX , BLY ,MM,TC,ZZ AS INTEGER
    
    '-----------------------
    'Set Offset In Bob Bank;
    '-----------------------
    FOR BLY=0 TO 11
    FOR BLX=1 TO 12
        '---------
        'CLIPPING;
        '---------
        IF (BX+BLX>MARGIN) AND (BX+BLX<XRES-MARGIN) AND (BY+BLY>0) AND (BY+BLY<YRES) THEN
            '-------------
            'COLOUR VALUE;
            '-------------
            
                MM= (BOB(((BLY*12)+BLX)))*CL
            IF MM>0 THEN 
            IF MM>220 THEN MM=220
            '------------------
            'DRAW PIXEL OF BOB;
            '------------------
            TC=RGBA(MM+30,MM+10,MM+25,120)
            ZZ=((BY+BLY)*XRES)+BX+BLX

            BUFFER (ZZ) = ALPHAA (TC,BUFFER (ZZ) )
            END IF
        END IF
    NEXT
    NEXT

END SUB

SUB DRAWBOB2 (BYVAL BX AS INTEGER , BYVAL BY AS INTEGER,  BYVAL CL AS INTEGER)
    
    DIM BLX , BLY ,MM,TC,ZZ AS INTEGER
    
    '-----------------------
    'Set Offset In Bob Bank;
    '-----------------------
    FOR BLY=0 TO 5
    FOR BLX=1 TO 6
        '---------
        'CLIPPING;
        '---------
        IF (BX+BLX>MARGIN) AND (BX+BLX<XRES-MARGIN) AND (BY+BLY>0) AND (BY+BLY<YRES) THEN
            '-------------
            'COLOUR VALUE;
            '-------------
            
                MM= (BOB2(((BLY*6)+BLX)))*CL
            IF MM>0 THEN 
            IF MM>220 THEN MM=220
            '------------------
            'DRAW PIXEL OF BOB;
            '------------------
            TC=RGBA(MM+30,MM+10,MM+25,120)
            ZZ=((BY+BLY)*XRES)+BX+BLX

            BUFFER (ZZ) = ALPHAA (TC,BUFFER (ZZ) )
            END IF
        END IF
    NEXT
    NEXT

END SUB





DATA 0,0,0,1,2,2,3,3,1,0,0,0
DATA 0,1,2,2,0,0,0,2,4,4,1,0
DATA 0,2,1,0,0,0,0,2,2,5,4,0
DATA 1,2,0,0,0,0,2,9,9,2,4,1
DATA 3,1,0,1,0,0,2,9,9,2,2,3
DATA 3,2,0,3,0,0,0,2,2,0,0,3
DATA 5,2,0,3,2,1,0,0,0,0,0,2
DATA 5,4,1,2,6,2,0,0,0,0,0,2
DATA 3,8,4,0,2,3,3,1,0,0,2,1
DATA 0,4,7,4,1,0,0,0,0,1,2,0
DATA 0,2,4,8,4,4,2,2,2,2,1,0
DATA 0,0,0,3,5,5,3,3,1,0,0,0





data 0,1,1,1,1,0
data 1,1,0,6,6,1
data 2,2,1,9,6,1
data 2,5,2,1,0,1
data 1,8,5,2,1,1
data 0,1,2,2,1,0


DATA 1,1,1,0,1,0,0,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,0,0,1,1,0,0,1,1,1,0,1,0,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1
DATA 1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,0,1,0,0,0,1,0,0
DATA 1,1,0,0,1,0,0,0,1,1,0,0,1,0,0,0,0,1,0,0,1,1,0,0,0,1,0,0,1,0,0,0,0,0,1,0,1,0,1,1,0,0,1,0,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,1,0,0,1,1,1
DATA 1,0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0,0,0,1,0,0,0,1,0,0,1,0,0,1,0,0,0,0,0,1
DATA 1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,0,1,0,0,1,0,1,0,1,1,1,0,1,1,1,0,0,0,1,1,0,0,1,0,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1
