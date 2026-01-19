!***********************************************************************************
!* PPPLIB fortran plotting library                                                 *
!*    - fortran90 version (in progress)                                            *
!*    - postscript and/or scalable vector graphics output                          *
!*    - 3D surface and contour plots, including a lighting model                   *
!***********************************************************************************
module ppp_parameters

  real    :: ppp_line_color(3), ppp_text_color(3), ppp_line_width, ppp_transparency
  real    :: ppp_azimuth,       ppp_elevation,     ppp_roll   
  real    :: focal_point(3), light(3), camera(3), camera_distance
  real    :: ambient_fraction, specular_power, specular_fraction, diffusive_fraction
  real    :: ppp_current_xp(2), ppp_current_x(3)
  real    :: ppp_Z_start, ppp_X_scale, ppp_Y_scale, ppp_XY_scale, ppp_Z_scale
  integer :: igr
  character*30 :: ppp_finish
  character*10 font

  real    :: some_colors(3,10)

  data some_colors / 0.0, 0.0, 0.0,  1.0, 0.0, 0.0,  0.0, 1.0, 0.0,  0.0, 0.0, 1.0, &  
                     1.0, 1.0, 0.0,  0.0, 1.0, 1.0,  1.0, 0.0, 1.0,  1.0, 0.6, 0.0, &
                     0.6, 0.0, 1.0,  0.0, 1.0, 0.6 /

end module

module ppp_type_definitions

  type point
    real        :: x(3), normal(3), color(3), transparency
  endtype point
  
  type patch      
    integer :: nv                          ! the number of vertices
    integer, dimension(4) :: vertex
    character*7  :: point_label
    integer      :: nchar
    integer      :: char_size
  endtype

end module  ppp_type_definitions

module ppp_data
  use ppp_type_definitions
 
  type(patch), allocatable :: ppp_patches(:)
  type(point), allocatable :: ppp_points(:)
  integer :: ppp_npatches, ppp_npoints

end module ppp_data

module ppp_transformations  ! change to 4x4 transformations?

  real R_camera(3,3)

end module



SUBROUTINE PPP  
!***********************************************************************
!     DUMMY HEADING FOR THE FORTRAN SOURCE OF LIBRARY PPPLIB.  "PPP"   *
! STANDS FOR "PLASMA PHYSICS PLOTTING (PACKAGE)".  IT IS DERIVED FROM  *
! THE SIMILAR PLOTTING PACKAGE P4, DEVELOPED AT LOS ALAMOS SCIENTIFIC  *
! LABORATORY BY CLAIR NIELSON, BRENDAN GODFREY, DENNIS HEWETT, DEBORAH *
! HYMAN, AND ROBERT MALONE, STARTING FROM BASIC LASL PLOTTING ROUTINES *
! WHICH WERE WRITTEN BY R.M. FRANK, GENE WILLBANKS, AND OTHERS (WHOSE  *
! NAMES WE WERE NOT ABLE TO TRACE).  THE PACKAGE HAS BEEN OBTAINED IN  *
! AUG '83 FROM DENNIS HEWETT, WHO DOES NOT WANT TO BE HELD RESPONSIBLE *
! THOUGH FOR PROBLEMS ARISING WITH ITS USE.                            *
!                                                                      *
!     PPPLIB HAS BEEN ADAPTED IN '84 AND '85 FOR USE ON THE CYBERS 750 *
! AND 205 OF SARA AT AMSTERDAM BY HANS GOEDBLOED AND DICK HOGEWEIJ.    *
! THE ORIGINAL TEXT REFERING TO THE MFECC CRAY1 COMPUTERS AT LIVERMORE *
! HAS BEEN CONSERVED BY ENCLOSING IT BETWEEN THE REVISE DIRECTIVES     *
! "*IF MFE", OR "*ELSEIF MFE", AND "*ENDIF".                           *
!                                                                      *
!     THE CALLS OF THE LOWEST LEVEL GRAPHICS SUPPORT ROUTINES, WHICH   *
! ARE NECESSARILY SYSTEM DEPENDENT, APPEAR AT THE END OF THE PACKAGE.  *
! FOR USE ON THE MFECC CRAY1 COMPUTERS THE PRIMITIVE PLOTTING ROUTINES *
! ARE TAKEN FROM THE LIBRARY TV80LIB.  FOR USE ON THE SARA COMPUTERS   *
! THE PRIMITIVE PLOTTING ROUTINES ARE TAKEN FROM THE CALCOMP LIBRARY.  *
!                                                                      *
!     MODIFICATION BY HANS GOEDBLOED 31/10/85: TRANSITION TO STANDARD  *
!     FORTRAN 77, REPLACING ALL OPERATIONS INVOLVING HOLLERITHS,       *
!     WORD LENGTH, AND OCTAL REPRESENTATIONS BY MACHINE-INDEPENDENT    *
!     MANIPULATIONS AND DECIMAL INTEGER REPRESENTATIONS.               *
!                                                                      *
!     GUIDO HUYSMANS, STEFAAN POEDTS, AND HANS GOEDBLOED 1/09/91:      *
!     ADAPTATION TO THE IBM 3090 COMPUTERS AT SARA AND KUL.            *
!                                                                      *
!     HANS GOEDBLOED, GUIDO HUYSMANS, AND EGBERT WESTERHOF 11/11/91:   *
!     SEPARATE BRANCH CREATING LASERWRITER POSTSCRIPT FILES.           *
!                                                                      *
!     GUIDO HUYSMANS 27/7/99                                           *
!     CONVERTED PLOT COORDINATES TO REAL TO IMPROVE RESOLUTION         *
!     ADDED NEW ROUTINE CPLOTM (DERIVED FROM CPLOT) FOR CONTOUR PLOTS  *
!     ON IRREGULAR BUT ORDERED GRIDS.                                  *
!                                                                      *
!     Guido Huysmans 2/6/2006                                          *
!     f90'ish version, including SVG output                            *
!                                                                      *
!     Guido Huysmans 21/8/2008                                         *
!     updated svg output (multipage, svg text still incomplete)        *
!     added 3D plots : surface plots (tplot) and 3D contour            *
!***********************************************************************
   WRITE(*,10)
10 FORMAT(/1X,'LIBRARY PPPLIB'/1X,'VERSION 20, D.D. 21/08/2008')

RETURN     
END        
       
SUBROUTINE LPLOT6(MX,MY,X,Y,NPTS,TITLE) 
!***********************************************************************
!     LPLOT6 PLOTS THE VALUES IN THE ARRAYS X AND Y AND CONNECTS THEM  *
! WITH A  LINE.  IT DRAWS A BOX AROUND THE PLOT WITH LINEAR-LINEAR     *
! SCALING AND PLACES LABELS ON LEFT AND BOTTOM AXES.  NPTS IS THE      *
! DIMENSION OF THE PLOTTED ARRAYS.  TITLE CONTAINS A TITLE OF AN ARBI- *
! TRARY NUMBER OF CHARACTERS TO BE DRAWN AT THE TOP OF THE GRAPH.  THE *
! POSITION OF THE PLOT ON THE PAGE IS DETERMINED BY MX,MY (SEE LPLOT). *
!                                                                      *
!     WRITTEN BY DEBBY HYMAN, 7-79                                     *
!     REMOVED NTITLE FROM ARGUMENT LIST, HGO 4/12/85.                  *
!***********************************************************************
DIMENSION X(*),Y(*)

CHARACTER*(*) TITLE 
 
  CALL LPLOT(MX,MY,1,X,Y,NPTS,1,TITLE,LEN(TITLE),'X',1,'Y',1)      

RETURN  
END        

SUBROUTINE HPLOT6(MX,MY,X,Y,NPTS,TITLE) 
!***********************************************************************
!     HPLOT6 DRAWS A HISTOGRAM OF THE VALUES IN THE ARRAYS X AND Y     *
! WHICH BOTH CONTAIN NPTS POINTS.  TITLE CONTAINS A TITLE OF AN ARBI-  *
! TRARY NUMBER OF CHARACTERS TO BE DRAWN AT THE TOP OF THE GRAPH.  THE *
! POSITION OF THE PLOT ON THE PAGE IS DETERMINED BY MX,MY (SEE LPLOT). *
!                                                                      *
!     WRITTEN BY DEBBY HYMAN, 7-79                                     *
!     REMOVED NTITLE FROM ARGUMENT LIST, HGO 4/12/85.                  *
!***********************************************************************
DIMENSION X(*),Y(*)   
CHARACTER*(*) TITLE 
       
  CALL HPLOT(MX,MY,1,X,Y,NPTS,1,TITLE,LEN(TITLE),'X',1,'Y',1)      

RETURN     
END        
       
     
SUBROUTINE CPLOT8(MX,MY,X,Y,NX,NY,Z,TITLE)       
!***********************************************************************
!     A CONTOUR PLOT DISPLAYS THE SHAPE OF A SURFACE Z = F(X,Y) BY     *
! TRACING OUT LINES THAT CONNECT POINTS OF EQUAL VALUE ON THE SURFACE. *
! THERE ARE TWO CONTOUR PLOTTING SUBROUTINES IN PPPLIB.  IN THE HIGH-  *
! LEVEL (EASIER TO USE) ROUTINE CPLOT8, THE MATRIX Z(I,J) IS DIMEN-    *
! SIONED NX,NY.  IT CONTAINS THE VALUES OF THE FUNCTION F(X(I),Y(J)),  *
! WHERE I=1,2,..NX, AND J=1,2,..,NY.  TITLE CONTAINS A TITLE OF AN     *
! ARBITRARY NUMBER OF CHARACTERS TO BE DRAWN AT THE TOP OF THE GRAPH.  *
! CPLOT8 SCALES THE PLOT DYNAMICALLY AND TRACES THE SURFACE WITH NC    *
! CONTOUR LINES, WHERE NC IS FIXED IN THE PARAMETER STATEMENT BELOW.   *
! THE POSITION OF THE PLOT ON THE PAGE IS DETERMINED BY MX,MY (SEE     *
! LPLOT FOR MORE DETAILS).                                             *
!     WARNING: IN CPLOT8 THE FIRST DIMENSION OF Z IS ASSUMED TO EXTEND *
! OVER THE MAXIMUM RANGE DECLARED (CALLED NDIM IN THE LOWER LEVEL MAIN *
! ROUTINE CPLOT).  USE CPLOT WHEN THIS IS NOT THE CASE!                *
!                                                                      *
!     WRITTEN BY DEBBY HYMAN, 7-79                                     *
!     ADDED PARAMETER NC, HGO 17/12/85.                                *
!***********************************************************************
PARAMETER (NC=5)      
DIMENSION X(*),Y(*),Z(NX,*),ZC(NC)    
CHARACTER*(*) TITLE 
 
  CALL CPLOT(MX,MY,1,X,Y,NX,NY,1,1,Z,NX,ZC,-NC,TITLE,LEN(TITLE),'X',1,'Y',1)       

RETURN     
END        

SUBROUTINE VPLOT9(MX,MY,X,Y,NX,NY,VX,VY,TITLE)   
!***********************************************************************
!     A VECTOR PLOT GIVES A REPRESENTATION OF A TWO-DIMENSIONAL VECTOR *
! FIELD VX = F(X,Y), VY = G(X,Y) BY DRAWING VECTORS STARTING FROM DOTS *
! AT EACH DATA LOCATION.  THERE ARE TWO VECTOR PLOTTING SUBROUTINES IN *
! PPPLIB.  THE HIGH-LEVEL (EASIER TO USE) ROUTINE VPLOT9 PROCESSES     *
! NX*NY ELEMENTS OF THE TWO-DIMENSIONAL ARRAYS VX(I,J) = F(X(I),Y(J))  *
! AND VY(I,J) = G(X(I),Y(J)), CONTAINING THE HORIZONTAL AND VERTICAL   *
! FIELD COMPONENTS TO BE PLOTTED.  NX IS THE FIRST DIMENSION OF BOTH   *
! VX AND VY IN THE DIMENSION STATEMENT OF THE CALLING PROGRAM.  TITLE  *
! CONTAINS A TITLE OF AN ARBITRARY NUMBER OF CHARACTERS TO BE DRAWN AT *
! THE TOP OF THE GRAPH.  THE POSITION OF THE PLOT ON THE PAGE IS       *
! DETERMINED BY MX,MY (SEE LPLOT FOR MORE DETAILS).                    *
!     WARNING: IN VPLOT9 THE FIRST DIMENSION OF VX AND VY IS ASSUMED   *
! TO EXTEND OVER THE MAXIMUM RANGE DECLARED (CALLED NDIM IN THE LOWER  *
! LEVEL MAIN ROUTINE VPLOT).  USE VPLOT WHEN THIS IS NOT THE CASE!     *
!                                                                      *
!     WRITTEN BY DEBBY HYMAN, 7-79                                     *
!     ADDED ARGUMENTS X AND Y, HGO 17/12/85.                           *
!***********************************************************************
DIMENSION X(*),Y(*),VX(NX,*),VY(NX,*) 
CHARACTER*(*) TITLE 
       
  CALL VPLOT(MX,MY,111,X,Y,NX,NY,1,1,VX,VY,NX,.9,20,TITLE,LEN(TITLE),'X',1,'Y',1)       

RETURN     
END        

SUBROUTINE SPLOT9(MX,MY,IS,YX,ZXY,NX,NY,Z,TITLE) 
!***********************************************************************
!     THE SECTION PLOT ROUTINES PRODUCE ONE-DIMENSIONAL CROSS-SECTION  *
! PLOTS OF A TWO-DIMENSIONAL FUNCTION AT CONSTANT VALUE OF ONE OF THE  *
! TWO COORDINATES X AND Y.                                             *
!     SPLOT9 IS THE HIGH-LEVEL SECTION PLOT SUBROUTINE.  FOR IS=1 IT   *
! PRODUCES NS X-SECTION PLOTS AT EQUIDISTANT VALUES OF X.  IN THE SAME *
! WAY, FOR IS=2  SPLOT9 PRODUCES NS Y-SECTION PLOTS.  THE VALUE OF NS  *
! IS FIXED IN THE PARAMETER STATEMENT BELOW.  NX IS THE ACTUAL FIRST   *
! DIMENSION OF Z, AND NY IS THE ACTUAL SECOND DIMENSION.  TITLE CON-   *
! TAINS A TITLE OF AN ARBITRARY NUMBER OF CHARACTERS TO BE DRAWN AT    *
! THE TOP OF THE GRAPH.  THE POSITION OF THE PLOT ON THE PAGE IS DE-   *
! TERMINED BY MX,MY (SEE LPLOT FOR MORE DETAILS).                      *
!     WARNING: IN SPLOT9 THE FIRST DIMENSION OF Z IS ASSUMED TO EXTEND *
! OVER THE MAXIMUM RANGE DECLARED (CALLED NDIM IN THE LOWER LEVEL MAIN *
! ROUTINE SPLOT).  USE SPLOT WHEN THIS IS NOT THE CASE!                *
!                                                                      *
!     WRITTEN BY DEBBY HYMAN, 7-79                                     *
!     MERGED X- AND Y-SECTION SUBROUTINES BY MEANS OF THE ADDITIONAL   *
!     ARGUMENT IS, HGO 23/12/85.                                       *
!***********************************************************************
PARAMETER (NS=3)       
DIMENSION YX(*),ZXY(*),Z(NX,*),IJARR(NS)       
CHARACTER*(*) TITLE 
 
  IF(IS.EQ.1) CALL SPLOT(MX,MY,1,30971,YX,ZXY,NX,NY,1,Z,NX,IJARR,-NS,TITLE,LEN(TITLE),'Y',1,' ',1)         
  IF(IS.EQ.2) CALL SPLOT(MX,MY,2,30971,YX,ZXY,NX,NY,1,Z,NX,IJARR,-NS,TITLE,LEN(TITLE),'X',1,' ',1)         

RETURN     
END        


SUBROUTINE APLOT9(MX,MY,IA,YX,AVXY,NX,NY,Z,TITLE)         
!***********************************************************************
!     THE AVERAGE PLOT ROUTINES AVERAGE A TWO-DIMENSIONAL ARRAY IN ONE *
! DIRECTION AND PLOT THE RESULT WITH RESPECT TO THE OTHER DIRECTION.   *
!     APLOT9 IS THE HIGH-LEVEL AVERAGE PLOT SUBROUTINE.  FOR IA=1 IT   *
! AVERAGES THE MATRIX Z IN THE X-DIRECTION.  IN THE SAME WAY, FOR IA=2 *
! APLOT9 PRODUCES A Y-AVERAGE PLOT.  NX IS THE ACTUAL FIRST DIMENSION  *
! OF Z, AND NY IS THE ACTUAL SECOND DIMENSION.  TITLE CONTAINS A TITLE *
! OF AN ARBITRARY NUMBER OF CHARACTERS TO BE DRAWN AT THE TOP OF THE   *
! GRAPH.  THE POSITION OF THE PLOT ON THE PAGE IS DETERMINED BY MX,MY  *
! (SEE LPLOT FOR MORE DETAILS).                                        *
!     WARNING: IN APLOT9 THE FIRST DIMENSION OF Z IS ASSUMED TO EXTEND *
! OVER THE MAXIMUM RANGE DECLARED (CALLED NDIM IN THE LOWER LEVEL MAIN *
! ROUTINE APLOT).  USE APLOT WHEN THIS IS NOT THE CASE!                *
!                                                                      *
!     WRITTEN BY DEBBY HYMAN, 7-79                                     *
!     MERGED X- AND Y-SECTION SUBROUTINES BY MEANS OF THE ADDITIONAL   *
!     ARGUMENT IA, HGO 23/12/85.                                       *
!***********************************************************************
DIMENSION YX(*),AVXY(*),Z(NX,*)
CHARACTER*(*) TITLE 
       
  IF(IA.EQ.1) CALL APLOT(MX,MY,1,YX,AVXY,NX,NY,1,Z,NX,1,NX,TITLE,LEN(TITLE),'Y',1,' ',1)    
  IF(IA.EQ.2) CALL APLOT(MX,MY,2,YX,AVXY,NX,NY,1,Z,NX,1,NY,TITLE,LEN(TITLE),'X',1,' ',1)    

RETURN     
END        

     
SUBROUTINE LPLOT(MX,MY,IOP,X,Y,NPTS,INC,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)  
!***********************************************************************
!     THIS SUBROUTINE DRAWS A LINE PLOT OF THE NPTS VALUES IN X AND Y. *
! LPLOT DETERMINES THE RANGES OF X AND Y ,SUBSEQUENTLY CALLS NFRAME TO *
! DRAW A BOX AROUND THE PLOT, TO SCALE THE X- AND Y-AXES, AND TO PLACE *
! A TITLE AND LABELS ALONG THE AXES, AND FINALLY PUTS THE CURVE ON THE *
! PLOT.  THIS SEQUENCE MAY BE SPLIT BY THE USE OF NEGATIVE VALUES OF   *
! THE ARGUMENTS IOP (TO SUPPRESS PLOTTING OF THE CURVE) AND NPTS (TO   *
! SUPPRESS PLOTTING OF THE FRAME AND SCALES).                          *
!                                                                      *
!     THE PLOT'S POSITION ON THE PAGE IS DETERMINED BY IMX,IMY.  IMY=1 *
! SPECIFIES THAT THE Y-COORDINATE RANGE SPANS A FULL PAGE; IMY=2 AND 3 *
! SPECIFY THE UPPER AND LOWER HALVES OF THE PAGE; AND IMY=4, 5, AND 6  *
! SPECIFY THE UPPER, MIDDLE, AND LOWER THIRDS OF THE PAGE.  IMX=1 SPE- *
! CIFIES THAT THE X-COORDINATE RANGE SPANS A FULL PAGE; IMX=2 AND 3    *
! SPECIFY THE LEFT AND RIGHT HALVES OF THE PAGE; WHILE IMX=4, 5, AND 6 *
! ARE NOT ALLOWED.                                                     *
!     FOR EXAMPLE, (IMX,IMY)=(1,1) SPECIFIES A PLOT FILLING THE FULL   *
! PAGE, AND (3,3) SPECIFIES A PLOT IN THE LOWER RIGHT-HAND QUADRANT.   *
! A MAXIMUM OF SIX PLOTS ON A PAGE IS POSSIBLE IF THE PAIRS (2,4),     *
! (3,4), (2,5), (3,5), (2,6), AND (3,6) ARE USED.  PAGE ADVANCE IS     *
! AUTOMATIC WITH THE FIRST PLOT THAT EXTENDS INTO THE UPPER LEFT-HAND  *
! CORNER OF THE PAGE.  SUCH A PLOT MUST BE THE FIRST IN ANY PLOT       *
! SEQUENCE INTENDED TO APPEAR ON ONE PAGE.                             *
!                                                                      * 
!     THE PLOT'S RANGE USUALLY IS EXPANDED TO A "ROUND" DECIMAL NUMBER *
! BY THE AUTOMATIC SCALING ROUTINES FROM THE MINIMUM RANGE IMPLIED BY  *
! THE DATA.  EXPANSION MAY BE PREVENTED BY APPENDING A '1' IN FRONT OF *
! THE IMX AND IMY VALUES IN ANY PLOT CALL (I.E., ISX=1).  FOR EXAMPLE, *
! IF XMIN=0.17, XMAX=359.78, AND ISX=0 FOR AUTOMATIC SCALING, THE X-   *
! SCALE GOES FROM 0.0 TO 400.0.  NOW, IF XMIN AND XMAX STAY THE SAME   *
! AND ISX=1 FOR EXACT SCALING, THE X-SCALE GOES FROM 0.0 TO 360.0.     *
!                                                                      *
!     ARGUMENTS:                                                       *
!                                                                      *
! MX     - DEFINES THE GRAPH AREA AND THE SCALING IN THE X-DIRECTION   *
! ACCORDING TO THE FORMULA                                             *
!    IABS(MX) = IIX*1000 + IAX*100 + ISX*10 + IMX ,                    *
! WHERE IMX DETERMINES THE HORIZONTAL EXTENSION OF THE PLOT:           *
!    IMX = 1 - FULL PAGE                                               *
! 2 - LEFT HALF OF THE PAGE                                            *
! 3 - RIGHT HALF OF THE PAGE,                                          *
! AND ISX DETERMINES THE SCALING ALONG THE X-AXIS:                     *
!    ISX = 0 - AUTOMATIC SCALING WITH EXPANSION (DEFAULT)              *
! 1 - EXACT SCALING (NO ROUNDING)                                      *
! 2 - EQUIDISTANT SCALING WITH THE X-SCALE ADAPTED                     *
!     TO THE LENGTHS ALONG Y (SEE NOTE IN NFRAME),                     *
! AND IAX PROVIDES AN ADDITIONAL OPTION:                               *
!    IAX = 0 - NO ACTION (DEFAULT)                                     *
! 1 - X=0 AXIS IS DRAWN  (IF IT LIES IN THE RANGE)                     *
! 2 - X=0 AXIS IS DASHED (IF IT LIES IN THE RANGE),                    *
! AND IIX OVERRULES THE DEFAULT NUMBER OF SCALE INTERVALS:             *
!    IIX = 0 - 4 INTERVALS FOR SCALES AND TICKMARKS (DEFAULT)          *
!    IIX > 0 - IIX INTERVALS (NOT FOR AUTOMATIC SCALING).              *
! MX < 0 : PLOTTING OF SCALES AND TICK MARKS SUPPRESSED.               *
! MY     - DEFINES THE GRAPH AREA AND THE SCALING IN THE Y-DIRECTION,  *
! ANALOGOUS TO THE ABOVE EXPRESSIONS WITH X REPLACED BY Y,             *
! WHERE IMY DETERMINES THE VERTICAL EXTENSION OF THE PLOT:             *
!    IMY = 1 - FULL PAGE                                               *
! 2 - TOP HALF OF THE PAGE                                             *
! 3 - BOTTOM HALF OF THE PAGE                                          *
! 4 - TOP THIRD OF THE PAGE                                            *
! 5 - MIDDLE THIRD OF THE PAGE                                         *
! 6 - BOTTOM THIRD OF THE PAGE.                                        *
! IOP    - PROVIDES DIFFERENT OPTIONS FOR THE X-Y SCALES, THE SYMBOLS  *
! PLOTTED, AND THE CURVE DRAWN, ACCORDING TO THE FORMULA               *
!    IABS(IOP) = N*10000 + IC*10 + JOP,                                *
! WHERE JOP DETERMINES THE SCALES ALONG THE X- AND Y-AXES:             *
!    JOP = 1 - LINEAR X-AXIS, LINEAR Y-AXIS                            *
! 2 - LINEAR X-AXIS, LOG Y-AXIS                                        *
! 3 - LOG X-AXIS, LINEAR Y-AXIS                                        *
! 4 - LOG X-AXIS, LOG Y-AXIS                                           *
! 5 - LINEAR X-AXIS, LINEAR Y-AXIS (BUT PLOTTING OF                    *
!     FRAME, SCALES, AND TICK MARKS SUPPRESSED),                       *
! AND IC INDICATES THE ASCII CHARACTER TO BE PLACED AT THE             *
! POINTS:                                                              *
!    IC = 0 (DEFAULT) - NO CHARACTER PLACED                            *
!    32 (192) <= IC <= 126 (254)                                       *
!   - CHARACTER FROM TABLE OF DLCH,                                    *
! AND N DETERMINES THE SPACING BETWEEN THE PLOTTED CHARACTERS          *
! AND WHETHER A CURVE IS TO BE DRAWN THROUGH THEM:                     *
!    N = 0 (DEFAULT) - SYMBOL SPECIFIED BY IC PLACED AT EACH           *
!    POINT; THE POINTS ARE NOT CONNECTED                               * 
!    N > 0  - A SYMBOL PLACED AT EVERY N'TH POINT;                     *
!    ALL POINTS ARE CONNECTED BY A CURVE.                              *
! IOP < 0: THE FRAME IS DRAWN AND THE AXES ARE SCALED, BUT             *
! THE CURVE IS NOT DRAWN.  THIS AMOUNTS TO JUST A CALL OF              *
! NFRAME WITH AUTOMATIC DETERMINATION OF THE EXTREME VALUES            *
! OF X AND Y BY LPLOT.  (IF THESE VALUES ARE ALREADY KNOWN,            *
! IT IS MORE EFFICIENT TO CALL NFRAME DIRECTLY).                       *
! X      - THE TABLE OF ABSCISSA VALUES TO BE PLOTTED.                 *
! Y      - THE TABLE OF ORDINATE VALUES TO BE PLOTTED.                 *
! NPTS   - IABS(NPTS) IS THE NUMBER OF X/Y ELEMENTS.                   *
! NPTS < 0: A CURVE IS DRAWN ONTO A FRAME PREVIOUSLY SET UP            *
! BY A CALL TO NFRAME OR LPLOT WITH IOP < 0.                           *
! INC    - IABS(INC) IS THE SPACING BETWEEN THE X/Y ELEMENTS PLOTTED.  *
! INC < 0: THE Y-ELEMENTS PLOTTED ARE PAIRED WITH ABSCISSA             *
! VALUES DETERMINED BY THE TWO VALUES XMIN=X(1) AND DX=X(2),           *
! WHICH THE USER SHOULD INSERT IN X.                                   *
! TITLE  - TITLE FOR THE GRAPH.                                        *
! NTITLE - THE NUMBER OF CHARACTERS IN NTITLE.                         *
! XNAME  - LABEL FOR THE X-AXIS.                                       *
! NXNAME - NUMBER OF CHARACTERS IN XNAME.                              *
! YNAME  - LABEL FOR THE Y-AXIS.                                       * 
! NYNAME - NUMBER OF CHARACTERS IN YNAME.                              *
! THE ABOVE THREE CHARACTER STRINGS ARE AUTOMATICALLY TRUN-            *
! CATED TO FIT ALONGSIDE THE CHOSEN FRAME.  THE FONT CAN BE            *
! CHANGED ACCORDING TO THE RULES GIVEN IN DLCH.                        *
!                                                                      *
!     ENTRY HPLOT DRAWS A HISTOGRAM OF THE VALUES IN X AND Y.  THE     *
! ARGUMENTS ARE THE SAME AS FOR LPLOT.                                 *
!                                                                      * 
!     WRITTEN BY CLAIR NIELSON.                                        * 
!     MODIFIED BY DENNIS HEWETT 2-78, FOR RANGE PRINTED ON TOP RIGHT.  *
!     MODIFIED BY BOB MALONE 3-78, FOR CHARACTERS ON THE CURVE.        *
!     MODIFIED BY DEBBY HYMAN 4-80, FOR INCREMENTATION TO WORK,        *
!     FOR RNG TO BE PRINTED ONLY WHEN VERY SMALL.                      *
!     MODIFIED BY HANS GOEDBLOED 14/11/85, FOR ADAPTATION TO NEW DLCH, *
!     SHIFT OF THE TITLE WHEN RNG IS PRINTED.                          *
!     MODIFIED BY GUIDO HUYSMANS 1/07/89, FOR CLIPPING LINES TO FIT    *
!     THE FRAME.                                                       *
!     MODIFIED BY RONALD VAN DER LINDEN 6/90, TO MAKE IT POSSIBLE      *
!     TO HAVE 100 INSTEAD OF 10 DATA POINTS BETWEEN PRINTED SYMBOLS.   *
!***********************************************************************
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
DIMENSION X(*),Y(*)   
CHARACTER*(*) TITLE,XNAME,YNAME       
CHARACTER TITLE1*80,RANGE*14 
LOGICAL FHIST,FLOGX,FLOGY,FCONN,FCHAR 
       
!     * FLAG FOR HISTOGRAM.        
FHIST=.FALSE.       
       
10 IOPA=IABS(IOP)        
   JOP=MOD(IOPA,10)      
   FLOGX=.FALSE.       
   FLOGY=.FALSE.       
   IF(JOP.EQ.3.OR.JOP.EQ.4) FLOGX=.TRUE. 
   IF(JOP.EQ.2.OR.JOP.EQ.4) FLOGY=.TRUE. 
   NTOT=IABS(NPTS)       
   INCA=IABS(INC)        
!       
!     * SCHEME FOR CHARACTERS ON THE CURVE BY BOB MALONE, 3/78         
!     * SET DEFAULTS FOR OPERATION WITHOUT CHARACTERS ON CURVES.       
!
   FCONN=.TRUE.        
   FCHAR=.FALSE.       
      N=1        
!       
!     * DETERMINE WHETHER CHARACTERS ARE DESIRED.    
!
   IC=MOD(IOPA/10,1000)  
   IF(IC.NE.0) THEN    
     FCHAR=.TRUE.     
     N=MOD(IOPA/10000,100)       
     IF(N.EQ.0) THEN  
       FCONN=.FALSE. 
       N=1  
     ENDIF   
   ENDIF      
!
!     * REDUCE CHARACTERSIZE FOR SMALLEST PLOTS
!
   IMX=MOD(IABS(MX),10)  
   IMY=MOD(IABS(MY),10)
   ICHARSIZE=2         
   IF ((IMX.GE.2).AND.(IMY.GE.2)) ICHARSIZE=1
!       
!     * DRAW THE FRAME.   
!
   IF(NPTS.GT.0) THEN  
     IF(INC.LT.0) THEN         
       XMN=X(1)        
       XMX=X(1)+(NTOT-1)*X(2)/INCA     
     ELSE    
       CALL MAXV(X,NTOT,INCA,XMX,IDUM) 
       CALL MINV(X,NTOT,INCA,XMN,IDUM) 
     ENDIF   
     CALL MAXV(Y,NTOT,INCA,YMX,IDUM)    
     CALL MINV(Y,NTOT,INCA,YMN,IDUM)    
     NB=0    
     RNG=ABS(YMX-YMN)   
     IF(RNG.LT.(.02*ABS(YMX))) THEN     
       WRITE(RANGE,'(''RNG ='',1PE9.2)') RNG    
       NB=10         
     ENDIF   
     TITLE1=TITLE     
     NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE)         
     CALL NFRAME(MX,MY,JOP,XMN,XMX,YMN,YMX, &     
     TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME) 
     IF(NB.NE.0) CALL DLCH(IXR-120,IYT+8,RANGE,14,1)      
     IF(IOP.LT.0) RETURN       
   ELSE       
     CALL OFRAME(MX,MY) 
   ENDIF      
       
!     * DRAW THE CURVE.   
   XFAC=real(IXR-IXL)/(XR-XL)   
   YFAC=real(IYT-IYB)/(YT-YB) 
   HX=0.      
   IF(INC.LT.0) HX=X(2)  
   XJS=X(1)     
   XJ=XJS     
   IF(FLOGX) XJ=ALOG19(XJ)
   ZIX1=real(IXL)+(XJ-XL)*XFAC      
   YJ=Y(1)      
   IF(FLOGY) YJ=ALOG19(YJ)        
   ZIY1=real(IYB)+(YJ-YB)*YFAC         
   IF(FHIST) CALL DRV(ZIX1,real(IYB),ZIX1,ZIY1)  
   IF(FCHAR.AND.N.EQ.1) CALL DLCH(INT(ZIX1),-INT(ZIY1),' ',IC,1)    

   DO 20 J=1+INCA,NTOT,INCA     
     XJS=XJS+HX       
     IF(INC.GT.0) XJS=X(J)       
       XJ=XJS  
       IF(FLOGX) XJ=ALOG19(XJ)     
         ZIX=real(IXL)+(XJ-XL)*XFAC     
         YJ=Y(J)   
         IF(FLOGY) YJ=ALOG19(YJ)
         ZIY=real(IYB)+(YJ-YB)*YFAC        
         IF(FHIST) THEN   
!  * HISTOGRAM DRAWN BY THESE CALLS TO DRV. 
           CALL DRV(ZIX1,ZIY1,ZIX,ZIY1)    
           CALL DRV(ZIX,ZIY1,ZIX,ZIY)      
         ELSE    
           IF(FCONN) THEN         
             ZIDX1=ZIX1         
             ZIDY1=ZIY1 
             ZIDX=ZIX   
             ZIDY=ZIY   
             CALL CLIP(ZIDX1,ZIDY1,ZIDX,ZIDY)      
           ENDIF         
           IF(FCHAR.AND.MOD(J,N).EQ.0) THEN         
           IF((ZIX.GT.real(IXL).AND.ZIX.LT.real(IXR)).AND.   &    
              (ZIY.GT.real(IYB).AND.ZIY.LT.real(IYT)))       &     
             CALL DLCH(INT(ZIX),-INT(ZIY),' ',IC,-ICHARSIZE)         
           ENDIF         
         ENDIF   
         ZIX1=ZIX         
         ZIY1=ZIY         
   20 CONTINUE   
      RETURN     
       
!     * ENTRY FOR DRAWING A HISTOGRAM.      
      ENTRY HPLOT(MX,MY,IOP,X,Y,NPTS,INC,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)     
      FHIST=.TRUE.        
      GOTO 10    
END        
      
SUBROUTINE CLIP(ZIX1,ZIY1,ZIX2,ZIY2)        
!***********************************************************************
!     SUBROUTINE TO CLIP LINE TO LINEPIECE WITHIN PLOT WINDOW.         *
!     SOURCE : INTERACTIVE GRAPHICS, P. 88                             *
!     ADDED BY GUIDO HUYSMANS 1/07/89.                                 * 
!***********************************************************************
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
INTEGER C,C1,C2     
       
CALL PPPCODE(ZIX1,ZIY1,C1)        
CALL PPPCODE(ZIX2,ZIY2,C2)        

10 IF((C1.GT.1).OR.(C2.GT.1)) THEN       
     IF(MOD(44100/(C1*C2),210).NE.0) RETURN      
       C = C1  
       IF(C.LE.1) C = C2         
       IF(MOD(C,5).EQ.0) THEN    
         ZIY = ZIY1 + (ZIY2-ZIY1)*(real(IXL)-ZIX1)/(ZIX2-ZIX1)   
         ZIX = real(IXL)         
       ELSE    
         IF(MOD(C,7).EQ.0) THEN 
           ZIY = ZIY1 + (ZIY2-ZIY1)*(real(IXR)-ZIX1)/(ZIX2-ZIX1)  
           ZIX = real(IXR)     
         ELSE 
           IF(MOD(C,3).EQ.0) THEN       
             ZIX = ZIX1 + (ZIX2-ZIX1)*(real(IYB)-ZIY1)/(ZIY2-ZIY1)
             ZIY = real(IYB)   
           ELSE       
             IF(MOD(C,2).EQ.0) THEN    
               ZIX = ZIX1 + (ZIX2-ZIX1)*(real(IYT)-ZIY1)/(ZIY2-ZIY1)         
               ZIY = real(IYT)  
             ENDIF   
           ENDIF      
         ENDIF         
       ENDIF   
       IF(C.EQ.C1) THEN 
         ZIX1 = ZIX   
         ZIY1 = ZIY   
         CALL PPPCODE(ZIX,ZIY,C1)   
       ELSE       
         ZIX2 = ZIX    
         ZIY2 = ZIY    
         CALL PPPCODE(ZIX,ZIY,C2)   
       ENDIF   
       GOTO 10 
    ENDIF      
    CALL DRV(ZIX1,ZIY1,ZIX2,ZIY2) 

RETURN     
END        
       
SUBROUTINE PPPCODE(ZIX,ZIY,C)    
!**********************************************************************
!     ADDED BY GUIDO HUYSMANS 1/07/89.                                *
!**********************************************************************
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
INTEGER C  
      
  C = 1      
  IF(ZIX.LT.real(IXL)) THEN  
    C = 5*C 
  ELSE       
    IF(ZIX.GT.real(IXR)) C = 7*C      
  ENDIF      
  IF(ZIY.LT.real(IYB)) THEN   
    C = 3*C 
  ELSE       
    IF(ZIY.GT.real(IYT)) C = 2*C      
  ENDIF      

RETURN     
END        

      
SUBROUTINE PPLOT(MX,MY,X,Y,NPTS,INC)    
!***********************************************************************
!     SUBROUTINE PPLOT PLOTS THE VALUES IN X AND Y.  EACH POINT IS RE- *
! PRESENTED BY A PLOTTING DOT, AND ADJACENT POINTS ARE NOT CONNECTED.  *
! ENTRY PPLOTC PROVIDES A CONDITIONAL POINT PLOT OF THOSE POINTS FOR   *
! WHICH THE Z VALUE SATISFIES ZMIN < Z < ZMAX.  THE ROUTINES HAVE BEEN *
! OPTIMIZED TO PLOT MANY PARTICLES AS DOTS.                            *
!     BOTH SUBROUTINES ASSUME THAT THE FRAME, SCALE, AND LABELS FOR    *
! THIS (IMX,IMY) PLOT HAVE BEEN GENERATED BY A PREVIOUS CALL OF LPLOT  *
! WITH IOP = -1 OR A DIRECT CALL OF NFRAME.  ONLY LINEAR-LINEAR SCA-   *
! LING IS ALLOWED.  IF PPLOT IS CALLED WITHOUT A PRECEDING LPLOT CALL, *
! IT WILL USE THE SCALING LEFT IN COMMON BLOCK CJE07 FOR THAT FRAME.   *
!                                                                      *
!     ARGUMENTS:                                                       *
!                                                                      *
! MX/MY - SEE LPLOT.                                                   *
! X     - THE TABLE OF ABSCISSA VALUES.                                *
! Y     - THE TABLE OF ORDINATE VALUES.                                *
! NPTS  - THE NUMBER OF ELEMENTS IN THE ARRAYS X, Y, AND Z.            *
! INC   - IABS(INC) IS THE SPACING BETWEEN THE X/Y ELEMENTS PLOTTED.   *
!                                                                      *
!     ADDITIONAL ARGUMENTS FOR PPLOTC:                                 *
!                                                                      *
! Z     - A FUNCTION OF X AND Y.                                       *
! ZMIN  - THE SMALLEST VALUE TO BE PLOTTED.                            *
! ZMAX  - THE LARGEST VALUE TO BE PLOTTED.                             *
!                                                                      *
!     WRITTEN BY CLAIR NIELSON.                                        *
!***********************************************************************
  COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
  DIMENSION X(*),Y(*),Z(*)       
   
  CALL OFRAME(MX,MY)    
  XFAC=(IXR-IXL)/(XR-XL)         
  YFAC=(IYT-IYB)/(YT-YB)         
  DO J=1,NPTS,IABS(INC)       
     ZIX=MIN(MAX(real(IXL),real(IXL)+(X(J)-XL)*XFAC),real(IXR))         
     ZIY=MIN(MAX(real(IYB),real(IYB)+(Y(J)-YB)*YFAC),real(IYT))         
     CALL DRP(ZIX,ZIY)    
  ENDDO   
  RETURN     
   
  ENTRY PPLOTC(MX,MY,X,Y,NPTS,INC,Z,ZMIN,ZMAX)   
   
  CALL OFRAME(MX,MY)    
  XFAC=(IXR-IXL)/(XR-XL)         
  YFAC=(IYT-IYB)/(YT-YB)         
  DO 20 J=1,NPTS,IABS(INC)       
     IF(Z(J).LT.ZMIN) GOTO 20  
     IF(Z(J).GT.ZMAX) GOTO 20  
     ZIX=MIN(MAX(real(IXL),real(IXL)+(X(J)-XL)*XFAC),real(IXR))         
     ZIY=MIN(MAX(real(IYB),real(IYB)+(Y(J)-YB)*YFAC),real(IYT))         
     CALL DRP(ZIX,ZIY)    
20 CONTINUE   

RETURN     
END        


SUBROUTINE DPLOT(MX,MY,X,Y,NPTS,INC,L1,L2)       
!***********************************************************************
!     DPLOT DRAWS A DASHED OR DOTTED CURVE THROUGH THE POINTS          *
! X(I),Y(I), I=1,NPTS,INC,                                             *
! WHERE L1 AND L2 ARE THE LENGTHS OF THE STROKES AND SPACES OF THE     *
! LINE.  E.G., IF L1=0, A DOTTED CURVE IS PRODUCED WITH DISTANCES L2   *
! BETWEEN THE DOTS.  IF L2=0 THE CURVE IS FULLY DRAWN (OF COURSE, ONE  *
! SHOULD NOT USE DPLOT BUT LPLOT IN THAT CASE).                        *
!     THIS SUBROUTINE ASSUMES A PREVIOUS CALL OF NFRAME OR LPLOT WITH  *
! IOP = -1 TO SET UP THE FRAME AND SCALING.                            *
!                                                                      *
!     ARGUMENTS:                                                       *
!                                                                      *
! MX/MY - SEE LPLOT.                                                   *
! X     - THE TABLE OF ABSCISSA VALUES.                                *
! Y     - THE TABLE OF ORDINATE VALUES.                                *
! NPTS  - THE NUMBER OF ELEMENTS IN THE ARRAYS X AND Y.                *
! INC   - IABS(INC) IS THE SPACING BETWEEN THE X/Y ELEMENTS USED.      *
! L1    - LENGTH OF THE STROKES IN PLOTTING COORDINATES.               *
! L2    - LENGTH OF THE SPACES IN PLOTTING COORDINATES.                *
!                                                                      *
!     WRITTEN HGO 28/01/86                                             *
!***********************************************************************
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
DIMENSION X(*),Y(*)   
 
CALL OFRAME(MX,MY)    
XFAC=real(IXR-IXL)/(XR-XL)   
YFAC=real(IYT-IYB)/(YT-YB)   
ZIX1=MIN(MAX(real(IXL),real(IXL)+(X(1)-XL)*XFAC),real(IXR))  
ZIY1=MIN(MAX(real(IYB),real(IYB)+(Y(1)-YB)*YFAC),real(IYT))         
L=0        
INCA=IABS(INC)        

DO  J = 1+INCA, NPTS,INCA     
  ZIX=MIN(MAX(real(IXL),real(IXL)+(X(J)-XL)*XFAC),real(IXR))         
  ZIY=MIN(MAX(real(IYB),real(IYB)+(Y(J)-YB)*YFAC),real(IYT)) 
  CALL DASH(ZIX1,ZIY1,ZIX,ZIY,L1,L2,L,LL)     
  L=LL    
  ZIX1=ZIX         
  ZIY1=ZIY         
ENDDO 

RETURN     
END        

      
SUBROUTINE DASH(ZIX1,ZIY1,ZIX2,ZIY2,L1,L2,L,LL)        
!***********************************************************************
!     THIS ROUTINE DRAWS A DASHED LINE FROM (IX1,IY1) TO (IX2,IY2).    *
! THE ARGUMENTS L1 AND L2 ARE THE LENGTHS IN PLOTTING COORDINATES OF   *
! THE STROKES AND SPACES OF THE LINE, RESP.  L IS THE INITIAL POSITION *
! (INPUT) AND LL IS THE FINAL POSITION (OUTPUT) OF THE POINTER ON THE  *
! PLOTTING STRIP (0,L1+L2).                                            *
!                                                                      *
!     WRITTEN HGO 28/01/86                                             *
!***********************************************************************
LL=L       
R=SQRT(REAL((ZIX2-ZIX1)**2+(ZIY2-ZIY1)**2))        
IF(R.NE.0.) THEN   
   XFAC=(ZIX2-ZIX1)/R        
   YFAC=(ZIY2-ZIY1)/R        
ELSE       
   XFAC=1. 
   YFAC=1. 
ENDIF      
IR=INT(R)    
LTOT=0     
DX=0.      
DY=0.      
ZIX=ZIX1   
ZIY=ZIY1   
CALL MOVABS(ZIX,ZIY)  

10 IF(LL.EQ.0.AND.L1.EQ.0) CALL DRP(ZIX,ZIY)      
   IF(LL.LT.L1) THEN   
     L11=MIN(L1-LL,IR-LTOT)      
     DX=DX+L11*XFAC   
     DY=DY+L11*YFAC   
     ZIX=ZIX1+DX      
     ZIY=ZIY1+DY      
!!!        IF(L1.NE.0) CALL DRWABS(ZIX,ZIY)    
     IF(L1.NE.0) CALL CLIP(ZIX1,ZIY1,ZIX,ZIY)       
     IF(L1.EQ.0) CALL DRP(ZIX,ZIY)      
     LTOT=LTOT+L11    
     LL=LL+L11        
   ELSE       
     L22=MIN(L1+L2-LL,IR-LTOT)   
     DX=DX+L22*XFAC   
     DY=DY+L22*YFAC   
     ZIX=ZIX1+DX      
     ZIY=ZIY1+DY      
     CALL MOVABS(ZIX,ZIY)        
     LTOT=LTOT+L22    
     LL=LL+L22        
     IF(LL.GE.L1+L2) LL=0      
   ENDIF      
   IF(LTOT.LT.IR) GOTO 10       

RETURN     
END        

SUBROUTINE CPLOT(MX,MY,ILAB,X,Y,NX,NY,INCX,INCY,Z,NDIM,ZC,NC,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)
!***********************************************************************
!     SUBROUTINE CPLOT DRAWS NC CONTOURS OF THE FUNCTION Z = F(X,Y).   *
! THIS FUNCTION SHOULD BE STORED AS A TWO-DIMENSIONAL ARRAY Z(I,J),    *
! COMPUTED AT THE POINTS   X(I), I=1,IABS(NX),IABS(INCX),              *
!                          Y(J), J=1,IABS(NY),IABS(INCY).              *
! ENTRY CPLOTX IS AN EXTENSION FOR DRAWING POLAR PLOTS AND LOG10 CON-  *
! TOURS.                                                               *
!                                                                      *
!     ARGUMENTS:                                                       *
!                                                                      *
! MX/MY - SEE LPLOT.                                                   *
! ILAB  - CONTROLS THE ABSENCE/PRESENCE (ILAB=0/1) OF ALPHABETIC       *
!         LABELS ON THE CONTOURS.  THE CHOICE OF THE LABELS IS FIXED   *
!         IN THE PARAMETER STATEMENT BELOW TO BE UPPER CASE (N1=65),   *
!         LOWER CASE (N1=97), OR GREEK (N1=225).                       *
! X     - TABLE OF ABSCISSA VALUES.                                    *
! Y     - TABLE OF ORDINATE VALUES.                                    *
! NX    - IABS(NX) IS THE NUMBER OF POINTS IN X TO BE USED.            *
!         NX < 0 : ONLY THE FRAME AND SCALES FOR THE PLOT ARE DRAWN.   *
! NY    - IABS(NY) IS THE NUMBER OF POINTS IN Y TO BE USED.            *
!         NY < 0 : CONTOURS ARE DRAWN ON A FRAME PREVIOUSLY CREATED    *
!         BY A CALL TO CPLOT WITH NX < 0 OR A DIRECT CALL OF NFRAME    *
!         (IN ORDER TO SPECIFY A FRAME SIZE DIFFERENT FROM THE ONE     *
!         IMPLIED BY THE RANGES OF X AND Y).                           *
! INCX  - IABS(INCX) IS THE SKIP PARAMETER IN A ROW.                   *
!         INCX < 0 : XMIN = X(1) AND HX = X(2).                        *
! INCY  - IABS(INCY) IS THE SKIP PARAMETER IN A COLUMN.                *
!         INCY < 0 : YMIN = Y(1) AND HY = Y(2).                        *
! Z     - THE TWO-DIMENSIONAL FUNCTION TO BE CONTOURED; Z SHOULD BE    *
!         STORED SO THAT Z(I,J) IS THE VALUE OF Z AT X(I),Y(J).        *
! NDIM  - LENGTH OF A ROW OF Z (1ST DIMENSION OF THE 2-D ARRAY).       *
!         HENCE, ONE SHOULD OBSERVE: NX <= NDIM.                       *
! ZC    - THE TABLE OF CONTOUR VALUES, WHICH SHOULD BE DIMENSIONED AT  *
!         LEAST AS ZC(NC) IN THE CALLING PROGRAM.                      *
! NC    - NUMBER OF CONTOURS TO BE PLOTTED; MAXIMUM OF 26.             *
!         NC < 0 : CPLOT AUTOMATICALLY FILLS ZC WITH NC VALUES.        *
!         NC > 0 : ZC IS SUPPLIED BY THE USER; VALUES MUST BE STORED   *
!         IN INCREASING ORDER IN ZC.                                   *
! TITLE                - TITLE FOR THE GRAPH.                          *
! XNAME/YNAME          - LABEL FOR THE X/Y-AXIS.                       *
! NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *
!                                                                      *
!     ADDITIONAL ARGUMENTS FOT CPLOTX:                                 *
!                                                                      *
! RMAX  - MAXIMUM RADIUS FOR A POLOR PLOT.                             *
!         = 0 : CARTESIAN PLOT.                                        *
!         > 0 : X/Y CORRESPONDS TO R/THETA (IN RADIANS).               *
!         < 0 : X/Y CORRESPONDS TO R/COS(THETA).                       *
! IQUAD - TOTAL NUMBER OF QUADRANTS (FOR RMAX.NE.0 ONLY).              *
! LGZ   - IABS(ILGZ) CONTROLS THE NUMBER OF LOG10 CONTOURS.            *
!         LGZ = 0 : SCALAR CONTOURS.                                   *
!         ILGZ = 1,2,3,4 : LOG10 CONTOURS AT II = 1,9,ILGZ INTVLS/DEC. *
!         LGZ > 0 : THIS NUMBER II IS AUTOMATICALLY OVERRIDDEN (DOWN   *
!         TO 1 INT/DEC, DEPENDING ON THE NUMBER OF DECADES COMPUTED    *
!         FROM THE RANGE OF Z) TO GET A REASONABLE NUMBER OF CONTOURS. *
!         LGZ < 0 : THE AUTOMATIC OVERRIDE IS SWITCHED OFF, BUT THE    *
!         NUMBER OF DECADES IS LIMITED TO 8.                           *
!                                                                      *
!     WRITTEN BY CLAIR NIELSON.                                        *
!     MODIFIED BY DENNIS HEWETT 2/8/78, FOR CONTOLLING CONTOUR LABELS, *
!     ADDED VARIABLE NLAB BOTH HERE AND IN TRICJ3.                     *
!     EXTENDED BY DENNIS HEWETT 12/8/82, WITH CPLOTX AND TRICJ3 FOR    *
!     LOG10 CONTOURS AND POLAR PLOTS.                                  *
!     ADDED ARGUMENT ILAB (=NLAB), HGO 6/12/85.                        *
!***********************************************************************
      PARAMETER (N1=97)                                                 
                                                                       
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT                         
      COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C                
      DIMENSION X(*),Y(*),Z(NDIM,*),ZC(*)                               
      CHARACTER*(*) TITLE,XNAME,YNAME                                   
      DIMENSION ZT(4)                                                   
      CHARACTER*19 AMIN,AMAX                                            
      CHARACTER*80 TITLE1                                               
      LOGICAL FLGZ                                                      
                                                                       
!     * INITIALIZE N1 AND ISYM FOR USE IN TRICJ3.                       
      N1C=N1                                                            
      DO 5 I=1,26                                                       
    5    ISYM(I)=0                                                      
                                                                       
!     * INITIALIZE FOR SCALAR CONTOURS.                                 
      ILGZ=0                                                            
!     * FLAG DOWN FOR OVERRIDING THE AUTOMATIC DETERMINATION OF THE     
!     * NUMBER OF CONTOURS PER DECADE IN THE CASE OF LOG10 CONTOURS.    
      FLGZ=.FALSE.          
                                                                             
!     * INPUT PARAMETERS.                                               
   10 ICORD=0                                                           
      RMX=0.                                                            
      NNX=IABS(NX)                                                      
      NNY=IABS(NY)                                                      
      IF(NNY.LE.1) RETURN                                               
      NLAB=IABS(ILAB)                                                   
      INX=IABS(INCX)                                                    
      INY=IABS(INCY)                                                    
      XMN=X(1)                                                          
      YMN=Y(1)                                                          
      IF(INCX.LE.0) THEN                                                
         HX=X(2)                                                        
         XMX=X(1)+(NNX-1)*X(2)/INX                                      
      ELSE                                                              
         XMX=X(NNX)                                                     
      ENDIF                                                             
      IF(INCY.LE.0) THEN                                                
         HY=Y(2)                                                        
         YMX=Y(1)+(NNY-1)*Y(2)/INY                                      
      ELSE                                                              
         YMX=Y(NNY)                                                     
      ENDIF                                                             
                                                                       
!     * DRAW THE FRAME.                                                 
   20 IF(NY.GE.0) THEN                                                  
         NB=0                                                           
         IF(IABS(ILAB).EQ.1) THEN                                       
            NB=8                                                        
            IF(ILGZ.NE.0) NB=14                                         
         ENDIF                                                          
         TITLE1=TITLE                                                   
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE)                   
	 CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME)           
         IF(NX.LT.0) RETURN                                             
      ELSE                                                              
         CALL OFRAME(MX,MY)                                             
      ENDIF                                                             
                                                                        
                                                                       
!     * PARAMETERS FOR COMMON /CPLCOM/ SHARED WITH SUBROUTINE TRICJ3.   
!     * INT(FX0) AND INT(FY0) ARE THE INTEGER LOCATIONS OF X=0 AND Y=0. 
      XFAC=real(IXR-IXL)/(XR-XL)                                       
      YFAC=real(IYT-IYB)/(YT-YB)                                       
      FX0=real(IXL)-XL*XFAC                                            
      FY0=real(IYB)-YB*YFAC                                            
                                                                       
!     * DETERMINE CONTOUR VALUES AND NUMBER OF CONTOURS.                
      NOC=MIN(26,IABS(NC))                                              
      ICPS = NOC                                                        
      IF (ILAB.LT.0) THEN                                               
        ICPS = -NOC                                                     
      ENDIF                                                             
      IF(NC.LE.0) THEN                                                  
         CALL MINM(Z,NDIM,NNX,NNY,INX,INY,ZMIN,IDUM,JDUM)               
         CALL MAXM(Z,NDIM,NNX,NNY,INX,INY,ZMAX,IDUM,JDUM)               
         IF(ILGZ.EQ.0) THEN                                             
            DELZ=(ZMAX-ZMIN)/NOC                                        
            DO 30 IC=1,NOC                                              
   30          ZC(IC)=ZMIN+(REAL(IC)-.5)*DELZ                           
         ELSE                                                           
            LGMX=ALOG19(ZMAX)                                           
            IF(ZMAX.LT.1.) LGMX=LGMX-1                                  
            LGMN=ALOG19(ZMIN)                                           
            IF(ZMIN.LT.1.) LGMN=LGMN-1                                  
            LGMN=MAX(LGMN,LGMX-25)                                      
            LDEC=LGMX-LGMN+1                                            
            IF(FLGZ) THEN                                               
               IF(ILGZ.EQ.1) LDEC=MIN(LDEC,2)                           
               IF(ILGZ.EQ.2) LDEC=MIN(LDEC,5)                           
               LDEC=MIN(LDEC,8)                                         
               LGMN=LGMX-LDEC+1                                         
            ELSE                                                        
               IF(LDEC.GT.2.AND.ILGZ.LT.2) ILGZ=2                       
               IF(LDEC.GT.5.AND.ILGZ.LT.3) ILGZ=4                       
               IF(LDEC.GT.8) ILGZ=10                                    
            ENDIF                                                       
            IC=0                                                        
            STEP=10.**LGMN                                              
            DO 50 ID=1,LDEC                                             
               DO 40 II=1,9,ILGZ                                        
                  ZCT=REAL(II)*STEP                                     
                  IF((IC.EQ.NOC).OR.(ZCT.GT.ZMAX)) GOTO 60              
                  IC=IC+1                                               
   40             ZC(IC)=ZCT                                            
   50          STEP=STEP*10.                                            
   60       NOC=IC                                                      
         ENDIF                                                          
      ENDIF                                                             
                                                                       
!     * PUT EXTREME PARAMETERS ALONG THE TOP OF THE GRAPH.              
      IF(ABS(ILAB).EQ.1) THEN                                           
         IF(ILGZ.EQ.0) THEN                                             
            WRITE(AMIN,'(''='',1PE9.2)') ZC(1)                          
            WRITE(AMAX,'(''='',1PE9.2)') ZC(NOC)    
            CALL DLCH(IXR-90,IYT+18,' ',N1,1)     
            CALL DLCH(IXR-75,IYT+18,AMIN,10,1)                          
            CALL DLCH(IXR-90,IYT+4,' ',N1+NOC-1,1)                      
            CALL DLCH(IXR-75,IYT+4,AMAX,10,1)                           
         ELSE                                                           
            WRITE(AMIN,'(''='',1PE9.2,'' ILGZ ='',I2)') ZC(1),ILGZ      
            WRITE(AMAX,'(''='',1PE9.2,'' LDEC ='',I2)') ZC(NOC),LDEC    
            CALL DLCH(IXR-170,IYT+18,' ',N1,1)                          
            CALL DLCH(IXR-155,IYT+18,AMIN,19,1)                         
            CALL DLCH(IXR-170,IYT+4,' ',N1+NOC-1,1)                     
            CALL DLCH(IXR-155,IYT+4,AMAX,19,1)                          
         ENDIF                                                          
      ENDIF                                                
                                                                       
!     * DRAW THE CONTOURS BY CALLING TRICJ3 FOR THE TWO TRIANGLES       
!     * WITHIN A MESH OF THE GRID.                                      
      Y1=Y(1)                                                           
      DO 80 J=1+INY,NNY,INY                                             
         IF(INCY.GT.0) HY=Y(J)-Y(J-INY)                                 
         Y2=Y1+HY                                                       
         X1=X(1)                                                        
         DO 70 I=1+INX,NNX,INX                                          
            IF(INCX.GT.0) HX=X(I)-X(I-INX)                              
            X2=X1+HX                                                    
            ZT(1)=Z(I-INX,J-INY)                                        
            ZT(2)=Z(I,J-INY)                                            
            ZT(3)=Z(I,J)                                                
            ZT(4)=Z(I-INX,J)                                            
            IF(ABS(ZT(3)-ZT(1)).GE.ABS(ZT(4)-ZT(2))) THEN               
              CALL TRICJ3(X1,Y1,HX,HY,ICPS,ZC,ZT(2),ZT(1),ZT(4),ICORD)  
              CALL TRICJ3(X2,Y2,-HX,-HY,ICPS,ZC,ZT(4),ZT(3),ZT(2),ICORD)
            ELSE                                                        
              CALL TRICJ3(X2,Y1,-HX,HY,ICPS,ZC,ZT(1),ZT(2),ZT(3),ICORD) 
              CALL TRICJ3(X1,Y2,HX,-HY,ICPS,ZC,ZT(3),ZT(4),ZT(1),ICORD) 
            ENDIF                                                       
            X1=X2                                                       
   70    CONTINUE                                                       
         Y1=Y2                                                          
   80 CONTINUE                                                          
!-------------------------- postscript extension add colorbar            

      IF (ILAB.EQ.-1) THEN                                              
	XBAR  = real(IXR)                                                     
	YBART = real(IYT)                                                     
	YBARB = real(IYB)                                                     
        CALL COLORBAR(ZC,NOC,XBAR,YBART,YBARB)                          
      ENDIF                                                             
                                                                                                                                               
      RETURN                                                            
                                                                       
!     * ENTRY FOR POLAR PLOTS AND LOG10 CONTOURS.                       
      ENTRY CPLOTX(MX,MY,ILAB,X,Y,NX,NY,INCX,INCY,Z,NDIM,ZC,NC,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME,RMAX,IQUAD,LGZ) 
                                                                       
      ILGZ=MIN(IABS(LGZ),4)                                             
      IF(ILGZ.LT.0) FLGZ=.TRUE.                                         
      IF(RMAX.EQ.0.) GOTO 10                                            
                                                                       
      ICORD=1                                                           
      IF(RMAX.LT.0.) ICORD=2                                            
      RMX=ABS(RMAX)                                                     
      NNX=IABS(NX)                                                      
      NNY=IABS(NY)                                                      
      IF(NNY.LE.1) RETURN                                               
      NLAB=IABS(ILAB)                                                   
      INX=IABS(INCX)                                                    
      INY=IABS(INCY)                                                    
      IF(INCX.LT.0) HX=X(2)                                             
      IF(INCY.LT.0) HY=Y(2)                                             
      XMN=0.                                                            
      YMN=0.                                                            
      XMX=RMX                                                           
      YMX=RMX                                                           
      IQUD=MAX(IQUAD,1)                                                 
      IF(IQUD.GT.2.AND.RMAX.LT.0.) IQUD=2                               
      IF(IQUD.EQ.2.) THEN                                               
         XMN=-RMX                                                       
         XMX=RMX                                                        
         YMX=RMX                                                        
      ELSEIF(IQUD.EQ.3.OR.IQUD.EQ.4) THEN                               
         XMN=-RMX                                                       
         YMN=-RMX                                                       
      ENDIF                                                             
      GOTO 20                                                           
                                                                       
      END                                                               

      

SUBROUTINE TRICJ3(XV,YV,DX,DY,NOC,ZC,ZX,ZV,ZY,ICORD)      
!***********************************************************************        
!     THIS SUBROUTINE IS CALLED FROM CPLOT TO DETERMINE THE PARTS OF   *        
! THE CONTOURS THAT LIE WITHIN A TRIANGLE OF THE GRID MESH.  TRICJ3    *        
! FINDS THE INTERSECTIONS OF THE CONTOURS WITH THE TWO SIDES OF THE    *        
! TRIANGLE AND DRAWS LINES BETWEEN THOSE POINTS.                       *        
!     IF NLABEL=1 IN COMMON /CPLCOM/, ALPHABETIC LABELS ARE WRITTEN    *        
! EVERY #(ISKIP+1) CALL OF TRICJ3.  ISKIP IS FIXED IN THE PARAMETER    *        
! STATEMENT BELOW.                                                     *        
!                                                                      *        
!     MODIFIED BY D.W. HEWETT 12-82, FOR THE DIFFERENT COORDINATES     *        
!     X,Y (ICORD=0), R,THETA (ICORD=1), AND  R,COS(THETA) (ICORD=2).   *        
!     ADDED PARAMETER ISKIP, ADDED CHECK ON RANGE Y1/2, HGO 9/12/85.   *        
!***********************************************************************      
      PARAMETER (ISKIP=9)   
       
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
      COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C      
      DIMENSION ZC(*)       
      DIMENSION X(3),Y(3),Z(3),XP(3),YP(3)       
      
      PI=3.1415926535898  
      TPI=2.*PI  
      N1=N1C     
      IX=1     
      ICPS=0
      IF (NOC.LE.0) ICPS=1
      NOC2 = ABS(NOC)
      IF(ZV.LT.ZX) THEN   
         IX=2    
         IF(ZY.LT.ZX) IX=3         
         IV=1    
         IY=5-IX 
         IF(ZY.LE.ZV) THEN         
           IV=5-IX       
           IY=1 
         ENDIF   
      ELSE       
         IF(ZY.LT.ZX) IX=2         
         IV=3-IX 
         IY=3    
         IF(ZY.LE.ZV) THEN         
           IV=3 
           IY=3-IX       
         ENDIF   
      ENDIF      
       
      X(IX)=XV+DX         
      X(IV)=XV   
      X(IY)=XV   
      Y(IX)=YV   
      Y(IV)=YV   
      Y(IY)=YV+DY         
      Z(IX)=ZX   
      Z(IV)=ZV   
      Z(IY)=ZY        
!-----------------------------------------------------------------------
! Postscript extension using gradient fill, Guido Huysmans 15/11/2000
!-----------------------------------------------------------------------
      IF (ICPS.EQ.1) THEN
        DO I=1,3
          XP(I) = X(I) 
	  YP(I) = Y(I)
    	  IF(ICORD.NE.0) THEN       
            FLP1=1.   
            IF(ICORD.EQ.1) THEN    
              IF(YP(I).GT.PI.AND.YP(I).LT.TPI) FLP1=-1. 
              YP(I)=COS(YP(I))   
            ENDIF         
            YP(I)=AMIN1(AMAX1(-1.,YP(I)),1.)   
            TX1=YP(I)*XP(I)       
            YP(I)=FLP1*XP(I)*SQRT(1.-YP(I)*YP(I))
            XP(I)=TX1         
          ENDIF       
	  XP(I) = FX0 + XP(I)* XFAC
	  YP(I) = FY0 + YP(I)* YFAC
        ENDDO
        CALL FILLTRIA(XP,YP,Z,ZC(1),ZC(NOC2))
        RETURN
      ENDIF
!-----------------------------------------------------------------------
      
      IF(Z(1).EQ.Z(3)) RETURN      
       
      DO 10 IC=1,NOC2      
         IF(ZC(IC).LT.Z(1)) GOTO 10         
         IF(ZC(IC).GT.Z(3)) GOTO 20         
         FRAC=(ZC(IC)-Z(1))/(Z(3)-Z(1))     
         X1=X(1)+(X(3)-X(1))*FRAC  
         Y1=Y(1)+(Y(3)-Y(1))*FRAC  
         IF(ZC(IC).LE.Z(2).AND.Z(1).NE.Z(2)) THEN    
           FRAC=(ZC(IC)-Z(1))/(Z(2)-Z(1))  
           X2=X(1)+FRAC*(X(2)-X(1)) 
           Y2=Y(1)+FRAC*(Y(2)-Y(1)) 
         ELSE    
           FRAC=(ZC(IC)-Z(2))/(Z(3)-Z(2))  
           X2=X(2)+FRAC*(X(3)-X(2)) 
           Y2=Y(2)+FRAC*(Y(3)-Y(2)) 
         ENDIF   
         IF(ICORD.NE.0) THEN       
           FLP1=1.       
           FLP2=1.       
           IF(ICORD.EQ.1) THEN    
             IF(Y1.GT.PI.AND.Y1.LT.TPI) FLP1=-1.   
             IF(Y2.GT.PI.AND.Y2.LT.TPI) FLP2=-1.   
             Y1=COS(Y1)   
             Y2=COS(Y2)   
           ENDIF         
           Y1=AMIN1(AMAX1(-1.,Y1),1.)      
           Y2=AMIN1(AMAX1(-1.,Y2),1.)      
           TX1=Y1*X1     
           Y1=FLP1*X1*SQRT(1.-Y1*Y1)
           X1=TX1        
           TX2=Y2*X2     
           Y2=FLP2*X2*SQRT(1.-Y2*Y2)
           X2=TX2        
         ENDIF   
         ZIX1=FX0+X1*XFAC 
         ZIY1=FY0+Y1*YFAC 
         ZIX2=FX0+X2*XFAC 
         ZIY2=FY0+Y2*YFAC 
         CALL ppp_set_color(0)
	 CALL DRV(ZIX1,ZIY1,ZIX2,ZIY2)      
         ISYM(IC)=ISYM(IC)+NLAB    
         IF(ISYM(IC).GE.1) THEN    
           ICC=IC+N1-1   
           IDX=8         
           IDY=0         
           IF(ABS(ZIX2-ZIX1).GE.ABS(ZIY2-ZIY1)) THEN         
             IDX=0      
             IDY=8      
           ENDIF         
           IX11=MIN(MAX(IXL+5,INT(ZIX1)+IDX),IXR-5) 
           IY11=MIN(MAX(IYB+5,INT(ZIY1)+IDY),IYT-5) 
           CALL DLCH(IX11,-IY11,' ',ICC,1) 
           ISYM(IC)=-ISKIP        
         ENDIF   
   10 CONTINUE   
       
   20 RETURN     

END  


SUBROUTINE CPLOTM(MX,MY,ILAB1,X,Y,NX,NY,INCX,INCY,Z,NDIM,ZC,NC,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)         
!***********************************************************************        
! SUBROUTINE CPLOTM FOR CONTOUR PLOTS ON NON-EQUIDISTANT GRIDS IN X,Y  *
!     see CPLOT except X and Y are 2-D arrays with the position at     *
!     every grid point of Z.                                           *
!     Positions of contour are calculated with a linear interpolation  *
!     of X and Y.                                                      *
!     ILAB=10 or 11  : plot the irregular grid                         *
! ENTRY CPLOTXM uses the (r,theta)  coordinate system                  *
!                                                                      *
! Guido Huysmans 21-7-99                                               *
!***********************************************************************        
PARAMETER (N1=97)     
       
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C      
DIMENSION X(NDIM,*),Y(NDIM,*),Z(NDIM,*),ZC(*)   
CHARACTER*(*) TITLE,XNAME,YNAME       
DIMENSION ZT(4),XZ(4),YZ(4),XP(NDIM),YP(NDIM)       
CHARACTER*19 AMIN,AMAX       
CHARACTER*80 TITLE1 
LOGICAL FLGZ        
       
!     * INITIALIZE N1 AND ISYM FOR USE IN TRICJ3.    
N1C=N1       
ILAB  = MOD(ILAB1,10)     

DO I=1,26         
  ISYM(I)=0        
ENDDO
      
!     * INITIALIZE FOR SCALAR CONTOURS.     

ILGZ=0     

!     * FLAG DOWN FOR OVERRIDING THE AUTOMATIC DETERMINATION OF THE    
!     * NUMBER OF CONTOURS PER DECADE IN THE CASE OF LOG10 CONTOURS.   

FLGZ=.FALSE.        
       
!     * INPUT PARAMETERS. 
   10 ICORD=0    
      RMX=0.     
      NNX=IABS(NX) 
      NNY=IABS(NY) 
      IF(NNY.LE.1) RETURN 
      NLAB=ILAB  
      INX=IABS(INCX)        
      INY=IABS(INCY)        
      XMN = 1.e20
      XMX = -XMN
      YMN =  XMN
      YMX =  XMX       
      DO I=1,NX,INX
        DO J=1,NY,INY
	  IF (X(I,J).GT.XMX) XMX=X(I,J)
	  IF (X(I,J).LT.XMN) XMN=X(I,J)
	  IF (Y(I,J).GT.YMX) YMX=Y(I,J)
	  IF (Y(I,J).LT.YMN) YMN=Y(I,J)
        ENDDO
      ENDDO
       
!     * DRAW THE FRAME.   
   20 IF(NY.GE.0) THEN    
         NB=0    
         IF(ILAB.EQ.1) THEN        
           NB=8 
           IF(ILGZ.NE.0) NB=14    
         ENDIF   
         TITLE1=TITLE     
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE) 
         CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME) 
         IF(NX.LT.0) RETURN        
      ELSE       
         CALL OFRAME(MX,MY) 
      ENDIF      

!     PLOT GRID

      IF (ILAB1.GT.9) THEN
        DO I=1,NY
          IF (ICORD.EQ.0) THEN
            DO J=1,NX
              XP(J) = X(J,I)  
              YP(J) = Y(J,I)
            ENDDO
	  ELSE
            DO J=1,NX
              XP(J) = X(J,I)*COS(Y(J,I))
	      YP(J) = X(J,I)*SIN(Y(J,I))
	    ENDDO
	  ENDIF
          CALL DPLOT(MX,MY,XP,YP,NX,1,2,8)
        ENDDO
        DO I=1,NX
          IF (ICORD.EQ.0) THEN
            DO J=1,NY
              XP(J) = X(I,J) 
              YP(J) = Y(I,J)
	    ENDDO
	  ELSE 
            DO J=1,NY
              XP(J) = X(I,J)*COS(Y(I,J))
	      YP(J) = X(I,J)*SIN(Y(I,J))
	    ENDDO
	  ENDIF
          CALL DPLOT(MX,MY,XP,YP,NY,1,2,8)
        ENDDO   
      ENDIF  
       
!     * PARAMETERS FOR COMMON /CPLCOM/ SHARED WITH SUBROUTINE TRICJ3.  
!     * INT(FX0) AND INT(FY0) ARE THE INTEGER LOCATIONS OF X=0 AND Y=0.         
      XFAC=real(IXR-IXL)/(XR-XL)   
      YFAC=real(IYT-IYB)/(YT-YB)   
      FX0=real(IXL)-XL*XFAC      
      FY0=real(IYB)-YB*YFAC    
!       
!     * DETERMINE CONTOUR VALUES AND NUMBER OF CONTOURS.      
      NOC=MIN(26,IABS(NC))    
      ICPS = NOC
      IF (ILAB.LT.0) THEN
        ICPS = -NOC
      ENDIF        
      IF(NC.LE.0) THEN    
         CALL MINM(Z,NDIM,NNX,NNY,INX,INY,ZMIN,IDUM,JDUM)     
         CALL MAXM(Z,NDIM,NNX,NNY,INX,INY,ZMAX,IDUM,JDUM)     
         IF(ILGZ.EQ.0) THEN        
           DELZ=(ZMAX-ZMIN)/NOC   
           DO IC=1,NOC         
             ZC(IC)=ZMIN+(REAL(IC)-.5)*DELZ
           ENDDO
         ELSE    
           LGMX=ALOG19(ZMAX)        
           IF(ZMAX.LT.1.) LGMX=LGMX-1      
           LGMN=ALOG19(ZMIN)        
           IF(ZMIN.LT.1.) LGMN=LGMN-1      
           LGMN=MAX(LGMN,LGMX-25)   
           LDEC=LGMX-LGMN+1       
           IF(FLGZ) THEN 
             IF(ILGZ.EQ.1) LDEC=MIN(LDEC,2)        
             IF(ILGZ.EQ.2) LDEC=MIN(LDEC,5)        
             LDEC=MIN(LDEC,8)      
             LGMN=LGMX-LDEC+1    
           ELSE 
             IF(LDEC.GT.2.AND.ILGZ.LT.2) ILGZ=2    
             IF(LDEC.GT.5.AND.ILGZ.LT.3) ILGZ=4    
             IF(LDEC.GT.8) ILGZ=10        
           ENDIF         
           IC=0 
           STEP=10.**LGMN         
           DO ID=1,LDEC        
             DO II=1,9,ILGZ   
               ZCT=REAL(II)*STEP         
               IF((IC.EQ.NOC).OR.(ZCT.GT.ZMAX)) EXIT    
               IC=IC+1 
               ZC(IC)=ZCT
             ENDDO
             STEP=STEP*10.
           ENDDO
           NOC=IC        
         ENDIF   
      ENDIF      
!       
!     * PUT EXTREME PARAMETERS ALONG THE TOP OF THE GRAPH.    
      IF(ILAB.EQ.1) THEN  
         IF(ILGZ.EQ.0) THEN        
           WRITE(AMIN,'(''='',1PE9.2)') ZC(1)       
           WRITE(AMAX,'(''='',1PE9.2)') ZC(NOC)     
           CALL DLCH(IXR-90,IYT+18,' ',N1,1)        
           CALL DLCH(IXR-75,IYT+18,AMIN,10,1)       
           CALL DLCH(IXR-90,IYT+4,' ',N1+NOC-1,1)   
           CALL DLCH(IXR-75,IYT+4,AMAX,10,1)        
         ELSE    
           WRITE(AMIN,'(''='',1PE9.2,'' ILGZ ='',I2)') ZC(1),ILGZ     
           WRITE(AMAX,'(''='',1PE9.2,'' LDEC ='',I2)') ZC(NOC),LDEC   
           CALL DLCH(IXR-170,IYT+18,' ',N1,1)       
           CALL DLCH(IXR-155,IYT+18,AMIN,19,1)      
           CALL DLCH(IXR-170,IYT+4,' ',N1+NOC-1,1)  
           CALL DLCH(IXR-155,IYT+4,AMAX,19,1)       
         ENDIF   
      ENDIF      
!       
!     * DRAW THE CONTOURS BY CALLING TRICJ3 FOR THE TWO TRIANGLES      
!     * WITHIN A MESH OF THE GRID. 
      DO J=1+INY,NNY,INY        
         DO I=1+INX,NNX,INX     
           ZT(1)=Z(I-INX,J-INY)     
           ZT(2)=Z(I,J-INY)         
           ZT(3)=Z(I,J)    
           ZT(4)=Z(I-INX,J)     
	   XZ(1)=X(I-INX,J-INY)     
           XZ(2)=X(I,J-INY)         
           XZ(3)=X(I,J)    
           XZ(4)=X(I-INX,J)
	   YZ(1)=Y(I-INX,J-INY)     
           YZ(2)=Y(I,J-INY)         
           YZ(3)=Y(I,J)    
           YZ(4)=Y(I-INX,J)  
           IF(ABS(ZT(3)-ZT(1)).GE.ABS(ZT(4)-ZT(2))) THEN     
             CALL TRICJ3M(XZ(2),YZ(2),XZ(1),YZ(1),XZ(4),YZ(4),ICPS,ZC,ZT(2),ZT(1),ZT(4),ICORD) 
             CALL TRICJ3M(XZ(4),YZ(4),XZ(3),YZ(3),XZ(2),YZ(2),ICPS,ZC,ZT(4),ZT(3),ZT(2),ICORD)        
           ELSE 
             CALL TRICJ3M(XZ(1),YZ(1),XZ(2),YZ(2),XZ(3),YZ(3),ICPS,ZC,ZT(1),ZT(2),ZT(3),ICORD)         
             CALL TRICJ3M(XZ(3),YZ(3),XZ(4),YZ(4),XZ(1),YZ(1),ICPS,ZC,ZT(3),ZT(4),ZT(1),ICORD)         
           ENDIF         
         ENDDO         
      ENDDO   
! 
!-------------------------- postscript extension add colorbar      
      IF (ILAB.EQ.-1) THEN
	XBAR  = real(IXR)
	YBART = real(IYT)
	YBARB = real(IYB)
        CALL COLORBAR(ZC,NOC,XBAR,YBART,YBARB)
      ENDIF
      RETURN     
       
!     * ENTRY FOR POLAR PLOTS AND LOG10 CONTOURS.    
      ENTRY CPLOTXM(MX,MY,ILAB1,X,Y,NX,NY,INCX,INCY,Z,NDIM,ZC,NC,    &    
                    TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME,RMAX,IQUAD,LGZ)   
       
      IPLGR = ISIGN(ILAB1,1)
      ILAB  = IABS(ILAB1)         
      ILGZ=MIN(IABS(LGZ),4) 
      IF(ILGZ.LT.0) FLGZ=.TRUE.    
      IF(RMAX.EQ.0.) GOTO 10       
       
      ICORD=1    
      IF(RMAX.LT.0.) ICORD=2       
      RMX=ABS(RMAX)         
      NNX=IABS(NX) 
      NNY=IABS(NY) 
      IF(NNY.LE.1) RETURN 
      NLAB=ILAB  
      INX=IABS(INCX)        
      INY=IABS(INCY)        
      XMN = 1.e20
      XMX = -XMN
      YMN =  XMN
      YMX =  XMX       
      DO I=1,NX,INX
        DO J=1,NY,INY
	  XTT = X(I,J)*COS(Y(I,J))
	  YTT = X(I,J)*SIN(Y(I,J))
	  IF (XTT.GT.XMX) XMX=XTT
	  IF (XTT.LT.XMN) XMN=XTT
	  IF (YTT.GT.YMX) YMX=YTT
	  IF (YTT.LT.YMN) YMN=YTT
        ENDDO
      ENDDO
      IQUD=MAX(IQUAD,1)     
      IF (IQUD.GT.2.AND.RMAX.LT.0.) IQUD=2    
      IF (IQUD.EQ.1) XMN=0.        
      IF (IQUD.LE.2) YMN=0.  
      GOTO 20    
       
END        

      
SUBROUTINE CPLOTFE(MX,MY,ILAB1,X,Y,Z,NX,INC,ZC,NC,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)         
!***********************************************************************        
! SUBROUTINE CPLOTFE FOR CONTOUR PLOTS ON IRREGULAR GRID IN X,Y        *
!     see CPLOT except X and Y and Z are given as a set of 'squares'   *
!     X(i,1:4),Y(i,1:4),Z(i,1:4)                                       *
!     Positions of contour are calculated with a linear interpolation  *
!     of X and Y.                                                      *
!     ILAB=10 or 11  : plot the irregular grid                         *
! ENTRY CPLOTXM uses the (r,theta)  coordinate system                  *
!                                                                      *
! Guido Huysmans 21-7-99                                               *
!***********************************************************************        
PARAMETER (N1=97)     
       
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C      
DIMENSION X(4,*),Y(4,*),Z(4,*),ZC(*)   
CHARACTER*(*) TITLE,XNAME,YNAME       
DIMENSION ZT(4),XZ(4),YZ(4),XP(5),YP(5)       
CHARACTER*19 AMIN,AMAX       
CHARACTER*80 TITLE1 
LOGICAL FLGZ        
 
!     * INITIALIZE N1 AND ISYM FOR USE IN TRICJ3.    
N1C=N1       
ILAB  = MOD(ILAB1,10)     
DO I=1,26         
   ISYM(I)=0        
ENDDO   

!     * INITIALIZE FOR SCALAR CONTOURS.     

ILGZ=0     

!     * FLAG DOWN FOR OVERRIDING THE AUTOMATIC DETERMINATION OF THE    
!     * NUMBER OF CONTOURS PER DECADE IN THE CASE OF LOG10 CONTOURS.   

FLGZ=.FALSE.        

!       
!     * INPUT PARAMETERS. 
   10 ICORD=0    
      RMX=0.     
      NNX=IABS(NX) 
      NLAB=ILAB  
      INX=IABS(INC)        
      XMN = 1.e20
      XMX = -XMN
      YMN =  XMN
      YMX =  XMX       
      DO I=1,NNX,INC
        DO J=1,4
	  IF (X(J,I).GT.XMX) XMX=X(J,I)
	  IF (X(J,I).LT.XMN) XMN=X(J,I)
	  IF (Y(J,I).GT.YMX) YMX=Y(J,I)
	  IF (Y(J,I).LT.YMN) YMN=Y(J,I)
        ENDDO
      ENDDO
!       
!     * DRAW THE FRAME.   
      IF(NX.GE.0) THEN    
         NB=0    
         IF(ILAB.EQ.1) THEN        
           NB=8 
           IF(ILGZ.NE.0) NB=14    
         ENDIF   
         TITLE1=TITLE     
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE) 
         CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME) 
      ELSE       
         CALL OFRAME(MX,MY) 
      ENDIF      
      NX = ABS(NX)

!     PLOT GRID

      IF (ILAB1.GT.9) THEN
        DO I=1,NX
          DO J=1,5
	    J1 = MOD(J-1,4) + 1
            XP(J) = X(J1,I)  
            YP(J) = Y(J1,I)
          ENDDO
	  CALL DPLOT(MX,MY,XP,YP,4,1,2,8)	 
	ENDDO
      ENDIF  
       
!     * PARAMETERS FOR COMMON /CPLCOM/ SHARED WITH SUBROUTINE TRICJ3.  
!     * INT(FX0) AND INT(FY0) ARE THE INTEGER LOCATIONS OF X=0 AND Y=0.         
      XFAC=real(IXR-IXL)/(XR-XL)   
      YFAC=real(IYT-IYB)/(YT-YB)   
      FX0=real(IXL)-XL*XFAC      
      FY0=real(IYB)-YB*YFAC    
       
!     * DETERMINE CONTOUR VALUES AND NUMBER OF CONTOURS.      
!     * DETERMINE CONTOUR VALUES AND NUMBER OF CONTOURS.      
      NOC=MIN(26,IABS(NC))    
      ICPS = NOC
      IF (ILAB.LT.0) THEN
        ICPS = -NOC
      ENDIF    
      IF(NC.LE.0) THEN    
         CALL MINM(Z,4,4,NX,1,INC,ZMIN,IDUM,JDUM)     
         CALL MAXM(Z,4,4,NX,1,INC,ZMAX,IDUM,JDUM)     
         IF(ILGZ.EQ.0) THEN        
           DELZ=(ZMAX-ZMIN)/NOC   
           DO IC=1,NOC         
             ZC(IC)=ZMIN+(REAL(IC)-.5)*DELZ     
           ENDDO
         ELSE    
           LGMX=ALOG19(ZMAX)        
           IF(ZMAX.LT.1.) LGMX=LGMX-1      
           LGMN=ALOG19(ZMIN)        
           IF(ZMIN.LT.1.) LGMN=LGMN-1      
           LGMN=MAX(LGMN,LGMX-25)   
           LDEC=LGMX-LGMN+1       
           IF(FLGZ) THEN 
             IF(ILGZ.EQ.1) LDEC=MIN(LDEC,2)        
             IF(ILGZ.EQ.2) LDEC=MIN(LDEC,5)        
             LDEC=MIN(LDEC,8)      
             LGMN=LGMX-LDEC+1    
           ELSE 
             IF(LDEC.GT.2.AND.ILGZ.LT.2) ILGZ=2    
             IF(LDEC.GT.5.AND.ILGZ.LT.3) ILGZ=4    
             IF(LDEC.GT.8) ILGZ=10        
           ENDIF         
           IC=0 
           STEP=10.**LGMN         
           DO ID=1,LDEC        
             DO II=1,9,ILGZ   
               ZCT=REAL(II)*STEP         
               IF((IC.EQ.NOC).OR.(ZCT.GT.ZMAX)) EXIT    
               IC=IC+1 
               ZC(IC)=ZCT   
             ENDDO
             STEP=STEP*10.
           ENDDO
           NOC=IC        
         ENDIF   
      ENDIF  
       
!     * PUT EXTREME PARAMETERS ALONG THE TOP OF THE GRAPH.    
      IF(ILAB.EQ.1) THEN  
         IF(ILGZ.EQ.0) THEN        
           WRITE(AMIN,'(''='',1PE9.2)') ZC(1)       
           WRITE(AMAX,'(''='',1PE9.2)') ZC(NOC)     
           CALL DLCH(IXR-90,IYT+18,' ',N1,1)        
           CALL DLCH(IXR-75,IYT+18,AMIN,10,1)       
           CALL DLCH(IXR-90,IYT+4,' ',N1+NOC-1,1)   
           CALL DLCH(IXR-75,IYT+4,AMAX,10,1)        
         ELSE    
           WRITE(AMIN,'(''='',1PE9.2,'' ILGZ ='',I2)') ZC(1),ILGZ     
           WRITE(AMAX,'(''='',1PE9.2,'' LDEC ='',I2)') ZC(NOC),LDEC   
           CALL DLCH(IXR-170,IYT+18,' ',N1,1)       
           CALL DLCH(IXR-155,IYT+18,AMIN,19,1)      
           CALL DLCH(IXR-170,IYT+4,' ',N1+NOC-1,1)  
           CALL DLCH(IXR-155,IYT+4,AMAX,19,1)       
         ENDIF   
      ENDIF      
       
!     * DRAW THE CONTOURS BY CALLING TRICJ3 FOR THE TWO TRIANGLES      
!     * WITHIN A MESH OF THE GRID. 
      DO I=1,NX,INC       
         DO J=1,4     
            ZT(J)=Z(J,I)     
            XZ(J)=X(J,I)     
	    YZ(J)=Y(J,I)      
	 ENDDO	  
         IF(ABS(ZT(3)-ZT(1)).GE.ABS(ZT(4)-ZT(2))) THEN     
           CALL TRICJ3M(XZ(2),YZ(2),XZ(1),YZ(1),XZ(4),YZ(4),ICPS,ZC,ZT(2),ZT(1),ZT(4),ICORD) 
           CALL TRICJ3M(XZ(4),YZ(4),XZ(3),YZ(3),XZ(2),YZ(2),ICPS,ZC,ZT(4),ZT(3),ZT(2),ICORD)        
         ELSE 
           CALL TRICJ3M(XZ(1),YZ(1),XZ(2),YZ(2),XZ(3),YZ(3),ICPS,ZC,ZT(1),ZT(2),ZT(3),ICORD)         
           CALL TRICJ3M(XZ(3),YZ(3),XZ(4),YZ(4),XZ(1),YZ(1),ICPS,ZC,ZT(3),ZT(4),ZT(1),ICORD)         
         ENDIF         
   70 ENDDO 
      CALL ppp_set_color(0)
!-------------------------- postscript extension add colorbar      
      IF (ICPS.LT.0) THEN
	XBAR  = real(IXR)
	YBART = real(IYT)
	YBARB = real(IYB)
        CALL COLORBAR(ZC,NOC,XBAR,YBART,YBARB)
      ENDIF
       
RETURN     
END

SUBROUTINE CPLOT_TRIA(MX,MY,ILAB1,X,Y,N_NODES,TRIA,N_TRIA,Z,ZC,NC,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)         
!***********************************************************************        
! SUBROUTINE CPLOTFE FOR CONTOUR PLOTS ON A TRIANGLE GRID IN X,Y       *
!     see CPLOT except X and Y and Z are given as a set of 'squares'   *
!     X(:),Y(:),Z(:), TRIA(3,:)                                       *
!     Positions of contour are calculated with a linear interpolation  *
!     of X and Y.                                                      *
!     ILAB=10 or 11  : plot the irregular grid                         *
! ENTRY CPLOTXM uses the (r,theta)  coordinate system                  *
!                                                                      *
! Guido Huysmans 17-2-16                                               *
!***********************************************************************        
PARAMETER (N1=97)     
       
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C
COMMON /KPOS/KP(36)   
DIMENSION X(*),Y(*),Z(*),ZC(*)
INTEGER TRIA(3,*), N_TRIA, N_NODES
CHARACTER*(*) TITLE,XNAME,YNAME       
DIMENSION ZT(3),XZ(3),YZ(3),XP(4),YP(4)       
CHARACTER*19 AMIN,AMAX       
CHARACTER*80 TITLE1 
LOGICAL FLGZ        
 
INC = 1

IMX=MOD(IABS(MX),10)  
IMY=MOD(IABS(MY),10)
M_FRAME=IMY+6*IMX-6

!     * INITIALIZE N1 AND ISYM FOR USE IN TRICJ3.    
N1C=N1       
ILAB  = MOD(ILAB1,10)     
DO I=1,26         
   ISYM(I)=0        
ENDDO   

!     * INITIALIZE FOR SCALAR CONTOURS.     

ILGZ=0     

!     * FLAG DOWN FOR OVERRIDING THE AUTOMATIC DETERMINATION OF THE    
!     * NUMBER OF CONTOURS PER DECADE IN THE CASE OF LOG10 CONTOURS.   

FLGZ=.FALSE.        

!       
!     * INPUT PARAMETERS. 
   10 ICORD=0    
      RMX=0.     
      NNX=IABS(N_NODES) 
      NLAB=ILAB  
      INX=IABS(INC)
      XMX = MAXVAL(X(1:NNX))
      XMN = MINVAL(X(1:NNX))
      YMX = MAXVAL(Y(1:NNX))       
      YMN = MINVAL(Y(1:NNX))
!       
!     * DRAW THE FRAME.   
      IF(N_NODES.GE.0) THEN    
         NB=0    
         IF(ILAB.EQ.1) THEN        
           NB=8 
           IF(ILGZ.NE.0) NB=14    
         ENDIF   
         TITLE1=TITLE     
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE) 
         CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME) 
      ELSE       
         CALL OFRAME(MX,MY) 
      ENDIF      

!     PLOT GRID

      IF (ILAB1.GT.9) THEN
        DO I=1,N_TRIA
          DO J=1,4
	    J1 = MOD(J-1,3) + 1
            XP(J) = X(TRIA(J1,I))  
            YP(J) = Y(TRIA(J1,I))
          ENDDO
	  CALL DPLOT(MX,MY,XP,YP,4,1,2,8)	 
	ENDDO
      ENDIF  
       
!     * PARAMETERS FOR COMMON /CPLCOM/ SHARED WITH SUBROUTINE TRICJ3.  
!     * INT(FX0) AND INT(FY0) ARE THE INTEGER LOCATIONS OF X=0 AND Y=0.         
      XFAC = real(IXR-IXL)/(XR-XL)   
      YFAC = real(IYT-IYB)/(YT-YB)   
      FX0  = real(IXL)-XL*XFAC      
      FY0  = real(IYB)-YB*YFAC    
       
!     * DETERMINE CONTOUR VALUES AND NUMBER OF CONTOURS.      
!     * DETERMINE CONTOUR VALUES AND NUMBER OF CONTOURS.      
      NOC=MIN(26,IABS(NC))    
      ICPS = NOC
      IF (ILAB.LT.0) THEN
        ICPS = -NOC
      ENDIF    
      IF(NC.LE.0) THEN 
         ZMIN = MINVAL(Z(1:NNX))
	 ZMAX = MAXVAL(Z(1:NNX))
         IF(ILGZ.EQ.0) THEN        
           DELZ=(ZMAX-ZMIN)/NOC   
           DO IC=1,NOC         
             ZC(IC)=ZMIN+(REAL(IC)-.5)*DELZ     
           ENDDO
         ELSE    
           LGMX=ALOG19(ZMAX)        
           IF(ZMAX.LT.1.) LGMX=LGMX-1      
           LGMN=ALOG19(ZMIN)        
           IF(ZMIN.LT.1.) LGMN=LGMN-1      
           LGMN=MAX(LGMN,LGMX-25)   
           LDEC=LGMX-LGMN+1       
           IF(FLGZ) THEN 
             IF(ILGZ.EQ.1) LDEC=MIN(LDEC,2)        
             IF(ILGZ.EQ.2) LDEC=MIN(LDEC,5)        
             LDEC=MIN(LDEC,8)      
             LGMN=LGMX-LDEC+1    
           ELSE 
             IF(LDEC.GT.2.AND.ILGZ.LT.2) ILGZ=2    
             IF(LDEC.GT.5.AND.ILGZ.LT.3) ILGZ=4    
             IF(LDEC.GT.8) ILGZ=10        
           ENDIF         
           IC=0 
           STEP=10.**LGMN         
           DO ID=1,LDEC        
             DO II=1,9,ILGZ   
               ZCT=REAL(II)*STEP         
               IF((IC.EQ.NOC).OR.(ZCT.GT.ZMAX)) EXIT    
               IC=IC+1 
               ZC(IC)=ZCT   
             ENDDO
             STEP=STEP*10.
           ENDDO
           NOC=IC        
         ENDIF   
      ENDIF  
       
!     * PUT EXTREME PARAMETERS ALONG THE TOP OF THE GRAPH.    
      IF(ILAB.EQ.1) THEN  
         IF(ILGZ.EQ.0) THEN        
           WRITE(AMIN,'(''='',1PE9.2)') ZC(1)       
           WRITE(AMAX,'(''='',1PE9.2)') ZC(NOC)     
           CALL DLCH(IXR-90,IYT+18,' ',N1,1)        
           CALL DLCH(IXR-75,IYT+18,AMIN,10,1)       
           CALL DLCH(IXR-90,IYT+4,' ',N1+NOC-1,1)   
           CALL DLCH(IXR-75,IYT+4,AMAX,10,1)        
         ELSE    
           WRITE(AMIN,'(''='',1PE9.2,'' ILGZ ='',I2)') ZC(1),ILGZ     
           WRITE(AMAX,'(''='',1PE9.2,'' LDEC ='',I2)') ZC(NOC),LDEC   
           CALL DLCH(IXR-170,IYT+18,' ',N1,1)       
           CALL DLCH(IXR-155,IYT+18,AMIN,19,1)      
           CALL DLCH(IXR-170,IYT+4,' ',N1+NOC-1,1)  
           CALL DLCH(IXR-155,IYT+4,AMAX,19,1)       
         ENDIF   
      ENDIF      
       
!     * DRAW THE CONTOURS BY CALLING TRICJ3 FOR THE TWO TRIANGLES      
!     * WITHIN A MESH OF THE GRID. 
      DO I=1,N_TRIA      
         DO J=1,3     
            ZT(J)=Z(TRIA(J,I))     
            XZ(J)=X(TRIA(J,I))     
	    YZ(J)=Y(TRIA(J,I))      
	 ENDDO	  
         CALL TRICJ3M(XZ(1),YZ(1),XZ(2),YZ(2),XZ(3),YZ(3),ICPS,ZC,ZT(1),ZT(2),ZT(3),ICORD)         
   70 ENDDO 
      CALL ppp_set_color(0)
!-------------------------- postscript extension add colorbar      
      IF (ICPS.LT.0) THEN
	XBAR  = real(IXR)
	YBART = real(IYT)
	YBARB = real(IYB)
        CALL COLORBAR(ZC,NOC,XBAR,YBART,YBARB)
      ENDIF
      IF(N_NODES.GE.0) THEN    
         NB=0    
         IF(ILAB.EQ.1) THEN        
           NB=8 
           IF(ILGZ.NE.0) NB=14    
         ENDIF   
         TITLE1=TITLE     
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE) 
!--------------------------- prevent page advance
	 KP(M_FRAME)=0   
         CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME) 
      ELSE       
         CALL OFRAME(MX,MY) 
      ENDIF      

RETURN     
END       
	 
       
SUBROUTINE TRICJ3M(XX,YX,XV,YV,XY,YY,NOC,ZC,ZX,ZV,ZY,ICORD)        
!***********************************************************************        
!     THIS SUBROUTINE IS CALLED FROM CPLOT TO DETERMINE THE PARTS OF   *        
! THE CONTOURS THAT LIE WITHIN A TRIANGLE OF THE GRID MESH.  TRICJ3    *        
! FINDS THE INTERSECTIONS OF THE CONTOURS WITH THE TWO SIDES OF THE    *        
! TRIANGLE AND DRAWS LINES BETWEEN THOSE POINTS.                       *        
!     IF NLABEL=1 IN COMMON /CPLCOM/, ALPHABETIC LABELS ARE WRITTEN    *        
! EVERY #(ISKIP+1) CALL OF TRICJ3.  ISKIP IS FIXED IN THE PARAMETER    *        
! STATEMENT BELOW.                                                     *         
!                                                                      *        
!     MODIFIED BY D.W. HEWETT 12-82, FOR THE DIFFERENT COORDINATES     *        
!     X,Y (ICORD=0), R,THETA (ICORD=1), AND  R,COS(THETA) (ICORD=2).   *        
!     ADDED PARAMETER ISKIP, ADDED CHECK ON RANGE Y1/2, HGO 9/12/85.   *        
!***********************************************************************      
PARAMETER (ISKIP=9)   
       
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
COMMON /CPLCOM/XFAC,YFAC,FX0,FY0,ISYM(26),NLAB,N1C      
DIMENSION ZC(*)       
DIMENSION X(3),Y(3),Z(3),XP(3),YP(3)        
  
ICPS=0
IF (NOC.LE.0) ICPS=1
  NOC2 = ABS(NOC)
  N1=N1C     
  IX=1       
  IF(ZV.LT.ZX) THEN   
    IX=2    
    IF(ZY.LT.ZX) IX=3         
      IV=1    
      IY=5-IX 
      IF(ZY.LE.ZV) THEN         
        IV=5-IX       
        IY=1 
      ENDIF   
    ELSE       
      IF(ZY.LT.ZX) IX=2         
        IV=3-IX 
        IY=3    
        IF(ZY.LE.ZV) THEN         
          IV=3 
          IY=3-IX       
        ENDIF   
      ENDIF      
       
      X(IX)=XX        
      X(IV)=XV   
      X(IY)=XY   
      Y(IX)=YX   
      Y(IV)=YV   
      Y(IY)=YY         
      Z(IX)=ZX   
      Z(IV)=ZV   
      Z(IY)=ZY
      
!-----------------------------------------------------------------------
! Postscript extension using gradient fill, Guido Huysmans 15/11/2000
!-----------------------------------------------------------------------
      IF (ICPS.EQ.1) THEN
        DO I=1,3
           XP(I) = X(I) 
	   YP(I) = Y(I)
    	   IF(ICORD.NE.0) THEN       
             FLP1=1.   
             IF(ICORD.EQ.1) THEN    
               IF(YP(I).GT.PI.AND.YP(I).LT.TPI) FLP1=-1. 
               YP(I)=COS(YP(I))   
             ENDIF         
             YP(I)=AMIN1(AMAX1(-1.,YP(I)),1.)   
             TX1=YP(I)*XP(I)       
             YP(I)=FLP1*XP(I)*SQRT(1.-YP(I)*YP(I))
             XP(I)=TX1         
           ENDIF       
	   XP(I) = FX0 + XP(I)* XFAC
	   YP(I) = FY0 + YP(I)* YFAC
        ENDDO
        CALL FILLTRIA(XP,YP,Z,ZC(1),ZC(NOC2))
        RETURN
      ENDIF

      IF(Z(1).EQ.Z(3)) RETURN      
       
      PI=3.1415926535898  
      TPI=2.*PI  
      DO 10 IC=1,NOC      
         IF(ZC(IC).LT.Z(1)) GOTO 10         
         IF(ZC(IC).GT.Z(3)) GOTO 20         
         FRAC=(ZC(IC)-Z(1))/(Z(3)-Z(1))     
         X1=X(1)+(X(3)-X(1))*FRAC  
         Y1=Y(1)+(Y(3)-Y(1))*FRAC  
         IF(ZC(IC).LE.Z(2).AND.Z(1).NE.Z(2)) THEN    
           FRAC=(ZC(IC)-Z(1))/(Z(2)-Z(1))  
           X2=X(1)+FRAC*(X(2)-X(1)) 
           Y2=Y(1)+FRAC*(Y(2)-Y(1)) 
         ELSE    
           FRAC=(ZC(IC)-Z(2))/(Z(3)-Z(2))  
           X2=X(2)+FRAC*(X(3)-X(2)) 
           Y2=Y(2)+FRAC*(Y(3)-Y(2)) 
         ENDIF   
         IF(ICORD.NE.0) THEN       
           FLP1=1.       
           FLP2=1.       
           IF(ICORD.EQ.1) THEN    
             IF(Y1.GT.PI.AND.Y1.LT.TPI) FLP1=-1.   
             IF(Y2.GT.PI.AND.Y2.LT.TPI) FLP2=-1.   
             Y1=COS(Y1)   
             Y2=COS(Y2)   
           ENDIF         
           Y1=AMIN1(AMAX1(-1.,Y1),1.)      
           Y2=AMIN1(AMAX1(-1.,Y2),1.)      
           TX1=Y1*X1     
           Y1=FLP1*X1*SQRT(1.-Y1*Y1)
           X1=TX1        
           TX2=Y2*X2     
           Y2=FLP2*X2*SQRT(1.-Y2*Y2)
           X2=TX2        
         ENDIF   
         ZIX1=FX0+X1*XFAC 
         ZIY1=FY0+Y1*YFAC 
         ZIX2=FX0+X2*XFAC 
         ZIY2=FY0+Y2*YFAC 
         CALL DRV(ZIX1,ZIY1,ZIX2,ZIY2)   
         ISYM(IC)=ISYM(IC)+NLAB    
         IF(ISYM(IC).GE.1) THEN    
           ICC=IC+N1-1   
           IDX=8         
           IDY=0         
           IF(ABS(ZIX2-ZIX1).GE.ABS(ZIY2-ZIY1)) THEN         
             IDX=0      
             IDY=8      
           ENDIF         
           IX11=MIN(MAX(IXL+5,INT(ZIX1)+IDX),IXR-5) 
           IY11=MIN(MAX(IYB+5,INT(ZIY1)+IDY),IYT-5) 
           CALL DLCH(IX11,-IY11,' ',ICC,1) 
           ISYM(IC)=-ISKIP        
         ENDIF   
   10 CONTINUE   
       
   20 RETURN     
      
END        
       
SUBROUTINE QCPLOT(NX,NY,INCX,INCY,Z,NDIM,ZC,NC,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME,LGZ,IOUNIT)         
!***********************************************************************        
!     NONGRAPHICS CONTOUR PLOTTER.  QUICK TEST OF THE LAYOUT OF PLOTS  *        
! TO BE MADE WITH CPLOT/CPLOTX.  WRITES AN ARRAY OF LETTERS TO FORM A  *        
! "CONTOUR" PLOT ON PRINTED OUTPUT FROM UNIT IOUNIT.                   *        
!                                                                      *        
!     ARGUMENTS:                                                       *        
!                                                                      *        
! NX/NY  - NUMBER OF POINTS IN THE X/Y-DIRECTION TO BE PLOTTED.        *        
! INCX   - SKIP PARAMETER IN A ROW.                                    *        
! INCY   - SKIP PARAMETER IN A COLUMN.                                 *        
! Z      - THE TWO-DIMENSIONAL FUNCTION TO BE CONTOURED; Z SHOULD BE   *        
! STORED SO THAT Z(I,J) IS THE VALUE OF Z AT X(I),Y(J).  THIS          *        
! CORRESPONDS TO I ACROSS AND J ALONG THE PAGE.                        *        
! NDIM   - LENGTH OF A ROW OF Z (1ST DIMENSION OF THE 2-D ARRAY).      *        
! ZC     - THE TABLE OF CONTOUR VALUES.                                *        
! NC     - NUMBER OF CONTOURS TO BE PLOTTED; MAXIMUM OF 26.            *        
! NC < 0:  QCPLOT AUTOMATICALLY FILLS ZC WITH NC VALUES.               *        
! NC > 0:  ZC IS SUPPLIED BY THE USER; VALUES MUST BE STORED           *        
! IN INCREASING ORDER IN ZC.                                           *        
! TITLE       - TITLE FOR THE GRAPH.                                   *        
! XNAME/YNAME - LABEL FOR THE X/Y-AXIS.                                *        
! NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *        
! LGZ    - CONTROLS THE NUMBER OF LOG10 CONTOURS; SEE CPLOTX.          *        
! IOUNIT - UNIT NUMBER FOR THE PRINTED OUTPUT.                         *        
!                                                                      *        
!     WRITTEN BY D.W. HEWETT 1/15/83                                   *        
!***********************************************************************      
CHARACTER*(*) TITLE,XNAME,YNAME       
DIMENSION Z(NDIM,*),ZC(*)      
DIMENSION IROW(80)    
 
NNX=IABS(NX) 
NNY=IABS(NY) 
IF(NNY.LE.1) RETURN 
ILGZ=IABS(LGZ)        
INX=IABS(INCX)        
INY=IABS(INCY)        

10 NCUT=NNX/INX        
   IF(NCUT.GE.75) THEN 
     INX=INX+INX      
     WRITE(IOUNIT,11) INX      
     GOTO 10 
   ENDIF      
       
   NOC=MIN(26,IABS(NC))  
   IF(NC.LE.0) THEN    
     CALL MINM(Z,NDIM,NNX,NNY,INX,INY,ZMIN,IDUM,JDUM)     
     CALL MAXM(Z,NDIM,NNX,NNY,INX,INY,ZMAX,IDUM,JDUM)     
     IF(ILGZ.EQ.0) THEN        
       DELZ=(ZMAX-ZMIN)/NOC   
       DO IC=1,NOC         
         ZC(IC)=ZMIN+(REAL(IC)-.5)*DELZ 
       ENDDO
     ELSE    
       LGMX=ALOG19(ZMAX)        
       IF(ZMAX.LT.1.) LGMX=LGMX-1      
         LGMN=ALOG19(ZMIN)        
         IF(ZMIN.LT.1.) LGMN=LGMN-1      
         LGMN=MAX(LGMN,LGMX-25)   
         LDEC=LGMX-LGMN+1       
         IF(LGZ.LT.0) THEN      
         IF(ILGZ.EQ.1) LDEC=MIN(LDEC,2)        
         IF(ILGZ.EQ.2) LDEC=MIN(LDEC,5)        
         LDEC=MIN(LDEC,8)      
         LGMN=LGMX-LDEC+1    
         ELSE 
           IF(LDEC.GT.2.AND.ILGZ.LT.2) ILGZ=2    
           IF(LDEC.GT.5.AND.ILGZ.LT.3) ILGZ=4    
           IF(LDEC.GT.8) ILGZ=10        
         ENDIF         
         IC=0 
         STEP=10.**LGMN         
         DO ID=1,LDEC        
           DO II=1,9,ILGZ   
             ZCT=REAL(II)*STEP         
             IF((IC.EQ.NOC).OR.(ZCT.GT.ZMAX)) EXIT    
             IC=IC+1 
             ZC(IC)=ZCT
           ENDDO
           STEP=STEP*10.
         ENDDO
         NOC=IC        
       ENDIF   
     ENDIF      
       
    WRITE(IOUNIT,61) TITLE       
    WRITE(IOUNIT,62) XNAME(1:LEN(XNAME)),YNAME(1:LEN(YNAME))         
    N1=ICHAR('A')         
    WRITE(IOUNIT,63) CHAR(N1),ZC(1),CHAR(N1+NOC-1),ZC(NOC)  
    IF(ILGZ.NE.0) WRITE(IOUNIT,64) NOC,ZMIN,ZMAX,LDEC,LGZ,ILGZ       
    WRITE(IOUNIT,65)      
       
    DO J=NNY,1,-INY 
       I1=0    
       DO I=1,NNX,INX         
         I1=I1+1       
         DO IC=1,NOC         
           IF(Z(I,J).LE.ZC(IC)) GOTO 80 
         ENDDO      
         IC=NOC+1      
 80      IROW(I1)=N1+IC-1       
       ENDDO        
       WRITE(IOUNIT,91) J,(CHAR(IROW(I)),I=1,I1)   
     ENDDO  
     I1=0       
     DO I=1,NNX,INX  
       I1=I1+1 
       IROW(I1)=MOD(I,10) 
     ENDDO
     WRITE(IOUNIT,111) (IROW(I),I=1,I1)    
       
     RETURN     
       
!     * FORMATS. 
   11 FORMAT(1X,'** NNX GREATER THAN 75, INX CHANGED TO',I5)  
   61 FORMAT(/1X,'QCPLOT: ',A)       
   62 FORMAT(9X,A,' HORIZONTALLY, ',A,' VERTICALLY') 
   63 FORMAT(9X,A1,' =',1PE9.2,4X,A1,' =',1PE9.2)    
   64 FORMAT(9X,'NOC,ZMIN,ZMAX,LDEC,LGZ,ILGZ',I5,2E14.6,3I5)  
   65 FORMAT(9X,'THE SYMBOL A MEANS: IN THAT LOCATION VALUE .LE. A'/)  
   91 FORMAT(I3,1X,75A1)    
  111 FORMAT(4X,75I1)       

END        

SUBROUTINE VPLOT(MX,MY,IVEC,X,Y,NX,NY,INCX,INCY,VX,VY,NDIM,SIZE,L,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME) 
!***********************************************************************
!     SUBROUTINE VPLOT DRAWS A REPRESENTION OF A 2-DIMENSIONAL VECTOR  *
! FIELD VX = F(X,Y), VY = G(X,Y).  THESE FUNCTIONS SHOULD BE STORED AS *
! 2-DIMENSIONAL ARRAYS VX(I,J), VY(I,J), COMPUTED AT THE OBSERVATION   *
! POINTS X(I), I=1,IABS(NX),IABS(INCX), Y(J), J=1,IABS(NY),IABS(INCY). *
! ENTRY VPLOTX IS AN EXTENSION FOR POLAR COORDINATES.                  *
!                                                                      *
!     ARGUMENTS:                                                       *
!                                                                      *
! MX/MY - SEE LPLOT.                                                   *
! IVEC  - PROVIDES DIFFERENT OPTIONS FOR THE PRESENTATION OF THE VEC-  *
!         TOR FIELD ACCORDING TO THE FORMULA                           *
!            IABS(IVEC) = ISUP*100 + IDOT*10 + JVEC,                   *
!         WHERE JVEC DETERMINES THE SHAPE OF THE ARROWHEADS:           *
!            JVEC = 1 - SIZE ARROWHEAD PROPORTIONAL TO VECTOR LENGTH   *
!                   2 - CONSTANT-SIZE ARROWHEAD,                       *
!         AND IDOT PROVIDES THE OPTION TO IDENTIFY THE DATA POINTS:    *
!            IDOT = 0 - NO ACTION (DEFAULT)                            *
!                   1 - DOT PLACED AT THE DATA LOCATIONS,              *
!         AND ISUP DETERMINES WHETHER SMALL VECTORS ARE DRAWN OR NOT:  *
!            ISUP = 0 - NO ACTION (DEFAULT)                            *
!                   1 - SUPPRESS DOT AND VECTOR IF BOTH VECTOR COMPO-  *
!                       NENTS <= EPS * MAXIMUM AMPLITUDE OF VX AND VY, *
!                       WHERE EPS IS FIXED IN THE PARAMETER STATEMENT. *
! X/Y   - TABLE OF THE ABSCISSA/ORDINATE VALUES.                       *
! NX    - IABS(NX) IS THE NUMBER OF POINTS IN X TO BE USED.            *
!         NX < 0 : ONLY THE FRAME AND SCALES FOR THE PLOT ARE DRAWN.   *
! NY    - IABS(NY) IS THE NUMBER OF POINTS IN Y TO BE USED.            *
!         NY < 0 : VECTORS ARE DRAWN ON A FRAME PREVIOUSLY CREATED     *
!         BY A CALL TO VPLOT WITH NX < 0 OR A DIRECT CALL OF NFRAME    *
!         (IN ORDER TO SPECIFY A FRAME SIZE DIFFERENT FROM THE ONE     *
!         IMPLIED BY THE RANGES OF X AND Y).                           *
! INCX  - IABS(INCX) IS THE SKIP PARAMETER IN A ROW.                   *
!         INCX < 0 : XMIN = X(1) AND HX =X(2).                         *
! INCY  - IABS(INCY) IS THE SKIP PARAMETER IN A COLUMN.                *
!         INCY < 0 : YMIN = Y(1) AND HY =Y(2).                         *
! VX/VY - THE TWO-DIMENSIONAL VECTOR COMPONENTS TO BE PLOTTED; STORED  *
!         SUCH THAT VX/VY(I,J) IS THE VALUE OF VX/VY AT X(I),Y(J).     *
! NDIM  - LENGTH OF A ROW OF VX/VY (1ST ARGUMENT OF THE 2-D ARRAYS).   *
!         HENCE, ONE SHOULD OBSERVE: NX <= NDIM.                       *
! SIZE  - THE VECTORS ARE PLOTTED WITH THEIR MAXIMUM AMPLITUDE AMP     *
!         (PRINTED ON TOP OF THE GRAPH IF ISUP=1) SCALED DOWN WITH A   *
!         FACTOR OF SIZE*STEP/AMP, WHERE STEP IS THE WIDTH OF THE      *
!         SMALLEST MESH OF THE GRID AND SIZE IS CHOSEN FOR CLARITY OF  *
!         PRESENTATION.  FOR A UNIFORM GRID, THE LARGEST VECTORS WILL  *
!         PRECISELY FIT THE MESH WHEN SIZE=1.  FOR A NON-UNIFORM GRID, *
!         SIZE HAS TO BE CHOSEN BY CONSIDERING THE SPACING OF A TYPI-  *
!         CAL MESH AS COMPARED TO THE SMALLEST ONE.                    *
! L     - LENGTH OF THE ARROWHEADS AS AN INTEGER PERCENTAGE OF THE     *
!         VECTOR LENGTH (FOR JVEC=1) OR AS AN ABSOLUTE VALUE IN TERMS  *
!         OF PLOTTING COORDINATES (FOR JVEC=2).                        *
! TITLE               - TITLE FOR THE GRAPH.                           *
! XNAME/YNAME         - LABEL FOR THE X/Y-AXIS.                        *
! NTITLE/NXNAME/YNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.     *
!                                                                      *
!     ADDITIONAL ARGUMENTS FOR VPLOTX:                                 *
!                                                                      *
! RMAX  - MAXIMUM RADIUS FOR A POLAR PLOT.                             *
!         = 0 : CARTESIAN PLOT.                                        *
!         > 0 : X/Y CORRESPONDS TO R/THETA (IN RADIANS),               *
!               VX/VY CORRESPONDS TO THE VECTOR COMPONENT VR/VT.       *
! IQUAD - TOTAL NUMBER OF QUADRANTS (FOR RMAX.NE.0 ONLY).              *
!                                                                      *
!     WRITTEN BY D. HEWETT 3/83 BY ADAPTING A VERSION OF CPLOT.        *
!     EXTENDED FOR NON-UNIFORM GRID, ADDED ARGUMENTS IVEC, SIZE, L,    *
!     ADDED PARAMETER EPS, ELIMINATED ICORD=2 OPTION OF CPLOT/TRICJ3,  *
!     HGO 16/12/85.                                                    *
!***********************************************************************
                                                                       
      PARAMETER (EPS=.1)                                                
                                                                       
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT                         
      DIMENSION X(*),Y(*),VX(NDIM,*),VY(NDIM,*)                         
      CHARACTER*(*) TITLE,XNAME,YNAME                                   
      CHARACTER TITLE1*80,STRING*14                                     
                                                                       
   10 ICORD=0                                                           
      RMX=0.                                                            
      NNX=IABS(NX)                                                      
      NNY=IABS(NY)                                                      
      IF(NNY.LE.1) RETURN                                               
      INX=IABS(INCX)                                                    
      INY=IABS(INCY)                                                    
      HX=X(2)-X(1)                                                      
      HY=Y(2)-Y(1)                                                      
      XMN=X(1)                                                          
      YMN=Y(1)                                                          
      XMX=X(NNX)                                                        
      YMX=Y(NNY)                                                        
      IF(INCX.LT.0) THEN                                                
         HX=X(2)                                                        
         XMX=X(1)+(NNX-1)*X(2)/INX                                      
      ENDIF                                                             
      IF(INCY.LT.0) THEN                                                
         HY=Y(2)                                                        
         YMX=Y(1)+(NNY-1)*Y(2)/INY                                      
      ENDIF                                                             
                                                                       
   20 JVEC=MOD(IABS(IVEC),10)                                           
      IDOT=MOD(IABS(IVEC)/10,10)                                        
      ISUP=MOD(IABS(IVEC)/100,10)                                       
      IF(NY.GE.0) THEN                                                  
         NB=0                                                           
         IF(ISUP.EQ.1) NB=9                                             
         TITLE1=TITLE                                                   
         NTITL1=ISIGN(MIN(IABS(NTITLE)+NB,80),NTITLE)                   
         CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,TITLE1,NTITL1,XNAME,NXNAME,YNAME,NYNAME)           
         IF(NX.LT.0) RETURN                                             
      ELSE                                                              
         CALL OFRAME(MX,MY)                                             
      ENDIF                                                             
                                                                       
!     * INT(FX0) AND INT(FY0) ARE THE INTEGER LOCATIONS OF X=0 AND Y=0. 
      XFAC=(IXR-IXL)/(XR-XL)                                            
      YFAC=(IYT-IYB)/(YT-YB)                                            
      FX0=IXL-XL*XFAC                                                   
      FY0=IYT-YT*YFAC                                                   
                                                                       
      CALL MAXAM(VX,NDIM,NNX,NNY,INX,INY,VXMX,IDUM,JDUM)                
      CALL MAXAM(VY,NDIM,NNX,NNY,INX,INY,VYMX,IDUM,JDUM)                
      VXMX=ABS(VXMX)                                                    
      VYMX=ABS(VYMX)                                                    
      AMP=AMAX1(VXMX,VYMX)                                              
      IF(ISUP.EQ.1.OR.AMP.EQ.0.) THEN                                   
         WRITE(STRING,'(''AMP ='',1PE9.2)') AMP                         
         CALL DLCH(IXR-110,IYT+18,STRING,14,1)                          
         WRITE(STRING,'(''EPS ='',1PE9.2)') EPS                         
         CALL DLCH(IXR-110,IYT+4,STRING,14,1)                           
         IF(AMP.EQ.0.) AMP=1.                                           
      ENDIF                                                             
      DXMN=INX*ABS(HX)                                                  
      IF(INCX.GT.0) THEN                                                
         DO 30 I=1+INX,NNX,INX                                          
            DX=ABS(X(I)-X(I-INX))                                       
   30       IF(DX.LT.DXMN) DXMN=DX                                      
      ENDIF                                                             
      DYMN=INY*ABS(HY)                                                  
      IF(INCY.GT.0) THEN                                                
         DO 40 J=1+INY,NNY,INY                                          
            DY=ABS(Y(J)-Y(J-INY))                                       
   40       IF(DY.LT.DYMN) DYMN=DY                                      
      ENDIF                                                             
      STEP=AMIN1(DXMN,DYMN)                                             
      VFAC=SIZE*STEP/AMP                                                
      THR=EPS*SIZE*STEP                                                 
                                                                       
      PI=3.1415926535898                                                
      TPI=2.*PI                                                         
      Y1SAV=Y(1)-HY                                                     
      DO 60 J=1,NNY,INY                                                 
         Y1=Y1SAV+HY                                                    
         IF(INCY.GT.0) Y1=Y(J)                                          
         Y1SAV=Y1                                                       
         X1SAV=X(1)-HX                                                  
         DO 50 I=1,NNX,INX                                              
            X1=X1SAV+HX                                                 
            IF(INCX.GT.0) X1=X(I)                                       
            X1SAV=X1                                                    
            Y1=Y1SAV                                                    
            X2=VFAC*VX(I,J)                                             
            Y2=VFAC*VY(I,J)                                             
            IF(ISUP.EQ.1.AND.ABS(X2).LT.THR.AND.ABS(Y2).LT.THR) GOTO 50 
            IF(ICORD.NE.0) THEN                                         
               C=COS(Y1)                                                
               C=AMIN1(AMAX1(-1.,C),1.)                                 
               FLP=1.                                                   
               IF(Y1.GT.PI.AND.Y1.LT.TPI) FLP=-1.                       
               S=FLP*SQRT(1.-C*C)                                       
               Y1=X1*S                                                  
               X1=X1*C                                                  
               X2S=X2                                                   
               X2=X2S*C-Y2*S                                            
               Y2=X2S*S+Y2*C                                            
            ENDIF                                                       
            X2=X1+X2                                                    
            Y2=Y1+Y2                                                    
            ZIX1=FX0+X1*XFAC                                            
            ZIY1=FY0+Y1*YFAC                                            
            ZIX2=FX0+X2*XFAC                                            
            ZIY2=FY0+Y2*YFAC                                            
            IF(IDOT.EQ.1) CALL DLCH(INT(ZIX1),INT(-ZIY1),' ',46,1)                
            IF(JVEC.EQ.1) CALL ARROW1(ZIX1,ZIY1,ZIX2,ZIY2,L)            
            IF(JVEC.EQ.2) CALL ARROW2(ZIX1,ZIY1,ZIX2,ZIY2,L)            
   50    CONTINUE                                                       
   60 CONTINUE                                                          
                                                                       
      RETURN                                                            
                                                                       
ENTRY VPLOTX(MX,MY,IVEC,X,Y,NX,NY,INCX,INCY,VX,VY,NDIM,SIZE,L,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME,RMAX,IQUAD)
                                                                       
      IF(RMAX.EQ.0.) GOTO 10                                            
                                                                       
      ICORD=1                                                           
      RMX=ABS(RMAX)                                                     
      NNX=IABS(NX)                                                      
      NNY=IABS(NY)                                                      
      IF(NNY.LE.1) RETURN                                               
      INX=IABS(INCX)                                                    
      INY=IABS(INCY)                                                    
      HX=X(2)-X(1)                                                      
      HY=Y(2)-Y(1)                                                      
      IF(INCX.LT.0) HX=X(2)                                             
      IF(INCY.LT.0) HY=Y(2)                                             
      XMN=0.                                                            
      YMN=0.                                                            
      XMX=RMX                                                           
      YMX=RMX                                                           
      IQUD=MAX(IQUAD,1)                                                 
      IF(IQUD.GT.2.AND.RMAX.LT.0.) IQUD=2                               
      IF(IQUD.EQ.2) THEN                                                
         XMN=-RMX                                                       
         XMX=RMX                                                        
         YMX=RMX                                                        
      ELSEIF(IQUD.EQ.3.OR.IQUD.EQ.4) THEN                               
         XMN=-RMX                                                       
         YMN=-RMX                                                       
      ENDIF                                                             
      GOTO 20                                                           
                                                                       
      END                                                               



SUBROUTINE FPLOT(MX,MY,IVEC,X,Y,NPTS,INC,VX,VY,VFAC,L,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)  
!***********************************************************************        
!     FPLOT IS A ONE-DIMENSIONAL VECTOR PLOTTING ROUTINE WHICH PLOTS   *        
! THE TWO-DIMENSIONAL FLOW FIELD VX = F(X,Y), VY = G(X,Y) ALONG A ONE- *        
! DIMENSIONAL CURVE X(I), Y(I), I=1,..,NPTS.  CONSEQUENTLY, THE VECTOR *        
! COMPONENTS SHOULD BE GIVEN AS ONE-DIMENSIONAL ARRAYS VX(I), VY(I).   *        
! THE AMPLITUDE OF THE VECTOR FIELD IS SCALED WITH THE FACTOR VFAC.    *        
!                                                                      *        
!     ARGUMENTS:                                                       *        
!                                                                      *        
! MX/MY - SEE LPLOT.                                                   *        
! IVEC  - PROVIDES DIFFERENT OPTIONS FOR THE PRESENTATION OF THE VEC-  *        
!         TOR FIELD ACCORDING TO THE FORMULA                           *        
!   IABS(IVEC) = IDOT*10 + JVEC,                                       *        
!         WHERE JVEC DETERMINES THE SHAPE OF THE ARROWHEADS:           *         
!   JVEC = 1 - SIZE ARROWHEAD PROPORTIONAL TO VECTOR LENGTH            *        
! 2 - CONSTANT-SIZE ARROWHEAD,                                         *        
!         AND IDOT PROVIDES THE OPTION TO IDENTIFY THE DATA POINTS:    *        
!   IDOT = 0 - NO ACTION (DEFAULT)                                     *        
! 1 - DOT PLACED AT THE DATA LOCATIONS.                                *        
!         IVEC < 0 : ONLY FRAME AND SCALES FOR THE PLOT ARE DRAWN.     *        
! X/Y   - TABLE OF ABSCISSA/ORDINATE VALUES.                           *        
! NPTS  - IABS(NPTS) IS THE NUMBER OF ELEMENTS IN THE ARRAYS X AND Y.  *        
!         NPTS < 0: THE VECTORS ARE DRAWN ONTO A FRAME PREVIOUSLY SET  *        
!         UP BY A CALL TO NFRAME OR FPLOT WITH IVEC < 0.               *        
! INC   - IABS(INC) IS THE SPACING BETWEEN THE X/Y POSITIONS PLOTTED.  *        
!         INC < 0: THE Y-POSITIONS PLOTTED ARE PAIRED WITH ABSCISSA    *        
!         VALUES DETERMINED BY THE TWO VALUES XMIN=X(1) AND DX=X(2),   *        
!         WHICH THE USER SHOULD INSERT IN X.                           *        
! VX    - 1D ARRAY CONTAINING THE X-COMPONENTS OF THE VECTOR FIELD.    *        
! VY    - 1D ARRAY CONTAINING THE Y-COMPONENTS OF THE VECTOR FIELD.    *        
! VFAC  - MULTIPLICATIVE FACTOR FOR THE AMPLITUDE OF THE VECTORS.      *        
! L     - LENGTH OF THE ARROWHEADS AS AN INTEGER PERCENTAGE OF THE     *        
!         VECTOR LENGTH (FOR JVEC=1) OR AS AN ABSOLUTE VALUE IN TERMS  *        
!         OF PLOTTING COORDINATES (FOR JVEC=2).                        *        
! TITLE       - TITLE FOR THE GRAPH.                                   *        
! XNAME/YNAME - LABEL FOR THE X/Y-AXIS.                                *        
! NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *        
!                                                                      *        
!     WRITTEN BY HANS GOEDBLOED 29/08/85 BY ADAPTING LPLOT.            *        
!***********************************************************************        
       
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
DIMENSION X(*),Y(*),VX(*),VY(*)
CHARACTER*(*) TITLE,XNAME,YNAME       
 
JVEC=MOD(IABS(IVEC),10)        
IDOT=MOD(IABS(IVEC)/10,10)     
NTOT=IABS(NPTS)       
INCA=IABS(INC)        
       
!     * DRAW THE FRAME.   
IF(NPTS.GT.0) THEN  
  IF(INC.LT.0) THEN         
    XMN=X(1)        
    XMX=X(1)+(NTOT-1)*X(2)/INCA     
  ELSE    
    CALL MAXV(X,NTOT,INCA,XMX,IDUM) 
    CALL MINV(X,NTOT,INCA,XMN,IDUM) 
  ENDIF   
  CALL MAXV(Y,NTOT,INCA,YMX,IDUM)    
  CALL MINV(Y,NTOT,INCA,YMN,IDUM)    
  CALL NFRAME(MX,MY,1,XMN,XMX,YMN,YMX,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)  
  IF(IVEC.LT.0) RETURN      
  ELSE       
    CALL OFRAME(MX,MY) 
  ENDIF      
       
!     * DRAW THE VECTOR FIELD.     
  XFAC=(IXR-IXL)/(XR-XL)         
  YFAC=(IYT-IYB)/(YT-YB)         
  HX=0.      
  IF(INC.LT.0) HX=X(2)  
  X1=X(1)-HX 
  DO 10 I=1,NTOT,INCA 
    X1=X1+HX         
    IF(INC.GT.0) X1=X(I)        
    Y1=Y(I)   
    ZIX1=MIN(MAX(real(IXL),real(IXL)+(X1-XL)*XFAC),real(IXR))  
    ZIY1=MIN(MAX(real(IYB),real(IYB)+(Y1-YB)*YFAC),real(IYT))  
    IF(IDOT .EQ. 1) CALL DLCH(INT(ZIX1),-INT(ZIY1),' ',46,1)      
    X2=X1+VX(I)*VFAC 
    Y2=Y1+VY(I)*VFAC 
    ZIX2=real(IXL)+(X2-XL)*XFAC       
    ZIY2=real(IYB)+(Y2-YB)*YFAC       
    IF(ZIX2.LT.real(IXL) .OR. ZIX2 .GT. real(IXR)     &
      .OR. ZIY2 .LT. real(IYB) .OR. ZIY2 .GT. real(IYT)) THEN 

! * IF VECTOR WOULD CROSS THE BOUNDARY, SUPPRESS ARROWHEAD   
! * AND PART OF THE VECTOR OUTSIDE THE DOMAIN.      
  
    ZIXX=MIN(MAX(real(IXL),ZIX2),real(IXR))         
    ZIYY=MIN(MAX(real(IYB),ZIY2),real(IYT))         
    ZIYYS=ZIYY    
    IF (ZIXX .NE. ZIX2) ZIYY=ZIY1 + (ZIY2-ZIY1)*(ZIXX-ZIX1)/(ZIX2-ZIX1)       
       IF (ZIYYS .NE. ZIY2 .AND. (ZIYY .LE. real(IYB) .OR. ZIYY .GE. real(IYT))) THEN        
           ZIXX=ZIX1+(ZIX2-ZIX1)*(ZIYYS-ZIY1)/(ZIY2-ZIY1) 
           ZIYY=ZIYYS         
       ENDIF         
       CALL DRV(ZIX1,ZIY1,ZIXX,ZIYY)   
     ELSE  
       IF(JVEC .EQ. 1) CALL ARROW1(ZIX1,ZIY1,ZIX2,ZIY2,L)  
       IF(JVEC .EQ. 2) CALL ARROW2(ZIX1,ZIY1,ZIX2,ZIY2,L)  
     ENDIF   
10 CONTINUE   
   RETURN     
END        

         
SUBROUTINE ARROW1(ZIX1,ZIY1,ZIX2,ZIY2,L)  
!***********************************************************************        
!     SUBROUTINE ARROW1 DRAWS AN ARROW FROM (IX1,IY1) TO (IX2,IY2).    *        
! THE HEIGHT AND THE WIDTH OF THE ARROWHEAD ARE FIXED RELATIVE TO THE  *        
! LENGTH OF THE ARROW BY THE VARIABLES H AND W, WHERE H = L/100 (I.E., *        
! THE ARGUMENT L PROVIDES THE LENGTH OF THE ARROWHEAD AS AN INTEGER    *        
! PERCENTAGE OF THE VECTOR LENGTH R) AND W = WR*H (I.E., THE PARAMETER *        
! WR PROVIDES THE WIDTH RELATIVE TO THE HEIGHT OF THE ARROWHEAD).      *        
!     THROUGH ENTRY ARROW2 ARROWS WITH A CONSTANT LENGTH OF THE ARROW- *        
! HEAD ARE DRAWN.  THIS LENGTH IS FIXED BY THE ARGUMENT L = H*R (I.E., *        
! L PROVIDES THE ABSOLUTE LENGTH OF THE ARROWHEAD IN TERMS OF PLOTTING *        
! COORDINATES).                                                        *        
!                                                                      *        
!     WRITTEN HGO 29/08/85                                             *        
!***********************************************************************      
PARAMETER (WR=.35)    
       
H=REAL(L)/100.      

10 CALL DRV(ZIX1,ZIY1,ZIX2,ZIY2)      
   IF(L.EQ.0) RETURN   
   W=WR*H     
   ZIHX=H*(ZIX2-ZIX1)       
   ZIHY=H*(ZIY2-ZIY1)       
   ZIWX=W*(ZIX2-ZIX1)       
   ZIWY=W*(ZIY2-ZIY1)       
   ZIX3=ZIX2-ZIHX+ZIWY     
   ZIY3=ZIY2-ZIWX-ZIHY     
   ZIX4=ZIX2-ZIHX-ZIWY     
   ZIY4=ZIY2+ZIWX-ZIHY     
   CALL DRWABS(ZIX3,ZIY3)  
   CALL DRWABS(ZIX4,ZIY4)  
   CALL DRWABS(ZIX2,ZIY2)  
   RETURN     
       
!     * ENTRY FOR DRAWING CONSTANT-SIZE ARROWHEADS.  
   ENTRY ARROW2(ZIX1,ZIY1,ZIX2,ZIY2,L)
   R=SQRT(REAL((ZIX2-ZIX1)**2+(ZIY2-ZIY1)**2))        
   IF(R.LT.REAL(L)) THEN        
      CALL DLCH(INT(ZIX1),-INT(ZIY1),' ',46,1)
      RETURN  
   ENDIF      
   H=REAL(L)/R         
   GOTO 10    
       
END        
       

SUBROUTINE SPLOT(MX,MY,IS,IOP,YX,ZXY,NX,NY,INCYX,Z,NDIM,IJARR,NS,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)  
!***********************************************************************        
!     SPLOT PLOTS THE ONE-DIMENSIONAL CROSS-SECTIONS OF THE TWO-DIMEN- *        
! SIONAL FUNCTION Z(X(I),Y(J)) IN THE X- OR Y-DIRECTION, DEPENDING ON  *        
! THE VALUE OF IS.  THE VALUES OF THE X- OR Y-INDICES MAY BE SPECIFIED *        
! IN THE ARRAY IJARR(ISEC), WHERE ISEC=1,NS.                           *        
!                                                                      *        
!     ARGUMENTS:                                                       *        
!                                                                      *        
! MX/MY  - SEE LPLOT.                                                  *        
! IS     - DETERMINES WHETHER X- OR Y-SECTIONS ARE PLOTTED.            *        
! IS = 1 : X-SECTION ZX PLOTTED AS A FUNCTION OF Y.                    *        
! IS = 2 : Y-SECTION ZY PLOTTED AS A FUNCTION OF X.                    *        
! HENCE, FOR EXPRESSIONS XY READ: X FOR IS=1, Y FOR IS=2,              *        
! AND VICE VERSA FOR YX.                                               *        
! IOP    - SEE LPLOT.                                                  *        
! IF IOP > 10 (I.E., CHARACTERS ARE PLACED ON THE CURVES),             *        
! THE CHARACTER NUMBER IC IS AUTOMATICALLY INCREMENTED FOR             *        
! THE NS DIFFERENT CURVES SPECIFIED.  E.G., IF IOP=30971, A            *        
! LOWER CASE 'A' IS PLACED AT EVERY 3RD POINT ON THE FIRST             *        
! CURVE, A LOWER CASE 'B' ON THE SECOND CURVE, ETC.                    *        
! YX     - TABLE OF THE ABSCISSA VALUES FOR THE PLOTS.                 *        
! ZXY    - AN ARRAY THAT HOLDS THE X/Y-SECTIONS OF THE FUNCTION Z.     *        
! IT SHOULD BE DIMENSIONED NY/NX IN THE CALLING PROGRAM.               *        
! NX     - THE NUMBER OF POINTS IN THE X-DIRECTION, I.E. THE NUMBER OF *        
! POINTS IN A Y-SECTION PLOT.                                          *        
! NX < 0 : ONLY THE FRAME AND SCALES FOR THE PLOT ARE DRAWN.           *        
! NY     - THE NUMBER OF POINTS IN THE Y-DIRECTION, I.E. THE NUMBER OF *        
! POINTS IN A X-SECTION PLOT.                                          *        
! NY < 0 : SECTIONS ARE DRAWN ON A FRAME PREVIOUSLY CREATED            *        
! BY A CALL TO SPLOT WITH NX < 0 OR A DIRECT CALL OF NFRAME            *        
! (IN ORDER TO SPECIFY A FRAME SIZE DIFFERENT FROM THE ONE             *         
! IMPLIED BY THE RANGES OF YX AND ZXY).                                *        
! INCYX  - IABS(INCYX) IS THE SKIP PARAMATER FOR YX.                   *        
! INCYX < 0 : YXMIN = YX(1) AND DYX = YX(2).                           *        
! Z      - THE TWO-DIMENSIONAL TABLE OF VALUES DIMENSIONED AT LEAST AS *        
! NX BY NY IN THE CALLING PROGRAM.                                     *        
! NDIM   - THE FIRST DIMENSION OF THE ARRAY Z.  HENCE: NX <= NDIM.     *        
! IJARR  - CONTAINS THE X/Y INDICES AT WHICH TO TAKE SECTIONS AND      *        
! DIMENSIONED BY NS.                                                   *        
! NS     - THE NUMBER OF ROWS OR COLUMNS AT WHICH TO TAKE SECTIONS.    *        
! NS < 0 : SPLOT AUTOMATICALLY FILLS IJARR WITH NS VALUES.             *        
! NS > 0 : IJARR IS SUPPLIED BY THE USER.                              *        
! TITLE       - TITLE FOR THE GRAPH.                                   *        
! XNAME/YNAME - LABEL FOR THE X/Y-AXIS.                                *        
! NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *        
!                                                                      *        
!     WRITTEN BY BRENDAN GODFREY                                       *        
!     ADDED ARGUMENTS IS AND NDIM, HGO 23/12/85.                       *        
!     CORRECTED ERROR IN CALCULATION IJARR, HGO 2/8/91                 *        
!***********************************************************************        
       
DIMENSION YX(*),ZXY(*),Z(NDIM,*),IJARR(*)      
CHARACTER*(*) TITLE,XNAME,YNAME       
       
NNX=IABS(NX) 
NNY=IABS(NY) 
IF(IS.EQ.1) THEN    
  NNXY=NNX         
  NNYX=NNY         
ELSE       
  NNXY=NNY         
  NNYX=NNX         
ENDIF      
       
!     * DRAW FRAME AND SCALES.     
IF(NY.GT.0) THEN    
!        * DETERMINE THE RANGE OF ALL POSSIBLE ZXY'S.         
  CALL MINM(Z,NDIM,NNX,NNY,1,1,ZXYMIN,IDUM,JDUM)       
  CALL MAXM(Z,NDIM,NNX,NNY,1,1,ZXYMAX,IDUM,JDUM)       
!        * DETERMINE THE RANGE OF YX.       
  YXMIN=YX(1)        
  YXMAX=YX(1)+YX(2)*(NNYX-1)/IABS(INCYX)      
  IF(INCYX.GT.0) YXMAX=YX(NNYX)      
  CALL NFRAME(MX,MY,IABS(IOP),YXMIN,YXMAX,ZXYMIN,ZXYMAX,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)  
!        * IF NX < 0, DRAW FRAME AND SCALES ONLY.    
   IF(NX.LT.0) RETURN        
ELSE       
!        * DRAW SECTIONS ONTO PREVIOUSLY DRAWN FRAME.         
  CALL OFRAME(MX,MY) 
ENDIF      
       
!     * DETERMINE THE NUMBER AND INDICES OF THE SECTIONS.     
NOS=IABS(NS) 
IF(NS.LT.0) THEN    
!        * STORE UNIFORMLY SPACED INDICES ALONG XY IN THE ARRAY IJARR. 
  DXY=REAL(NNXY)/REAL(NOS)    
  DO  ISEC=1,NOS 
    IJARR(ISEC)=(ISEC-0.5)*DXY+0.5
  ENDDO
ENDIF      
       
!     * FILL THE ARRAY ZXY AND PASS IT ONTO LPLOT.   
DO  ISEC=1,NOS    
  DO  JI=1,NNYX  
    IF(IS.EQ.1) ZXY(JI)=Z(IJARR(ISEC),JI)    
    IF(IS.EQ.2) ZXY(JI)=Z(JI,IJARR(ISEC))    
  ENDDO         
  IOP1=IOP         
  IF(IOP/10.NE.0) IOP1=IOP+(ISEC-1)*10        
    CALL LPLOT(MX,MY,IOP1,YX,ZXY,-NNYX,INCYX,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)   
ENDDO   
!       
RETURN     
END        
       

SUBROUTINE APLOT(MX,MY,IA,YX,AVXY,NX,NY,INCYX,Z,NDIM,IJ1,IJ2,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)  
!***********************************************************************        
!     APLOT AVERAGES OVER THE TWO-DIMENSIONAL FUNCTION Z(X(I),Y(J)) IN *        
! ONE DIRECTION (X OR Y, DEPENDING ON THE VALUE OF IA) AND PLOTS THE   *        
! RESULT WITH RESPECT TO THE OTHER DIRECTION.  AVERAGING MAY BE LIMI-  *        
! TED TO A SPECIFIED BAND OF INDICES IJ1-IJ2 IN THE X- OR Y-DIRECTION. *        
! THIS SUBROUTINE ONLY PERFORMS THE AVERAGING CALCULATION; IT CALLS    *        
! LPLOT TO DRAW THE RESULTING CURVE.                                   *        
!                                                                      *        
!     ARGUMENTS:                                                       *        
!                                                                      *        
! MX/MY  - SEE LPLOT.                                                  *        
! IA     - DETERMINES WHETHER X- OR Y-AVERAGES ARE PLOTTED.            *        
! IA = 1 : X-AVERAGE AVX PLOTTED AS A FUNCTION OF Y.                   *        
! IA = 2 : Y-AVERAGE AVY PLOTTED AS A FUNCTION OF X.                   *        
! HENCE, FOR EXPRESSIONS XY READ: X FOR IA=1, Y FOR IA=2,              *        
! AND VICE VERSA FOR YX.                                               *        
! YX     - TABLE OF THE ABSCISSA VALUES FOR THE PLOT.                  *        
! AVXY   - AN ARRAY THAT HOLDS THE X/Y-AVERAGES OF THE FUNCTION Z.     *        
! IT SHOULD BE DIMENSIONED NY/NX IN THE CALLING PROGRAM.               *        
! NX     - THE NUMBER OF POINTS IN THE X-DIRECTION, I.E. THE NUMBER OF *        
! POINTS IN A Y-AVERAGE PLOT.                                          *        
! NX < 0 : ONLY THE FRAME AND SCALES FOR THE PLOT ARE DRAWN.           *        
! NY     - THE NUMBER OF POINTS IN THE Y-DIRECTION, I.E. THE NUMBER OF *        
! POINTS IN A X-AVERAGE PLOT.                                          *        
! NY < 0 : AVERAGES ARE DRAWN ON A FRAME PREVIOUSLY CREATED            *        
! BY A CALL TO APLOT WITH NX < 0 OR A DIRECT CALL OF NFRAME            *        
! (IN ORDER TO SPECIFY A FRAME SIZE DIFFERENT FROM THE ONE             *        
! IMPLIED BY THE RANGES OF YX AND AVXY).                               *        
! INCYX  - IABS(INCYX) IS THE SKIP PARAMATER FOR YX.                   *         
! INCYX < 0 : YXMIN = YX(1) AND DYX = YX(2).                           *        
! Z      - THE TWO-DIMENSIONAL TABLE OF VALUES DIMENSIONED AT LEAST AS *        
! NX BY NY IN THE CALLING PROGRAM.                                     *        
! NDIM   - THE FIRST DIMENSION OF THE ARRAY Z.  HENCE: NX <= NDIM.     *        
! IJ1    - THE INDEX OF THE FIRST CELL IN THE AVERAGE CALCULATION.     *        
! IJ1 <= 0 : THE VALUE 1 FOR AVERAGING OVER THE WHOLE RANGE            *        
! IS TAKEN.                                                            *        
! IJ2    - THE INDEX OF THE LAST CELL OF THE BAND TO AVERAGE OVER.     *        
! IJ2 <= 0 : THE VALUE NX/NY FOR AVERAGING OVER THE WHOLE              *        
! RANGE IS TAKEN.                                                      *        
! TITLE       - TITLE FOR THE GRAPH.                                   *        
! XNAME/YNAME - LABEL FOR THE X/Y-AXIS.                                *        
! NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *        
!                                                                      *        
!     WRITTEN BY DENNIS HEWETT                                         *        
!     MODIFIED BY DEBBY HYMAN 5-80, FOR -NX OR -NY TO TRIGGER SEPARATE *        
!     DRAWING OF THE FRAME AND THE CURVE.                              *        
!     ADDED ARGUMENTS IA AND NDIM, SEPARATED X- AND Y-AVERAGE PLOTS,   *        
!     IMPROVED HANDLING OF THE SKIP PARAMETER, HGO 23/12/85.           *        
!***********************************************************************      
DIMENSION YX(*),AVXY(*),Z(NDIM,*)     
CHARACTER*(*) TITLE,XNAME,YNAME       
 
NNX=IABS(NX) 
NNY=IABS(NY) 
IF(IA.EQ.1) THEN    
   NNXY=NNX         
   NNYX=NNY         
ELSE       
   NNXY=NNY         
   NNYX=NNX         
ENDIF      
 
!     * INSTALL BAND OF INDICES TO AVERAGE OVER, IF DESIRED.  
IJ11=1     
IF(IJ1.GT.0) IJ11=IJ1        
IJ22=NNXY  
IF(IJ2.GT.0) IJ22=IJ2        
 
!     * COMPUTE THE AVERAGE AVXY.  
DO JI=1,NNYX     
  AVXY(JI)=0.      
  DO IJ=IJ11,IJ22        
    IF(IA.EQ.1) AVXY(JI)=AVXY(JI)+Z(IJ,JI)   
    IF(IA.EQ.2) AVXY(JI)=AVXY(JI)+Z(JI,IJ)   
  ENDDO         
  AVXY(JI)=AVXY(JI)/REAL(IJ22-IJ11+1)         
ENDDO
!     * PLOT AVXY AS A FUNCTION OF YX.      
CALL LPLOT(MX,MY,ISIGN(1,NX),YX,AVXY,ISIGN(NNYX,NY),INCYX,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)      
RETURN     
END        

     
SUBROUTINE TPLOT(MX,MY,IVERT,NX,NY,INCX,INCY,Z,NDIM,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)  
!***********************************************************************        
!     THIS ROUTINE PLOTS A FUNCTION OF TWO VARIABLES ON A RECTANGULAR  *        
! GRID AS A SET OF VERTICAL LINES RISING FROM THE GRID POINTS.  THE    *        
! HEIGHT OF THE LINES IS RELATED TO THE AMPLITUDE OF THE FUNCTION:     *        
! FMZ(I,J)/ZMAX FOR IVERT=1 (LINEAR),                                  *        
! GM(1+HM*ALOG10(Z(I,J)/ZMAX2))   FOR IVERT=2 (QUASI-LOG),             *        
! WHERE FM,GM,HM ARE FIXED IN THE PARAMETER STATEMENT BELOW.  THE      *        
! PARAMETERS DM,DN,EM DETERMINE THE LENGTH AND ORIENTATION OF THE X-   *        
! AND Y-AXES.  TPLOT MAY BE USED, E.G., TO PLOT FOURIER COMPONENTS     *        
! WITH A VERTICAL LOG SCALE.  IT PRODUCES A CRUDE 3-D PLOT.  ONLY      *        
! POSITIVE VALUES OF Z ARE PLOTTED.                                    *        
!                                                                      *        
!     ARGUMENTS:                                                       *        
!                                                                      *        
! MX/MY  - SEE LPLOT.                                                  *        
! IVERT  - DETERMINES WHETHER THE VERTICAL SCALE OF THE PLOT IS LINEAR *        
! OR QUASI-LOGARITHMIC.                                                *        
! NX     - NUMBER OF POINTS IN THE X-DIRECTION.                        *        
! NY     - NUMBER OF POINTS IN THE Y-DIRECTION.                        *        
! INCX   - SKIP PARAMETER FOR X.                                       *        
! INCY   - SKIP PARAMETER FOR Y.                                       *        
! Z      - 2-D ARRAY OF VERTICAL HEIGHTS.                              *        
! NDIM   - THE FIRST DIMENSION OF THE ARRAY Z.  HENCE: NX <= NDIM.     *        
! TITLE       - TITLE FOR THE GRAPH.                                   *         
! XNAME/YNAME - LABEL FOR THE X/Y-AXIS (REFERS TO INDICES!).           *        
! NTITLE/NXNAME/NYNAME - NUMBER OF CHARACTERS IN TITLE/XNAME/YNAME.    *        
!                                                                      *        
!     WRITTEN BY DAVE FORSLUND                                         *        
!     ADDED ARGUMENTS IVERT AND NDIM, INTRODUCED PARAMETERS DN AND FM, *        
!     IMPROVED SCALES, HGO 24/12/85.                                   *        
!***********************************************************************      
PARAMETER (DM=.57,DN=.91,EM=.82,FM=2.,GM=.1,HM=.33)     
 
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
DIMENSION Z(NDIM,*)   
CHARACTER*(*) TITLE,XNAME,YNAME       
CHARACTER*9 STRING  
 
CALL NFRAME(MX,MY,5,0.,1.,0.,1.,TITLE,NTITLE,' ',1,' ',1)        
 
!     * DETERMINE THE MAXIMUM OF Z (ZMAX2 IS THE NEXT LARGEST VALUE).  
ZMAX=0.    
ZMAX2=0.   
DO J=1,NY,INCY   
  DO I=1,NX,INCX         
    ZMAX2=AMAX1(ZMAX2,Z(I,J))
    IF(ZMAX2.GT.ZMAX) THEN 
      ZMAX2=ZMAX 
      ZMAX=Z(I,J)  
    ENDIF
  ENDDO
ENDDO
 
CALL DLCH(IXL,IYT-20,'ZMAX =',6,2)    
WRITE(STRING,'(1PE9.2)') ZMAX         
CALL DLCH(IXL,IYT-40,STRING,9,2)      
WRITE(STRING,'(1PE9.2)') ZMAX2        
CALL DLCH(IXL,IYT-60,STRING,9,2)      
 
!     * SCALE FACTORS.    
DMAX=(NX-1.)/DM     
EMAX=(NY-1.)/EM     
A=DMAX*(DN-DM)/(EMAX*EM)       
       
!     * DRAW X- AND Y-AXIS.        
CALL CONVRT(0.,I0,0.,DMAX,IXL,IXR)    
CALL CONVRT(0.,J0,0.,EMAX,IYB,IYT)    
CALL CONVRT(NX-1.,I1,0.,DMAX,IXL,IXR) 
CALL CONVRT(NX-1.+A*(NY-1.),I2,0.,DMAX,IXL,IXR)         
CALL CONVRT(NY-1.,J1,0.,EMAX,IYB,IYT) 
CALL DRV(real(I0),real(J0),real(I1),real(J0)) 
CALL DRV(real(I1),real(J0),real(I2),real(J1)) 
       
!     * SCALE AND LABEL X-AXIS.    
CALL DLCH(I0-6,J0-22,'1',1,2)  
IF(NX.LT.10) THEN   
  NC=1    
  WRITE(STRING,'(I1)') NX   
ELSEIF(NX.LT.100) THEN       
  NC=2    
  WRITE(STRING,'(I2)') NX   
ELSEIF(NX.LT.1000) THEN      
  NC=3    
  WRITE(STRING,'(I3)') NX   
ENDIF      
CALL DLCH(I1-NC*6,J0-22,STRING,NC,2)  
CALL DLCH((I0+I1)/2-NXNAME*6,J0-43,XNAME,NXNAME,2)      
       
!     * SCALE AND LABEL Y-AXIS.    
CALL DRV(real(I1),real(J0),real(I1+15),real(J0))       
CALL DRV(real(I2),real(J1),real(I2+15),real(J1))       
CALL DLCH(I1+20,J0-8,'1',1,2)  
IF(NY.LT.10) THEN   
  NC=1    
  WRITE(STRING,'(I1)') NY   
ELSEIF(NY.LT.100) THEN       
  NC=2    
  WRITE(STRING,'(I2)') NY   
  ELSEIF(NY.LT.1000) THEN      
  NC=3    
  WRITE(STRING,'(I3)') NY   
ENDIF      
  CALL DLCH(I2+20,J1-8,STRING,NC,2)     
  CALL DLCH((I1+I2)/2+50,(J0+J1)/2-8,YNAME,NYNAME,2)      
!       
!     * PLOT Z(I,J).      
  C=0.       
  IF(IVERT.EQ.1) THEN 
    IF(ZMAX.GT.0.) C=FM/ZMAX  
  ELSEIF(IVERT.EQ.2) THEN      
    IF(ZMAX2.GT.0.) C=1.-HM*ALOG10(ZMAX2)       
      GME=GM*EMAX      
    ENDIF      
    DO J=1,NY,INCY   
      E0=REAL(J-1)       
      D0=A*E0 
      DO I=1,NX,INCX         
        D=D0+REAL(I-1)  
        E=E0 
        IF(IVERT.EQ.1) THEN    
          IF(Z(I,J).GT.0.) E=E+AMAX1(0.,C*Z(I,J))        
        ELSEIF(IVERT.EQ.2) THEN         
          IF(Z(I,J).GT.0.) E=E+AMAX1(0.,GME*(C+HM*ALOG19(Z(I,J))))         
        ENDIF         
        CALL CONVRT(D,ID,0.,DMAX,IXL,IXR)        
        CALL CONVRT(E0,IE0,0.,EMAX,IYB,IYT)      
        CALL CONVRT(E,IE,0.,EMAX,IYB,IYT)        
        IF(IABS(IE-IE0).GT.1) THEN      
          CALL DRV(real(ID),real(IE),real(ID),real(IE0))
        ELSE 
          CALL DLCH(ID,-IE0,' ',46,2)  
        ENDIF  
      ENDDO
    ENDDO
       
RETURN     
END        
       
SUBROUTINE P3PLOT(MX,MY,R,TH,NR,NTH,F,NDIM,THX,THY,TITLE,NTITLE)   
!***********************************************************************        
!     SUBROUTINE P3PLOT PRODUCES A THREE-DIMENSIONAL POLAR PLOT OF THE *        
! FUNCTION F(R(I),TH(J)), I=1,..,NR, J=1,..,NTH.                       *        
!                                                                      *        
!     ARGUMENTS:                                                       *        
!                                                                      *        
! MX/MY  - SEE LPLOT.                                                  *        
! R      - 1D ARRAY OF RADIAL COORDINATES.                             *        
! TH     - 1D ARRAY OF ANGULAR COORDINATES (IN RADIANS).               *         
! NR     - NUMBER OF POINTS IN THE RADIAL DIRECTION.                   *        
! NTH    - NUMBER OF POINTS IN THE ANGULAR DIRECTION.                  *        
! F      - 2D ARRAY OF FUNCTION VALUES.                                *        
! NDIM   - THE FIRST DIMENSION OF THE ARRAY F.  HENCE: NR <= NDIM.     *        
! THX    - ANGLE (IN DEGREES!) AT WHICH THE X-AXIS IS TO BE DRAWN.     *        
! THY    - ANGLE (IN DEGREES!) BETWEEN THE X- AND Y-AXIS.              *        
! TITLE  - TITLE FOR THE GRAPH.                                        *         
! NTITLE - NUMBER OF CHARACTERS IN TITLE.                              *        
!                                                                      *        
!     WRITTEN BY R.M. FRANK, 1975.                                     *        
!     ADAPTED TO PPPLIB, HGO 24/04/86.                                 *        
!***********************************************************************      
COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
DIMENSION R(*),TH(*),F(NDIM,*) 
CHARACTER*(*) TITLE 
DIMENSION JXRL(3),JYTB(6),X(2),Y(2)   
CHARACTER*3 LAB(2,2)  
 
DATA JXRL/900,400,400/,JYTB/645,285,285,165,165,165/    
DATA LAB/' 0 ',' 90','180','270'/     
 
PI=3.1415926535898  
DGR=PI/180.         
 
!     * DETERMINE FRAME COORDINATES.        
CALL MAXV(R,NR,1,RMAX,IRM)     
CALL MAXAM(F,NDIM,NR,NTH,1,1,FMAX,IDUM,JDUM)   
SX=SIN(THX*DGR)       
CX=COS(THX*DGR)       
SXY=SIN((THY+THX)*DGR)         
CXY=COS((THY+THX)*DGR)         
XMX=RMAX*CX         
YMX=RMAX*SX         
DO J=1,71        
   THETA=J*5.*DGR   
   Q=SIN(THETA)       
   T=COS(THETA)       
   XMX=AMAX1(XMX,RMAX*(T*CX+Q*CXY))   
   YMX=AMAX1(YMX,RMAX*(T*SX+Q*SXY))   
ENDDO   
XMX=XMX*1.125       
XMN=-XMX   
YMN=-YMX   
S=ABS((YMX-YMN)/(2.*FMAX))     
DO J=1,NTH       
   Q=SIN(TH(J))*SXY+COS(TH(J))*SX     
   DO I=1,NR     
     YTEST=R(I)*Q+S*F(I,J)    
     YMX=AMAX1(YMX,YTEST)     
     YMN=AMIN1(YMN,YTEST)
 ENDDO
ENDDO
YMX=YMX*1.125       
YMN=YMN*1.125       
IMX=MOD(IABS(MX),10)  
IMY=MOD(IABS(MY),10)  
IF((XMX-XMN)/(YMX-YMN).LT.REAL(JXRL(IMX))/JYTB(IMY)) THEN        
   MXX=20+IMX       
   MYY=10+IMY       
ELSE       
   MXX=10+IMX       
   MYY=20+IMY       
ENDIF      
CALL NFRAME(MXX,MYY,5,XMN,XMX,YMN,YMX,TITLE,NTITLE,' ',1,' ',1)  
 
XFAC=real(IXR-IXL)/(XR-XL)         
YFAC=real(IYT-IYB)/(YT-YB)         
ZIX0=real(IXL)-(XL*XFAC)  
ZIY0=real(IYB)-(YB*YFAC)  
 
!     * DRAW BOUNDARY CURVE.       

ZIX1=ZIX0+(RMAX*CX*XFAC)      
ZIY1=ZIY0+(RMAX*SX*YFAC)      
L=0        
DO J=1,72        
   THETA=J*5.*DGR   
   Q=SIN(THETA)       
   T=COS(THETA)       
   ZIX=ZIX0+RMAX*(T*CX+Q*CXY)*XFAC 
   ZIY=ZIY0+RMAX*(Q*SXY+T*SX)*YFAC 
   CALL DASH(ZIX1,ZIY1,ZIX,ZIY,10,10,L,LL)         
   L=LL    
   ZIX1=ZIX  
   ZIY1=ZIY  
ENDDO   
 
!* DRAW AXES AND LABELS.      

RLAB=1.+60.*XMX/((IXR-IXL)*RMAX)      
DO K=1,2         
   DO L=1,2      
     THETA=(90.*(K-1)+180.*(L-1))*DGR         
     Q=SIN(THETA)    
     T=COS(THETA)    
     X(L)=RMAX*(T*CX+Q*CXY)   
     Y(L)=RMAX*(Q*SXY+T*SX)   
   ENDDO         
   CALL DASH(ZIX0+(X(1)*XFAC),ZIY0+(Y(1)*YFAC),       &       
   ZIX0+(X(2)*XFAC),ZIY0+(Y(2)*YFAC),10,10,0,IDUM) 
   DO L=1,2      
     ZIX=ZIX0+X(L)*RLAB*XFAC      
     ZIY=ZIY0+Y(L)*RLAB*YFAC      
     CALL DLCH(INT(ZIX-12),-INT(ZIY),LAB(K,L),3,2)
  ENDDO
ENDDO   
 
!     * DRAW BOUNDARY VERTICALS.   

DO J=1,NTH-1     
   THETA=TH(J)        
   Q=SIN(THETA)       
   T=COS(THETA)       
   ZIX =ZIX0+RMAX*(T*CX+Q*CXY)*XFAC         
   ZIY1=ZIY0+RMAX*(Q*SXY+T*SX)*YFAC        
   ZIY2=ZIY1+S*F(IRM,J)*YFAC       
   IF(ABS(ZIY2-ZIY1).GT.5) CALL DASH(ZIX,ZIY1,ZIX,ZIY2,0,5,0,IDUM)    
ENDDO   
       
!     * DRAW CENTER LINE. 

CALL MINV(R,NR,1,RMIN,IR0)     
IF(RMIN .EQ. 0.) CALL DASH(ZIX0,ZIY0,ZIX0,ZIY0+S*F(IR0,1)*YFAC,10,10,0,IDUM)  
     
!     * DRAW ANGULAR GRID LINES.   

DO I=1,NR        
  RI=R(I)   
  ZIX=ZIX0+RI*CX*XFAC    
  ZIY=ZIY0+(RI*SX+S*F(I,1))*YFAC  
  CALL MOVABS(ZIX,ZIY) 
  DO J=2,NTH    
    THETA=TH(J)     
    Q=SIN(THETA)    
    T=COS(THETA)    
    ZIX=ZIX0+RI*(T*CX+Q*CXY)*XFAC         
    ZIY=ZIY0+(RI*(Q*SXY+T*SX)+S*F(I,J))*YFAC       
    CALL DRWABS(ZIX,ZIY)
  ENDDO
ENDDO
       
!     * DRAW RADIAL GRID LINES.    
DO J=1,NTH-1     
   THETA=TH(J)        
   Q=SIN(THETA)       
   T=COS(THETA)       
   RI=R(1)   
   ZIX=ZIX0+RI*(T*CX+Q*CXY)*XFAC   
   ZIY=ZIY0+(RI*(Q*SXY+T*SX)+S*F(1,J))*YFAC 
   CALL MOVABS(ZIX,ZIY) 
   DO I=2,NR  
     RI=R(I)         
     ZIX=ZIX0+RI*(T*CX+Q*CXY)*XFAC         
     ZIY=ZIY0+(RI*(Q*SXY+T*SX)+S*F(I,J))*YFAC       
     CALL DRWABS(ZIX,ZIY)
  ENDDO
ENDDO   

RETURN     
END        
       

SUBROUTINE MAXV(A,N,INC,B,I)   
!***********************************************************************        
!     SUBROUTINE MAXV, AND ENTRIES MINV, MAXAV, MINAV DETERMINE THE    *        
! MAXIMUM, MINIMUM, MAXIMUM ABSOLUTE, MINIMUM ABSOLUTE VALUES OF AN    *        
! ARRAY OF REAL NUMBERS, RESPECTIVELY.  AS THE ARRAY IS SEARCHED, AN   *        
! INDEX IS UPDATED EACH TIME A LARGER VALUE OF A (IN THE CASE OF MAXV) *        
! IS ENCOUNTERED.  AFTER THE ARRAY IS SEARCHED, B IS SET TO THE VALUE  *        
! OF A WITH THE CALCULATED INDEX.                                      *        
!                                                                      *        
! A   - ONE-DIMENSIONAL INPUT ARRAY OF REAL NUMBERS.                   *        
! N   - NUMBER OF ELEMENTS IN THE ARRAY A.                             *        
! INC - SPACING AT WHICH ELEMENTS ARE TO BE EXAMINED.                  *        
! B   - MAXIMUM, MINIMUM, MAXIMUM ABSOLUTE OR MINIMUM ABSOLUTE VALUE   *        
!       OF A RETURNED TO THE CALLER.                                   *        
! I   - ELEMENT NUMBER OF MAXIMUM, MINIMUM, ETC. VALUE OF A (1<=I<=N). *        
!                                                                      *        
!     SEPARATED THE DIFFERENT ENTRIES, CHANGED THE MEANING OF N TO THE *        
!     PRESENT ONE, HGO 9/1/86.                                         *        
!***********************************************************************      
DIMENSION A(*)        
 
B=A(1)       
I=1        
DO K=1,N,INC     
   IF(A(K).GT.B) THEN        
      B=A(K) 
      I=K  
   ENDIF   
ENDDO   
RETURN     
 
ENTRY MINV(A,N,INC,B,I)        
B=A(1)       
I=1        
DO K=1,N,INC     
   IF(A(K).LT.B) THEN        
     B=A(K) 
     I=K  
   ENDIF   
ENDDO   
RETURN     
 
ENTRY MAXAV(A,N,INC,B,I)       
B=ABS(A(1))  
I=1        
DO K=1,N,INC     
   S=ABS(A(K))        
   IF(S.GT.B) THEN  
     B=S  
     I=K  
   ENDIF   
ENDDO   
B=A(I)       
RETURN     
 
ENTRY MINAV(A,N,INC,B,I)       
B=ABS(A(1))  
I=1        
DO K=1,N,INC     
   S=ABS(A(K))        
   IF(S.LT.B) THEN  
     B=S  
     I=K  
   ENDIF   
ENDDO   
B=A(I)       

RETURN     
END        
       
SUBROUTINE MAXM(A,IA,M,N,INCK,INCL,B,I,J)        
!***********************************************************************        
!     SUBROUTINE MAXM, AND ENTRIES MINM, MAXAM, MINAM DETERMINE THE    *        
! MAXIMUM, MINIMUM, MAXIMUM ABSOLUTE, AND MINIMUM ABSOLUTE ELEMENT AND *        
! THE INDICES OF THAT ELEMENT IN MATRIX A.                             *        
!                                                                      *        
! A    - TWO-DIMENSIONAL INPUT ARRAY.                                  *        
! IA   - MAXIMUM LENGTH OF THE FIRST ARGUMENT OF A AS SPECIFIED IN THE *        
!        DIMENSION STATEMENT, I.E. DIMENSION A(IA,JA).                 *        
! M    - NUMBER OF COLUMNS (1ST ARGUMENT).                             *        
! N    - NUMBER OF ROWS (2ND ARGUMENT).                                *        
! INCK - SKIP PARAMETER FOR THE 1ST ARGUMENT.                          *        
! INCL - SKIP PARAMETER FOR THE 2ND ARGUMENT.                          *        
! B    - CONTAINS THE DESIRED ELEMENT.                                 *        
! I    - FIRST INDEX TO THE RESULTANT ELEMENT.                         *        
! J    - SECOND INDEX TO THE RESULTANT ELEMENT.                        *        
!                                                                      *        
!     SEPARATED THE DIFFERENT ENTRIES, ADDED ARGUMENTS INCK AND NCL,   *        
!     HGO 9/1/86.                                                      *        
!***********************************************************************      
DIMENSION A(IA,*)     
 
B=A(1,1)     
I=1        
J=1        
DO K=1,M,INCK    
   DO L=1,N,INCL 
     IF(A(K,L).GT.B) THEN   
       B=A(K,L)     
       I=K        
       J=L        
     ENDIF
 ENDDO
ENDDO

RETURN     
 
ENTRY MINM(A,IA,M,N,INCK,INCL,B,I,J)  
B=A(1,1)     
I=1        
J=1        
DO K=1,M,INCK    
   DO  L=1,N,INCL 
     IF(A(K,L).LT.B) THEN   
       B=A(K,L)     
       I=K        
       J=L        
     ENDIF
  ENDDO
ENDDO

RETURN     
 
ENTRY MAXAM(A,IA,M,N,INCK,INCL,B,I,J) 
B=ABS(A(1,1))         
I=1        
J=1        
DO K=1,M,INCK    
   DO L=1,N,INCL 
     S=ABS(A(K,L))   
     IF (S.GT.B) THEN        
       B=S        
       I=K        
       J=L        
     ENDIF
   ENDDO
ENDDO

B=A(I,J)     
RETURN     
       
ENTRY MINAM(A,IA,M,N,INCK,INCL,B,I,J) 
B=ABS(A(1,1))         
I=1        
J=1        
DO K=1,M,INCK    
  DO L=1,N,INCL 
     S=ABS(A(K,L))   
     IF (S.LT.B) THEN        
       B=S        
       I=K        
       J=L        
     ENDIF
  ENDDO
ENDDO
B=A(I,J)     
RETURN     
END        
 

FUNCTION ALOG19(ARG)      
!***********************************************************************        
!     SPECIAL ALOG10 TO PREVENT ERROR ON ZERO OR NEGATIVE ARGUMENT.    *        
!     BOB MALONE, 12/08/78                                             *        
!***********************************************************************        
 
IF(ARG.LT.1.E-50) THEN       
   ALOG19=-50.      
ELSE       
   ALOG19=ALOG10(ARG) 
ENDIF      
RETURN     
END        


SUBROUTINE LBLTOP(LABEL,NLABEL)         
!***********************************************************************        
!     SUBROUTINE LBLTOP ENABLES A USER TO SPECIFY AN 80-CHARACTER      *        
! LABEL AT THE TOP OF A PAGE OF PLOTS.  ENTRY LBLBOT ENABLES A USER TO *        
! SPECIFY A 40-CHARACTER LABEL AT THE BOTTOM OF A PLOTTING PAGE AND TO *        
! WRITE DATE AND TIME IN THE LEFT CORNER OF THE PAGE.  THE LABELS ARE  *        
! CENTERED WITH RESPECT TO THE WIDTH OF THE PAGE.  LBLTOP AND LBLBOT   *        
! SHOULD BE CALLED BEFORE ANY OTHER PLOT CALLS FOR A PAGE.  ONCE A     *        
! LABEL CALL IS GIVEN, IT REMAINS IN EFFECT FOR EACH SUCCEEDING PAGE   *        
! UNTIL ANOTHER CALL WITH A DIFFERENT CHARACTER STRING IS GIVEN.       *        
!***********************************************************************        
COMMON /LHEAD1/LABTOP,LABBOT,D,T      
CHARACTER LABTOP*80,LABBOT*40,D*10,T*8 
COMMON /LHEAD2/NCT,NCB       
CHARACTER*(*) LABEL 
 
NCT=ISIGN(MIN(IABS(NLABEL),80),NLABEL)         
LABTOP=LABEL        
RETURN     
 
ENTRY LBLBOT(LABEL,NLABEL)     
NCB=ISIGN(MIN(IABS(NLABEL),40),NLABEL)         
LABBOT=LABEL        
CALL DATI(D,T)        
RETURN     
END        
       
SUBROUTINE NFRAME(MX,MY,IOP,XMIN,XMAX,YMIN,YMAX,TITLE,NTITLE,XNAME,NXNAME,YNAME,NYNAME)         
!***********************************************************************        
!     NFRAME IS THE INTERFACE DRIVER FOR THE HIGH-LEVEL ROUTINES, SUCH *        
! AS LPLOT, AND THE LOW-LEVEL ROUTINES, SUCH AS SBLIN, WHICH SCALES    *        
! THE BOTTOM BOUNDARY OF THE PLOT LINEARLY.  NFRAME DEFINES THE GRAPH  *        
! AREA AND THE SCALING ALONG X AND Y FROM THE MX AND MY VALUES PASSED  *        
! TO IT.  IT HAS THE GRID, SPECIFIED IN THE IOP VALUE, DRAWN AND THE   *        
! AXES SCALED ACCORDING TO THE MINIMUM AND MAXIMUM VALUES OF THE X AND *        
! Y ARRAYS AND THE GIVEN IOP.  IT HANDLES PLACEMENT OF THE TITLES OF   *        
! THE PLOT AND THE AXES.  IT ALSO PLACES THE TOP AND BOTTOM LABELS ON  *        
! THE PAGE.  IT STORES THE SCALING INFORMATION IN COMMON BLOCK CJE07   *        
! FOR FUTURE CALLS TO THE SAME (IMX,IMY) FRAME ON THE PAGE.  NFRAME    *        
! AUTOMATICALLY ADVANCES A PAGE WITH THE FIRST PLOT THAT EXTENDS INTO  *        
! THE UPPER LEFT-HAND CORNER.                                          *        
!                                                                      *        
!     ARGUMENTS:                                                       *        
!                                                                      *        
! MX     - DEFINES THE GRAPH AREA AND THE SCALING IN THE X-DIRECTION   *        
! ACCORDING TO THE FORMULA                                             *        
!    IABS(MX) = IIX*1000 + IAX*100 + ISX*10 + IMX ,                    *        
! WHERE IMX DETERMINES THE HORIZONTAL EXTENSION OF THE PLOT:           *        
!    IMX = 1 - FULL PAGE                                               *        
! 2 - LEFT HALF OF THE PAGE                                            *        
! 3 - RIGHT HALF OF THE PAGE,                                          *        
! AND ISX DETERMINES THE SCALING ALONG THE X-AXIS:                     *         
!    ISX = 0 - AUTOMATIC SCALING WITH EXPANSION (DEFAULT)              *        
! 1 - EXACT SCALING (NO ROUNDING)                                      *        
! 2 - EQUIDISTANT SCALING WITH THE X-SCALE ADAPTED                     *        
!     TO THE LENGTHS ALONG Y (SEE NOTE IN NFRAME),                     *         
! AND IAX PROVIDES AN ADDITIONAL OPTION:                               *        
!    IAX = 0 - NO ACTION (DEFAULT)                                     *        
! 1 - X=0 AXIS IS DRAWN  (IF IT LIES IN THE RANGE)                     *        
! 2 - X=0 AXIS IS DASHED (IF IT LIES IN THE RANGE),                    *        
! AND IIX OVERRULES THE DEFAULT NUMBER OF SCALE INTERVALS:             *        
!    IIX = 0 - 4 INTERVALS FOR SCALES AND TICKMARKS (DEFAULT)          *        
!    IIX > 0 - IIX INTERVALS (NOT FOR AUTOMATIC SCALING).              *        
! MX < 0 : PLOTTING OF SCALES AND TICK MARKS SUPPRESSED.               *        
! MY     - DEFINES THE GRAPH AREA AND THE SCALING IN THE Y-DIRECTION,  *        
! ANALOGOUS TO THE ABOVE EXPRESSIONS WITH X REPLACED BY Y,             *        
! WHERE IMY DETERMINES THE VERTICAL EXTENSION OF THE PLOT:             *        
!    IMY = 1 - FULL PAGE                                               *        
! 2 - TOP HALF OF THE PAGE                                             *        
! 3 - BOTTOM HALF OF THE PAGE                                          *        
! 4 - TOP THIRD OF THE PAGE                                            *        
! 5 - MIDDLE THIRD OF THE PAGE                                         *        
! 6 - BOTTOM THIRD OF THE PAGE.                                        *        
! IOP    - EQUALS THE SCALING OPTION JOP OF SUBROUTINE LPLOT:          *        
!    JOP = 1 - LINEAR X-AXIS, LINEAR Y-AXIS                            *        
! 2 - LINEAR X-AXIS, LOG Y-AXIS                                        *        
! 3 - LOG X-AXIS, LINEAR Y-AXIS                                        *        
! 4 - LOG X-AXIS, LOG Y-AXIS                                           *        
! 5 - LINEAR X-AXIS, LINEAR Y-AXIS (BUT PLOTTING OF                    *         
!     FRAME, SCALES, AND TICK MARKS SUPPRESSED).                       *        
! XMIN   - MINIMUM VALUE OF X.                                         *        
! XMAX   - MAXIMUM VALUE OF X.                                         *        
! YMIN   - MINIMUM VALUE OF Y.                                         *        
! YMAX   - MAXIMUM VALUE OF Y.                                         *        
! THESE FOUR EXTREME VALUES ARE EITHER PRESCRIBED BY THE USER          *        
! THROUGH A DIRECT CALL OF NFRAME (FOLLOWED BY CALLS TO LPLOT          *        
! WITH NPTS < 0) OR DETERMINED AUTOMATICALLY BY LPLOT ITSELF.          *        
! TITLE  - TITLE FOR THE GRAPH.                                        *        
! NTITLE - THE NUMBER OF CHARACTERS IN NTITLE.                         *        
! XNAME  - LABEL FOR THE X-AXIS.                                       *        
! NXNAME - NUMBER OF CHARACTERS IN XNAME.                              *        
! YNAME  - LABEL FOR THE Y-AXIS.                                       *        
! NYNAME - NUMBER OF CHARACTERS IN YNAME.                              *        
! THE ABOVE THREE CHARACTER STRINGS ARE AUTOMATICALLY TRUN-            *         
! CATED TO FIT ALONGSIDE THE CHOSEN FRAME.  THE FONT MAY BE            *        
! CHANGED ACCORDING TO THE RULES GIVEN IN DLCH.                        *         
!                                                                      *        
!     NOTE:                                                            *        
! THE SCALING ISX/Y=2 IS USED TO PRESERVE THE RELATIVE PROPORTIONS OF  *        
! GEOMETRIC FIGURES.  E.G., A STANDING ELLppp_unit_psE X=COS(T), Y=1.5*SIN(T)  *        
! IS PLOTTED ON THE LEFT HALF OF THE PAGE WITH THESE CALLS:            *        
!     "CALL NFRAME(22,11,1,-1.,1.,-2.,2.,'ELLppp_unit_psE',7,'X',1,'Y',1)",    *        
!     "CALL LPLOT(2,1,1,X,Y,-NPTS,1,' ',0,' ',0,' ',0)".               *        
! SINCE THE RANGE OF X IS DETERMINED AUTOMATICALLY IN THIS CASE, THE   *        
! PARAMETERS XMIN=-1.0 AND XMAX=1.0 ONLY FIX THE CENTRAL VALUE X=0.    *        
!                                                                      *        
!     OFRAME(MX,MY) IS AN ENTRY POINT INTO NFRAME; IT RESTORES THE     *        
! PLOTTING COMMON CJE07 TO THE CONDITIONS OF THE PARTICULAR (IMX,IMY)  *        
! PLOT DETERMINED BY THE LAST CALL TO NFRAME, TO PLOT A SECOND AND     *        
! THIRD CURVE ON THE SAME PLOT.                                        *        
!                                                                      *        
!     SETADV(IA) IS ANOTHER ENTRY POINT INTO NFRAME; IT OVERRIDES THE  *        
! AUTOMATIC ADVANCE AND PRINTING OF TOP AND BOTTOM LABELS (IN EFFECT   *        
! FOR THE DEFAULT VALUE IA = 0).  IA = 1 / -1 : ADVANCE / NO ADVANCE,  *        
! IRRESPECTIVE OF THE VALUES OF IMX AND IMY.                           *         
!                                                                      *        
!     WRITTEN BY CLAIR NIELSON.                                        *        
!     MODIFIED BY DEBBY HYMAN 2/80 FOR SCALING AXES NICELY.            *        
!     MODIFIED BY DICK HOGEWEIJ 21/06/84 FOR EQUIDISTANT SCALINGS.     *        
!     MODIFIED BY HANS GOEDBLOED 14/11/85 FOR ADAPTATION TO NEW DLCH,  *        
!     NEW MEANING OF ARGUMENTS MX,MY, IMPROVED EQUIDISTANT SCALING,    *        
!     IMPROVED HANDLING OF THE LABELS.                                 *        
!     NEW FRAME ADVANCE, ROUNDING OF LOG SCALES, HGO 4/11/91.          *        
!***********************************************************************        
 
COMMON /KPOS/KP(36)   
COMMON /LHEAD1/LABTOP,LABBOT,D,T      
CHARACTER LABTOP*80,LABBOT*40,D*10,T*8 
COMMON /LHEAD2/NCT,NCB       
CHARACTER*(*) TITLE,XNAME,YNAME       
DIMENSION JXL(6),JXR(6),JYB(6),JYT(6),IXL(36),IXR(36),IYB(36),IYT(36),   &  
 XL(36),XR(36),YB(36),YT(36),NPOS(36,36) 
LOGICAL FLOGX,FLOGY 
INTEGER ASW    
REAL XMIN,XMAX,YMIN,YMAX
SAVE IXL,IXR,IYB,IYT,XL,XR,YB,YT,ASW,NPOS      
 
!     * FRAME COORDINATES.         

DATA JXL/ 90, 90,590, 90, 423, 756/        
DATA JXR/990,490,990,323, 656, 990/        
DATA JYB/ 77,437, 77,557,317, 77/     
DATA JYT/722,722,362,722,482,242/     
DATA ASW/0/         
 
!     * DECISION TABLE FOR FRAME ADVANCE.   
! M:       1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18        
! MX,MY:  11 12 13 14 15 16 21 22 23 24 25 26 31 32 33 34 35 36        
!
DATA ((NPOS(N,M),N=1,18),M=1,18) &    
  / 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   &    
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   &     
    1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1,   &    
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   &    
    1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0,   &    
    1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,   &    
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   &    
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   &    
    1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   &    
    1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,   &    
    1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,   &    
    1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0,   &    
    1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1,   &    
    1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0,   &    
    1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0,   &    
    1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1/ 

! M:      19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36         
! MX,MY:  41 42 43 44 45 46 51 52 53 54 55 56 61 62 63 64 65 66  
DATA ((NPOS(N,M),N=1,18),M=19,36)         &
 /  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   &    
    1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0,   &    
    1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1,   &     
    1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1,   &    
    1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1,   &    
    1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0,   &    
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,   &    
    1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1 /  
DATA ((NPOS(N,M),N=19,36),M=1,18)         &
! M:      19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36         
! MX,MY:  41 42 43 44 45 46 51 52 53 54 55 56 61 62 63 64 65 66        
 /  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   &    
    1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0,   &    
    1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1,   &    
    1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1,   &    
    1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 1,   &    
    1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0,   &    
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,   &    
    1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1,   &     
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0,   &    
    1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 1/
DATA ((NPOS(N,M),N=19,36),M=19,36)        &
 /  1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   &    
    1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   &    
    1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   &    
    1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,   &    
    0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0,   &    
    0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,   &    
    0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0,   &    
    0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,   &    
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,   &    
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0,   &    
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1,   &    
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0,   &    
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0,   &    
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1/         
       
!     * INPUT PARAMETERS. 

IMX=MOD(IABS(MX),10)  
IMY=MOD(IABS(MY),10)  
ISX=MOD(IABS(MX)/10,10)        
ISY=MOD(IABS(MY)/10,10)        
IAX=MOD(IABS(MX)/100,10)       
IAY=MOD(IABS(MY)/100,10)       
IIX=MOD(IABS(MX)/1000,10)      
IIY=MOD(IABS(MY)/1000,10)      
M=IMY+6*IMX-6       
IF(M.GT.36) STOP '*** NFRAME: IMX OR IMY TOO BIG ***'   
JOP=MOD(IABS(IOP),10) 
FLOGX=.FALSE.       
FLOGY=.FALSE.       
IF(JOP.EQ.3.OR.JOP.EQ.4) FLOGX=.TRUE. 
IF(JOP.EQ.2.OR.JOP.EQ.4) FLOGY=.TRUE. 
 
!     * ADVANCE A PAGE AND DRAW TOP AND BOTTOM LABELS.        
IADV=0     
DO N=1,36        
   IADV=IADV+KP(N)*NPOS(N,M)   
ENDDO

IF((ASW.EQ.0.AND.IADV.NE.0).OR.(ASW.EQ.1)) THEN         
   CALL ADV(1)   
   DO N=1,36     
      KP(N)=0
   ENDDO
ENDIF    
!      IF (MAXVAL(KP).GT.0) THEN        
ICEN=(JXR(1)+JXL(1))/2    
CALL DLCH(ICEN-6*IABS(NCT),766,LABTOP,NCT,-2)         
CALL DLCH(ICEN-9*IABS(NCB),0,LABBOT,NCB,-3)  
CALL DLCH(25,17,D,10,-1)      
CALL DLCH(25,2,T,8,-1)       
!      ENDIF        
IF(ASW.NE.0) ASW=0  
KP(M)=1    

!       
!     * COMPUTE FRAME COORDINATES. 
IXL(M)=JXL(IMX)       
IXR(M)=JXR(IMX)       
IYB(M)=JYB(IMY)       
IYT(M)=JYT(IMY)       
!     * B GODFREY'S IDEA TO FORCE NORMALIZATION:     
XLN=XMIN+0.0        
XRN=XMAX+0.0        
IF(XLN.EQ.XRN) THEN 
  XLN=(1.0-SIGN(0.5,XLN))*XLN-1.0E-6 
  XRN=(1.0+SIGN(0.5,XRN))*XRN+1.0E-6 
ENDIF      
IF(FLOGX) THEN      
  XLN=ALOG19(XLN)    
  XRN=ALOG19(XRN)    
!        * LIMIT DECADES PLOTTED TO MOST SIGNIFICANT, HEWETT 6/9/83    
  XLN=AMAX1(XLN,XRN-24.)      
ENDIF      
YBN=YMIN+0.0        
YTN=YMAX+0.0   
IF(YBN.EQ.YTN) THEN 
  YBN=(1.0-SIGN(0.5,YBN))*YBN-1.0E-6 
  YTN=(1.0+SIGN(0.5,YTN))*YTN+1.0E-6 
ENDIF      
IF(FLOGY) THEN      
  YBN=ALOG19(YBN)    
  YTN=ALOG19(YTN)    
!        * LIMIT DECADES PLOTTED TO MOST SIGNIFICANT, HEWETT 6/9/83    
  YBN=AMAX1(YBN,YTN-24.)      
ENDIF      
XL(M)=XLN  
XR(M)=XRN  
YB(M)=YBN  
YT(M)=YTN      
!     * IF XMAX <= XMIN FOR A PARTICULAR FRAME,      
!     * MAKE XMAX = XMIN + 1.0 AT LEAST.    
IF(XR(M).LE.XL(M)) XR(M)=XL(M)+AMAX1(1.,XL(M)) 
IF(YT(M).LE.YB(M)) YT(M)=YB(M)+AMAX1(1.,YB(M)) 
       
!     * NUMBER OF INTERVALS NX FOR EXACT AND AUTOMATIC SCALING.        
NX=4       
IF(ISX.NE.0.AND.IIX.NE.0) NX=IIX      
!     * AUTOMATIC SCALING.         
IF(ISX.EQ.0.AND.(.NOT.FLOGX)) THEN    
  CALL ASCL(3,XL(M),XR(M),NX,IDUM,JDUM)       
  IF(NX.GT.5) THEN 
    IDIFF=XR(M)-XL(M)        
    IF(MOD(IDIFF,5).EQ.0) NX=5      
    IF(MOD(IDIFF,4).EQ.0) NX=4      
    IF(MOD(IDIFF,3).EQ.0) NX=3      
  ENDIF   
ENDIF      
!     * ROUNDING OF LOG SCALES.    
IF(ISX.EQ.0.AND.(FLOGX)) THEN         
  XL(M)=AMIN1(AINT(XL(M)),SIGN(AINT(ABS(XL(M))+.999),XL(M)))    
  XR(M)=AMAX1(AINT(XR(M)),SIGN(AINT(ABS(XR(M))+.999),XR(M)))    
ENDIF      
!       
!     * NUMBER OF INTERVALS NY FOR EXACT AND AUTOMATIC SCALING.        
NY=4       
IF(ISY.NE.0.AND.IIY.NE.0) NY=IIY      
!     * AUTOMATIC SCALING.         
  IF(ISY.EQ.0.AND.(.NOT.FLOGY)) THEN    
    CALL ASCL(3,YB(M),YT(M),NY,IDUM,JDUM)       
    IF(NY.GT.5) THEN 
      IDIFF=YT(M)-YB(M)        
      IF(MOD(IDIFF,5).EQ.0) NY=5      
        IF(MOD(IDIFF,4).EQ.0) NY=4      
        IF(MOD(IDIFF,3).EQ.0) NY=3      
      ENDIF   
    ENDIF      
!     * ROUNDING OF LOG SCALES.    
    IF(ISY.EQ.0.AND.(FLOGY)) THEN         
      YB(M)=AMIN1(AINT(YB(M)),SIGN(AINT(ABS(YB(M))+.999),YB(M)))    
      YT(M)=AMAX1(AINT(YT(M)),SIGN(AINT(ABS(YT(M))+.999),YT(M)))    
    ENDIF      
       
!     * INITIALIZE COORDINATES OF THE EXTREME TICK MARKS.     
    IXLX=IXL(M)  
    IXRX=IXR(M)  
    IYBX=IYB(M)  
    IYTX=IYT(M)  
    XLX=XL(M)    
    XRX=XR(M)    
    YBX=YB(M)    
    YTX=YT(M)    
       
!     * EQUIDISTANT SCALING WITH X-SCALE ADAPTED; ADDED 210684 GMDH.   
    IF(ISX.EQ.2.AND.(.NOT.FLOGX)) THEN    
      IF(ISY.EQ.2) STOP '*** NFRAME: ISX=ISY=2 FORBIDDEN ***'       
!        * CENTER X-INTERVAL WITH RESPECT TO PLOTTING AREA; HGO 25/11/85        
        XMID=.5*(XL(M)+XR(M))       
        FAC=(YT(M)-YB(M))*(IXR(M)-IXL(M))/(IYT(M)-IYB(M))    
        XL(M)=XMID-.5*FAC         
        XR(M)=XMID+.5*FAC         
        IDIVY=(IYT(M)-IYB(M))/NY  
        DIVY=(YT(M)-YB(M))/NY     
        RMULT=(XL(M)-YB(M))/DIVY  
        MULT=RMULT       
        IF(MULT.LT.RMULT) MULT=MULT+1      
        XLX=YB(M)+MULT*DIVY       
        IXLX=IXL(M)+(XLX-XL(M))*(IXR(M)-IXL(M))/(XR(M)-XL(M))         
        NX=0    
        XRX=XLX 
  30    IF((XRX+DIVY).LE.XR(M)) THEN       
          XRX=XRX+DIVY  
          NX=NX+1       
          GOTO 30       
        ENDIF   
        IXRX=IXLX+IDIVY*NX        
      ENDIF      
       
!     * EQUIDISTANT SCALING WITH Y-SCALE ADAPTED; ADDED 210684 GMDH.   
      IF(ISY.EQ.2.AND.(.NOT.FLOGY)) THEN    
!        * CENTER Y-INTERVAL WITH RESPECT TO PLOTTING AREA; HGO 25/11/85        
         YMID=.5*(YB(M)+YT(M))       
         FAC=(XR(M)-XL(M))*(IYT(M)-IYB(M))/(IXR(M)-IXL(M))    
         YB(M)=YMID-.5*FAC         
         YT(M)=YMID+.5*FAC         
         IDIVX=(IXR(M)-IXL(M))/NX  
         DIVX=(XR(M)-XL(M))/NX     
         RMULT=(YB(M)-XL(M))/DIVX  
         MULT=RMULT       
         IF(MULT.LT.RMULT) MULT=MULT+1      
         YBX=XL(M)+MULT*DIVX       
         IYBX=IYB(M)+(YBX-YB(M))*(IYT(M)-IYB(M))/(YT(M)-YB(M))         
         NY=0    
         YTX=YBX 
   40    IF((YTX+DIVX).LE.YT(M)) THEN       
           YTX=YTX+DIVX  
           NY=NY+1       
           GOTO 40       
         ENDIF   
         IYTX=IYBX+IDIVX*NY        
      ENDIF      
       
!     * DEFINE THE GRAPH AREA AND EXTREME TICK MARKS.         
      CALL DGA(IXL(M),IXR(M),IYB(M),IYT(M),XL(M),XR(M),YB(M),YT(M))    
      CALL DGAX(IXLX,IXRX,IYBX,IYTX,XLX,XRX,YBX,YTX) 
       
!     * SUPPRESS PLOTTING OF THE SCALES IF MX/Y < 0. 
      IF(MX.LT.0) NX=0    
      IF(MY.LT.0) NY=0    
       
!     * DRAW FRAME, SCALES, AND TICK MARKS (EXCEPT FOT JOP=5).         
      IF(JOP.EQ.1) THEN   
         CALL DLNLN(NX,NY,1,IAX,IAY) 
         CALL SBLIN(NX)     
         CALL SLLIN(NY)     
      ELSEIF(JOP.EQ.2) THEN        
         CALL DLNLG(NX,NY)  
         CALL SBLIN(NX)     
         CALL SLLOG(NY)     
      ELSEIF(JOP.EQ.3) THEN        
         CALL DLGLN(NX,NY)  
         CALL SBLOG(NX)     
         CALL SLLIN(NY)     
      ELSEIF(JOP.EQ.4) THEN        
         CALL DLGLG(NX,NY)  
         CALL SBLOG(NX)     
         CALL SLLOG(NY)     
      ELSEIF(JOP.EQ.5) THEN        
!        * DRAW X/Y=0 AXIS WHEN IAX/Y.NE.0. 
         CALL DLNLN(0,0,0,IAX,IAY)   
      ENDIF      
       
!     * DRAW TITLE AND LABELS OF THE AXES.  
!     * MAXIMUM NUMBER OF CHARACTERS FITTING ALONG THE FRAME: 
      MCX=33     
      IF(IMX.EQ.1) MCX=75 
      MCY=13     
      IF(IMY.EQ.2.OR.IMY.EQ.3) MCY=23       
      IF(IMY.EQ.1) MCY=53 
!     * TRUNCATE IF THE STRING IS TOO LONG, WHILE ACCOUNTING FOR THE   
!     * DIFFERENT MEANING OF THE ARGUMENTS FOR SINGLE CHARACTER CODING:         
      NXNAM1=ISIGN(MIN(IABS(NXNAME),MCX-6),NXNAME)   
      IXNAME=(IXL(M)+IXR(M))/2-6*IABS(NXNAM1)        
      IF((LEN(XNAME).EQ.1).AND.(NXNAME.NE.1)) THEN   
         NXNAM1=NXNAME    
         IXNAME=(IXL(M)+IXR(M))/2-6         
      ENDIF      
      NYNAM1=ISIGN(MIN(IABS(NYNAME),MCY),NYNAME)     
      IYNAME=(IYB(M)+IYT(M))/2-6*IABS(NYNAM1)        
      IF((LEN(YNAME).EQ.1).AND.(NYNAME.NE.1)) THEN   
         NYNAM1=NYNAME    
         IYNAME=(IYB(M)+IYT(M))/2-6         
      ENDIF      
      NTITL1=ISIGN(MIN(IABS(NTITLE),MCX),NTITLE)     
!GTA      ITITLE=(IXL(M)+IXR(M))/2-6*IABS(NTITL1)  
      ITITLE = IXL(M)   
      ICHARSIZE = 2
      IF ((IMX.GE.4).AND.(IMY.GE.4)) ICHARSIZE= 1   
      CALL DLCH(IXNAME,IYB(M)-43,XNAME,NXNAM1,-2)     
      CALL DLCV(IXL(M)-64,IYNAME,YNAME,NYNAM1,-2)     
      CALL DLCH(ITITLE,IYT(M)+8,TITLE,NTITL1,-ICHARSIZE)      
      RETURN     
       
!     * ENTRY FOR RESTORING PLOTTING COMMON CJE07.   
      ENTRY OFRAME(MX,MY)   
      IMX=MOD(IABS(MX),10)  
      IMY=MOD(IABS(MY),10)  
      M=IMY+6*IMX-6       
      IF(M.GT.36) STOP '*** OFRAME: IMX OR IMY TOO BIG ***'   
      CALL DGA(IXL(M),IXR(M),IYB(M),IYT(M),XL(M),XR(M),YB(M),YT(M))    
      RETURN     
       
!     * ENTRY FOR MANUAL ADVANCE BEFORE NEXT PLOT.   
      ENTRY SETADV(IA)      
      ASW=IA     
RETURN     
END        
      
SUBROUTINE ASCL(M,ZMIN,ZMAX,MAJOR,MINOR,KF)      
!***********************************************************************        
!     THIS ROUTINE PROVIDES THE AUTOMATIC SCALING OF THE GRAPH BOUN-   *        
! DARIES TO ROUNDED DECIMAL NUMBERS AND COMPUTES THE ASSOCIATED PARAM- *        
! ETERS FOR THE LINEAR GRID DRAWING SUBROUTINES.                       *        
!                                                                      *        
! M     - ON INPUT, MINIMUM NUMBER OF MAJOR INTERVALS (1 <= M <= 20).  *        
!         IT IS SUGGESTED THAT M BE FAIRLY SMALL (E.G. 4 OR 5) IN      *        
!         ORDER TO PREVENT THE NUMERICAL SCALE FROM RUNNING TOGETHER.  *        
!         THIS DEPENDS ON HOW MUCH OF THE PLOTTING AREA IS TO BE USED  *        
!         AND ON THE NUMBER OF CHARACTERS WHICH WILL BE NEEDED FOR     *        
!         EACH SCALE NUMBER.                                           *        
! ZMIN  - ON INPUT, THE VALUE OF THE SMALLER ENDPOINT.                 *        
!         ON OUTPUT, THE VALUE OF THE NEW SMALLER ENDPOINT.            *        
! ZMAX  - ON INPUT, THE VALUE OF THE LARGER ENDPOINT.                  *        
!         ON OUTPUT, THE VALUE OF THE NEW LARGER ENDPOINT.             *        
! MAJOR - ON OUTPUT, THE NUMBER OF MAJOR INTERVALS AT WHICH TO PLACE   *        
!         TICK MARKS AND A NUMERIC SCALE.                              *        
! MINOR - ON OUTPUT, THE NUMBER OF MINOR INTERVALS AT WHICH TO PLACE   *        
!         TICK MARKS AND A NUMERIC SCALE.                              *        
! KF    - ON OUTPUT, THE FORMAT CODE DESCRIBING THE NUMBER OF DIGITS   *        
!         NECESSARY TO DISPLAY THE SCALE NUMBERS UNIQUELY.  KF IS AN   *        
!         INTEGER (0 <= KF <= 6 OR 10 <= KF <= 16) SUCH THAT THE UNITS *        
!         DIGIT SPECIFIES THE NUMBER OF DIGITS TO BE PRINTED TO THE    *        
!         RIGHT OF THE DECIMAL POINT.  A TENS DIGIT OF ZERO INDICATES  *        
!         FIXED POINT FORMAT (F FORMAT) AND A TENS DIGIT OF ONE INDI-  *        
!         CATES FLOATING POINT FORMAT (E FORMAT).  THIS FORMAT CODE    *        
!         WAS USED PREVIOUSLY FOR PLACING A NUMERIC SCALE ALONG  THE   *        
!         GRAPH BOUNDARY USING THE SCALE BOUNDARY ROUTINES SBLIN AND   *        
!         SLLIN.  THE PRESENT VERSIONS OF THE LATTER SUBROUTINES DO    *        
!         NOT HAVE THIS INPUT ARGUMENT ANYMORE.                        *        
!***********************************************************************      
      Z1=ZMIN    
      Z2=ZMAX    
      AM=M       
       
!     * ZMAX <= ZMIN, M <= 0, AND M > 20 ARE INVALID VALUES: RETURN.   
      IF((Z2.LE.Z1).OR.(M.LE.0.OR.M.GT.20)) THEN     
         MAJOR=0 
         MINOR=0 
         KF=0    
         RETURN  
      ENDIF      
!       
      IF(Z2.NE.0.AND.Z1.NE.0) THEN 
         ZBAR=Z2/Z1       
         IF(ABS(ZBAR).GE.1000.) THEN        
           Z1=0.         
         ELSEIF(ABS(ZBAR).LE..001) THEN     
           Z2=0.         
         ELSEIF(ABS(ZBAR-1.).LE..000005*AM) THEN     
           ZBAR=(Z2+Z1)/2.        
           Z=.0000026*AM*ABS(ZBAR)  
           Z2=ZBAR+Z     
           Z1=ZBAR-Z     
           GOTO 10       
         ENDIF   
      ENDIF      
      IF(Z2-Z1.NE.AM) THEN         
         Z2=Z2-.000001*ABS(Z2)       
         Z1=Z1+.000001*ABS(Z1)       
      ENDIF      
   10 P=(Z2-Z1)/AM        
      IFLAG=0    
      TENK=1.    
      K=0        
      IF(P.LT.1.) THEN    
         IFLAG=1 
         P=1./P  
      ENDIF      
   20 IF(P.GE.10000.) THEN         
         P=P/10000.       
         TENK=TENK*10000. 
         K=K+4   
         GOTO 20 
      ENDIF      
   30 IF(P.GE.10.) THEN   
         P=P/10. 
         TENK=TENK*10.    
         K=K+1   
         GOTO 30 
      ENDIF      
      IF(IFLAG.NE.0) THEN 
         P=10./P 
         TENK=.1/TENK     
         K=-K-1  
      ENDIF      
      IF(P.LT.2.) THEN    
         P=1.    
         NM=5    
      ELSEIF(P.LT.5) THEN 
         P=2.    
         NM=4    
      ELSEIF(P.GE.5.) THEN         
         P=5.    
         NM=5    
      ENDIF      
      DZ=P*TENK  
      N1=Z1/DZ   
      FN=N1      
      Z=FN*DZ    
      IF(Z.GT.Z1) THEN    
         Z=Z-DZ  
         N1=N1-1 
      ENDIF      
      Z1=Z       
      N2=Z2/DZ   
      FN=N2      
      Z=FN*DZ    
      IF(Z.LT.Z2) THEN    
         N2=N2+1 
         Z=Z+DZ  
      ENDIF      
      Z2=Z       
      IF(K.LE.0.AND.K.GE.-5) THEN  
         K=-K    
         GOTO 50 
      ENDIF      
      IF(ABS(Z2).LE.ABS(Z1)) THEN  
         Z=ABS(Z1) 
      ELSE       
         Z=ABS(Z2) 
      ENDIF      
      Z=Z/TENK   
      J=0        
   40 IF(Z.GE.10.) THEN   
         Z=Z/10. 
         J=J+1   
         GOTO 40 
      ENDIF      
      IF(K.GE.0.AND.J+K.LE.5) THEN 
         K=0     
      ELSE       
         K=10+J  
         IF(K.LT.11) K=11 
      ENDIF      
       
   50 ZMIN=Z1    
      ZMAX=Z2    
      MAJOR=N2-N1         
      MINOR=NM*MAJOR      
      KF=K       
      RETURN     
      END        
       
      SUBROUTINE DGA(IX1,IX2,IY1,IY2,X1,X2,Y1,Y2)      
!***********************************************************************        
!     THIS ROUTINE DEFINES THE GRAPH AREA.  THE FIRST FOUR ARGUMENTS   *        
! DEFINE THE FRAME COORDINATES FOR THE BOUNDARIES OF THE GRAPH AREA.   *        
! THE NEXT FOUR ARGUMENTS ARE THE FLOATING-POINT VALUES ASSIGNED TO    *        
! THE BOUNDARIES.  IF IXL > IXR, AND SIMILARLY IF IYB > IYT, THEY ARE  *        
! REVERSED.  THE BOUNDARY COORDINATES ARE TESTED FOR 0 < RANGE < 1023; *        
! IF THEY ARE OUT OF RANGE, THEIR VALUES ARE SET TO THE APPROPRIATE    *        
! MINIMUM OR MAXIMUM VALUE.  THERE ARE NO RESTRICTIONS ON XL, XR, YB,  *        
! OR YT OTHER THAN NORMAL MACHINE LIMITS.  THE VALUES ARE STORED IN    *        
! COMMON BLOCK CJE07.                                                  *        
!     ENTRY DGAX FILLS COMMON /CJE07X/ WITH THE FRAME COORDINATES AND  *        
! FLOATING-POINT VALUES BELONGING TO THE MINIMUM/MAXIMUM LOCATIONS OF  *        
! THE TICK MARKS.                                                      *        
!                                                                      *        
!     MODIFIED 210684 GMDH: ADDED COMMON /CJE07X/ AND ENTRY DGAX.      *        
!***********************************************************************      
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
      COMMON /CJE07X/XLX,XRX,YBX,YTX,IXLX,IXRX,IYBX,IYTX      
       
      IXL=MIN(MAX(0,MIN(IX1,IX2)),1023)     
      IXR=MIN(MAX(0,MAX(IX1,IX2)),1023)     
      IYB=MIN(MAX(0,MIN(IY1,IY2)),1023)     
      IYT=MIN(MAX(0,MAX(IY1,IY2)),1023)     
      XL=X1      
      XR=X2      
      YB=Y1      
      YT=Y2      
       
!     * ENTRY FOR EQUIDISTANT SCALING.      
      ENTRY DGAX(IX1,IX2,IY1,IY2,X1,X2,Y1,Y2)        
      IXLX=IX1   
      IXRX=IX2   
      IYBX=IY1   
      IYTX=IY2   
      XLX=X1     
      XRX=X2     
      YBX=Y1     
      YTX=Y2     
      RETURN     
      END        
       
      
      SUBROUTINE DRAW_LOG_AXIS(JCOUNT,ITYPE)
!***********************************************************************        
!     Helper to draw a logarithmic grid along one axis.                *        
!***********************************************************************      
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
      DIMENSION IXY(4),XY(4)         
      EQUIVALENCE (IXY,IXL),(XY,XL)  
      DIMENSION ALG(8)      
      CHARACTER*14 MESS1,MESS2(2)    
       
      DATA (ALG(K),K=1,8) /.30102999566398,.47712125471966,       &     
                           .60205999132796,.69897000433602,.77815125038364,       &     
                           .84509804001426,.90308998699194,.95424250943933/   
      DATA MESS1/'DECADES EXCEED'/ 
      DATA MESS2/'  25 NO OF X  ','  25 NO OF Y  '/  

      IF(JCOUNT.EQ.0) RETURN

      I1=2*ITYPE-1        
      I2=2*ITYPE 
      Z1=XY(I1)    
      Z2=XY(I2)    
      IF(Z1.EQ.Z2) Z2=Z2+.01       
      ZMIN=AMIN1(Z1,Z2)     
      ZMAX=AMAX1(Z1,Z2)     
      ZMIN=AMIN1(AINT(ZMIN),SIGN(AINT(ABS(ZMIN)+.999),ZMIN))  
      ZMAX=AMAX1(AINT(ZMAX),SIGN(AINT(ABS(ZMAX)+.999),ZMAX))  
      Z1=ZMIN    
      Z2=ZMAX    
      NZ=ABS(Z1-Z2)         
      IF(NZ.GT.25) THEN   
         CALL DLCH(500,520,MESS1,14,2)      
         CALL DLCH(500,500,MESS2(ITYPE),14,2)        
         RETURN  
      ENDIF      
      IF(NZ.EQ.0) THEN    
         Z11=Z1+1.        
         IF(Z2.LT.Z1) Z11=Z1-1.    
         NZ=1    
         Z1=Z11  
      ENDIF      
      IF(XY(I2).GE.XY(I1)) THEN    
         IREV=1  
         XY(I1)=Z1        
         XY(I2)=Z2        
      ELSE       
         IREV=2  
         XY(I1)=Z2        
         XY(I2)=Z1        
      ENDIF      
      ISL=(IXY(I2)-IXY(I1))/NZ     
      IZC=IXY(I1)  
      DO 30 I=1,NZ        
         DO 20 K=1,8      
           ICZ=IZC+(IREV-1+(3-IREV-IREV)*ALG(K))*ISL         
           IF(ITYPE.EQ.1) THEN         
             CALL DRV(real(ICZ),real(IYT-15),real(ICZ),real(IYT)) 
             CALL DRV(real(ICZ),real(IYB),real(ICZ),real(IYB+15)) 
           ELSE         
             CALL DRV(real(IXL),real(ICZ),real(IXL+15),real(ICZ)) 
             CALL DRV(real(IXR-15),real(ICZ),real(IXR),real(ICZ)) 
           ENDIF         
   20    CONTINUE         
         IZC=IXY(I1)+(I*(IXY(I2)-IXY(I1)))/NZ        
         IF(ITYPE.EQ.1) THEN 
           CALL DRV(real(IZC),real(IYT),real(IZC),real(IYT-25))    
           CALL DRV(real(IZC),real(IYB+25),real(IZC),real(IYB))    
         ELSE    
           CALL DRV(real(IXL),real(IZC),real(IXL+25),real(IZC))    
           CALL DRV(real(IXR-25),real(IZC),real(IXR),real(ICZ))    
         ENDIF   
   30 CONTINUE   
      RETURN
      END

      SUBROUTINE DLGLG(JX,JY) 
!***********************************************************************        
!     LOG-LOG GRID                                                     *        
!***********************************************************************      
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      

      CALL BOX(IXL,IXR,IYB,IYT)      
      CALL DRAW_LOG_AXIS(JY,2)
      CALL DRAW_LOG_AXIS(JX,1)
      RETURN
      END

      SUBROUTINE DLGLN(JX,NY)       
!***********************************************************************        
!     LOG X, LINEAR Y GRID                                             *        
!***********************************************************************      
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      

      CALL DLNLN(0,NY,1,0,0)         
      CALL DRAW_LOG_AXIS(JX,1)
      RETURN
      END

      SUBROUTINE DLNLG(NX,JY)    
!***********************************************************************        
!     LINEAR X, LOG Y GRID                                             *        
!***********************************************************************      
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      

      CALL DLNLN(NX,0,1,0,0)         
      CALL DRAW_LOG_AXIS(JY,2)
      RETURN
      END

      SUBROUTINE DLNLN(NX,NY,IBOX,IAX,IAY)    
!***********************************************************************        
!     THIS ROUTINE DRAWS A FRAME WITH A LINEAR-LINEAR GRID CONSISTING  *        
! OF NX EQUALLY SPACED INTERVALS IN THE X-DIRECTION AND NY EQUALLY     *        
! SPACED INTERVALS IN THE Y-DIRECTION (0 < NX/Y <= 10).  THE INTERVALS *        
! ARE MARKED OFF BY TICKS ON THE BOUNDARIES.                           *        
!                                                                      *        
!     MODIFIED 210684 GMDH: NUMBER OF TICK MARKS INCREASED WITH 1 ON   *        
!     EACH SIDE OF THE RANGE; COMMON CJE07X ADDED.                     *        
!     MODIFIED HGO 13/11/85: SUPPRESS TICK MARKS IF NX/Y=0; ADDED THE  *        
!     ARGUMENTS IBOX,IAX,IAY TO SUPPRESS DRAWING OF THE BOX IF IBOX=0  *        
!     AND TO DRAW THE X/Y=0 AXIS IF IAX/Y.NE.0                         *        
!***********************************************************************      
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
      COMMON /CJE07X/XLX,XRX,YBX,YTX,IXLX,IXRX,IYBX,IYTX      
       

      IF(IBOX.NE.0) CALL BOX(IXL,IXR,IYB,IYT)        
       
      IF(IAX.NE.0.AND.(XL.LT.0..AND.XR.GT.0.)) THEN  
!        * DRAW X=0 AXIS. 
         IX0=(IXL*XR-IXR*XL)/(XR-XL) 
       IF(IAX.EQ.1) CALL DRV(real(IX0),real(IYB),real(IX0),real(IYT))      
       IF(IAX.EQ.2) CALL DASH(real(IX0),real(IYB),real(IX0),real(IYT),10,10,0,IDUM) 
      ENDIF      
      IF(NX.NE.0) THEN    
         NXS=MIN(IABS(NX),128)       
         DX=REAL(IXRX-IXLX)/NXS    
         IIYB=IYB+20      
         IIYT=IYT-20      
         DO 10 I=0,NXS    
           IXS=IXLX+I*DX 
           CALL DRV(real(IXS),real(IYB),real(IXS),real(IIYB))      
           CALL DRV(real(IXS),real(IYT),real(IXS),real(IIYT))      
   10    CONTINUE         
      ENDIF      
       
      IF(IAY.NE.0.AND.(YB.LT.0..AND.YT.GT.0.)) THEN  
!        * DRAW Y=0 AXIS. 
         IY0=(IYB*YT-IYT*YB)/(YT-YB) 
         IF(IAY.EQ.1) CALL DRV(real(IXL),real(IY0),real(IXR),real(IY0))      
         IF(IAY.EQ.2) CALL DASH(real(IXL),real(IY0),real(IXR),real(IY0),10,10,0,IDUM) 
      ENDIF      
      IF(NY.NE.0) THEN    
         NYS=MIN(IABS(NY),128)       
         DY=REAL(IYTX-IYBX)/NYS    
         IIXR=IXR-20      
         IIXL=IXL+20      
         DO 20 I=0,NYS    
           IYS=IYBX+I*DY 
           CALL DRV(real(IXL),real(IYS),real(IIXL),real(IYS))      
           CALL DRV(real(IXR),real(IYS),real(IIXR),real(IYS))      
   20    CONTINUE         
      ENDIF      
      RETURN     
      END        
       
      
      SUBROUTINE SBLIN(NX)    
!***********************************************************************        
!     THIS ROUTINE PRINTS A LINEAR NUMERIC SCALE ON THE BOTTOM BOUN-   *        
! DARY OF A FRAME WITH NX EQUALLY SPACED INTERVALS DRAWN BY DLNLN OR   *        
! DLNLG.  THE NUMBERS ARE PRINTED IN F5.2 FORMAT WITH AN ADDITIONAL    *        
! POWER OF 10 (IF NEEDED) PRINTED SEPATATELY.  THE DATA FOR THE SCALE  *        
! ARE OBTAINED FROM XLX,XRX,YBX,YTX OF COMMON BLOCK CJE07X.            *        
!                                                                      *        
!     MODIFIED BY DEBBY HYMAN 4/7/80: SCALE FACTOR KS CORRECTED.       *        
!     MODIFIED 210684 GMDH: ADDED COMMON /CJE07X/.                     *        
!     MODIFIED HGO 13/11/85: RETURN ON NX=0.                           *        
!***********************************************************************        
       
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
      COMMON /CJE07X/XLX,XRX,YBX,YTX,IXLX,IXRX,IYBX,IYTX      
      CHARACTER*5 OUT     
       
      IF(NX.EQ.0) RETURN  
       
!     * DETERMINE THE SCALE FACTOR KS OF 10.         
      T=AMAX1(ABS(XLX),ABS(XRX))     
      IF(ABS(T).LE.1.E-15) T=1.E-15         
      X=ALOG19(T)  
!     * FIX FOR -1.0E7 THAT RETURNS KS=6 INSTEAD OF KS=7:     
      KS=X+SIGN(0.001,X)    
!       
      FACT=10.**(-KS)       
      XLL=XLX*FACT        
      XRR=XRX*FACT        
!       
!     * WRITE XLL ONTO THE BOUNDARY.        
      IYB1=IYB-18         
      WRITE(OUT,'(F5.2)') XLL      
      CALL DLCH(IXLX-18,IYB1,OUT,5,-2)
!       
!     * DETERMINE THE NUMBER OF INTERVALS TO SCALE (0 < NX <= 10).     
      NXA=MIN(10,IABS(NX))  
      DX=(XRR-XLL)/NXA    
      DDX=REAL(IXRX-IXLX)/NXA      
!       
!     * WRITE THE SCALE ONTO THE BOUNDARY.  
      DO 10 I=1,NXA       
         IXC=IXLX+I*DDX-18         
         XC=XLL+I*DX      
         WRITE(OUT,'(F5.2)') XC    
         CALL DLCH(IXC,IYB1,OUT,5,-2) 
   10 CONTINUE   
       
!     * WRITE THE SCALE FACTOR OF 10.       
      IF(KS.EQ.0) RETURN  
      IF(2.LE.KS.AND.KS.LE.9) J=1  
      IF((-9.LE.KS.AND.KS.LE.-1).OR.(KS.GT.9)) J=2   
      IF(KS.LE.-10) J=3   
      IXR1=IXR-36         
      IYB2=IYB-43         
      CALL DLCH(IXR1,IYB2,'X',1,1) 
      CALL DLCH(IXR1+8,IYB2,'10',2,2) 
      IF(KS.EQ.1) RETURN  
      WRITE(OUT,'(I3)') KS         
      CALL DLCH(IXR1+33,IYB2+8,OUT(4-J:3),J,1)       
      RETURN     
      END        
       
      
      SUBROUTINE SLLIN(NY)    
!***********************************************************************        
!     THIS ROUTINE PRINTS A LINEAR NUMERIC SCALE ON THE LEFT BOUN-     *        
! DARY OF A FRAME WITH NY EQUALLY SPACED INTERVALS DRAWN BY DLNLN OR   *        
! DLGLN.  THE NUMBERS ARE PRINTED IN F5.2 FORMAT WITH AN ADDITIONAL    *        
! POWER OF 10 (IF NEEDED) PRINTED SEPATATELY.  THE DATA FOR THE SCALE  *        
! ARE OBTAINED FROM XLX,XRX,YBX,YTX OF COMMON BLOCK CJE07X.            *        
!                                                                      *        
!     MODIFIED BY DEBBY HYMAN 4/7/80: FIXED ALOG(X) BEING OFF FOR      *        
!     SCALING IN SOME CASES.                                           *        
!     MODIFIED 210684 GMDH: ADDED COMMON /CJE07X/.                     *        
!     MODIFIED HGO 13/11/85: RETURN ON NY=0.                           *        
!***********************************************************************      
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
      COMMON /CJE07X/XLX,XRX,YBX,YTX,IXLX,IXRX,IYBX,IYTX      
      CHARACTER*5 OUT     
       
      IF(NY.EQ.0) RETURN  
       
!     * DETERMINE THE SCALE FACTOR KS OF 10.         
      T=AMAX1(ABS(YBX),ABS(YTX))     
      IF(ABS(T).LE.1.E-15) T=1.E-15         
      X=ALOG19(T)  
!     * FIX FOR -1.0E7 THAT RETURNS KS=6 INSTEAD OF KS=7:     
      KS=X+SIGN(0.001,X)    
       
      FACT=10.**(-KS)       
      YTT=YTX*FACT        
      YBB=YBX*FACT        
       
!     * WRITE YBB ONTO THE BOUNDARY.        
      IXL1=IXL-64         
      IF(IXL1.LT.15) IXL1=15       
      WRITE(OUT,'(F5.2)') YBB      
      CALL DLCH(IXL1,IYBX-2,OUT,5,2) 
       
!     * DETERMINE THE NUMBER OF INTERVALS TO SCALE (0 < NY <= 10).     
      NYA=MIN(10,IABS(NY))  
      DY=(YTT-YBB)/NYA    
      DDY=REAL(IYTX-IYBX)/NYA      
       
!     * WRITE THE SCALE ONTO THE BOUNDARY.  
      DO 10 I=1,NYA       
         IYC=IYBX+I*DDY-6 
         YC=YBB+I*DY      
         WRITE(OUT,'(F5.2)') YC    
         CALL DLCH(IXL1,IYC,OUT,5,2) 
   10 CONTINUE   
       
!     * WRITE THE SCALE FACTOR OF 10.       
      IF(KS.EQ.0) RETURN  
      IF(2.LE.KS.AND.KS.LE.9) J=1  
      IF((-9.LE.KS.AND.KS.LE.-1).OR.(KS.GT.9)) J=2   
      IF(KS.LE.-10) J=3   
      IYT1=IYT+13         
      CALL DLCH(IXL1,IYT1,'X',1,1) 
      CALL DLCH(IXL1+8,IYT1,'10',2,2) 
      IF(KS.EQ.1) RETURN  
      WRITE(OUT,'(I3)') KS         
      CALL DLCH(IXL1+33,IYT1+8,OUT(4-J:3),J,1)       
      RETURN     
      END        
       
      
      SUBROUTINE SBLOG(JX)    
!***********************************************************************        
!     THIS ROUTINE PRINTS A LOG NUMERIC SCALE ON THE BOTTOM BOUNDARY.  *        
! THROUGH ENTRY SLLOG(JY) A LOG NUMERIC SCALE IS PRINTED ON THE LEFT   *        
! BOUNDARY.                                                            *        
!                                                                      *        
!     MODIFIED HGO 13/11/85: RETURN UPON JX=0 AND JY=0.                *        
!***********************************************************************      
      COMMON /CJE07/XL,XR,YB,YT,IXL,IXR,IYB,IYT      
      DIMENSION IXY(4),XY(4)         
      EQUIVALENCE (IXY,IXL),(XY,XL)  
      CHARACTER*3 OUT     
!       
      IF(JX.EQ.0) RETURN  
      IY=IYB     
      IYDEL=-23  
      IYDL=8     
      IX=IXL     
      IXDEL=-16  
      IXDL=23    
      I1=1       
      I2=2       
      GOTO 10    
       
!     * ENTRY FOR PLOTTING LOG NUMERIC SCALE ON THE LEFT BOUNDARY.     
      ENTRY SLLOG(JY)       
      IF(JY.EQ.0) RETURN  
      IX=IXL     
      IXDEL=-54  
      IXDL=24    
      IY=IYB     
      IYDEL=-2   
      IYDL=8     
      I1=3       
      I2=4       
       
   10 IXYV=XY(I1)  
      NX=AMIN1(ABS(XY(I1)-XY(I2)),25.)      
      WRITE(OUT,'(I3)') IXYV       
      IXC=IX+IXDEL        
      IYC=IY+IYDEL        
      IXX=IXC+IXDL        
      IYX=IYC+IYDL        
      CALL DLCH(IXC,IYC,'10',2,2)    
      J=1        
      IF(IXYV.LT.0) J=2   
      IF(IXYV.LT.-9) J=3  
      CALL DLCH(IXX,IYX,OUT(4-J:3),J,1)     
      IF(NX.EQ.0) RETURN  
      IDXYV=ISIGN(1,INT(XY(I2)-XY(I1)))     
      DO 20 I=1,NX        
         IXYV=IXYV+IDXYV  
         WRITE(OUT,'(I3)') IXYV    
         IF(I1.NE.1) THEN 
           IYC=IY+IYDEL+(I*(IXY(I2)-IXY(I1)))/NX    
           IYX=IYC+IYDL  
         ELSE    
           IXC=IX+IXDEL+(I*(IXY(I2)-IXY(I1)))/NX    
           IXX=IXC+IXDL  
         ENDIF   
         CALL DLCH(IXC,IYC,'10',2,2) 
         IF(IXYV.GE.-9) J=2        
         IF(IXYV.GE.0) J=1         
         CALL DLCH(IXX,IYX,OUT(4-J:3),J,1)  
   20 CONTINUE   
      RETURN     
      END        
       
     
      SUBROUTINE CONVRT(Z,IZ,Z1,Z2,IZ1,IZ2)   
!***********************************************************************        
!     CONVRT CONVERTS THE REAL NUMBER Z TO AN SC-4020 COORDINATE BASED *        
! ON Z1 AND Z2 AS THE REAL USER-SCALED VALUES ASSOCIATED WITH THE PLOT *        
! AREA BOUNDARIES IZ1 AND IZ2, RESPECTIVELY.  THE RESULT IS STORED IN  *        
! IZ.  THE CONVERSION IS PERFORMED BY THE FORMULA:                     *        
!                                                                      *        
!     IZ = IZ1 +((Z -Z1)/(Z2 -Z1))*(IZ2 -IZ1)                          *        
!                                                                      *        
! IZ IS TESTED TO ENSURE THAT IT LIES WITHIN THE BOUNDARIES SPECIFIED  *        
! BY IZ1 AND IZ2.  IF IT LIES OUTSIDE THESE LIMITS, IT IS SET EQUAL TO *        
! THE APPROPRIATE LIMIT.  IF Z2 EQUALS Z1 ON INPUT, THEN IZ IS SET TO  *        
! MAX(IZ1,IZ2).                                                        *        
!                                                                      *        
! Z       - REAL USER COORDINATE.                                      *        
! IZ      - CONVERTED SC-4020 COORDINATE IN THE RANGE IZ1 TO IZ2.      *        
! Z1/Z2   - REAL USER VALUES CORRESPONDING TO IZ1/IZ2.                 *        
! IZ1/IZ2 - SC-4020 COORDINATES BOUNDS OF THE PLOT AREA ALONG ONE AXIS *        
!  (0 <= IZ1 <= IZ2 <= 1023).                                          *        
!                                                                      *        
! EXAMPLE: "CALL CONVRT(1.,IX,0.,2.,100,900)".  UPON RETURN, IX=500.   *        
!***********************************************************************      
      F=Z2-Z1    
      IF(F.NE.0) F=(IZ2-IZ1)/F     
      IZ=MIN(MAX(MIN(IZ1,IZ2),IZ1+INT((Z-Z1)*F)),MAX(IZ1,IZ2))         
      RETURN     
      END        

       
      SUBROUTINE BOX(IX1,IX2,IY1,IY2)         
!***********************************************************************        
!     THIS ROUTINE DRAWS A BOX WITH VERTICAL SIDES AT IX1 AND IX2 AND  *        
! HORIZONTAL SIDES AT IY1 AND IY2.                                     *        
!                                                                      *        
!     WRITTEN HGO 18/10/85                                             *        
!***********************************************************************      
      CALL DRV(real(IX1),real(IY1),real(IX1),real(IY2))      
      CALL DRWABS(real(IX2),real(IY2))  
      CALL DRWABS(real(IX2),real(IY1))  
      CALL DRWABS(real(IX1),real(IY1))  
      RETURN     
      END        

subroutine palette(color,rgb)
!***********************************************************************
! converts an index (0.-1.) in a color palette into rgb values (0.-1.) *
!    implements the ppplib postscript palette (see ps_begplt) (not yet)*
!***********************************************************************
real :: color, rgb(3)

rgb = (/ max(min(2.*color,1.),0.), 1.-abs(color-0.5), max(min(2.-2.*color,1.),0.) /)

!dd = 1. - color

!r2 = max(min(1. - abs(6.*dd - 3.),1.),0.)
!r3 = max(min(6.*dd - 5.,1.),0.)
!r4 = max(min(1.- 12.*dd),1.),0.)

!rgb(1)= max(min(2. - 4.*dd,1.),0.) + r2 + r3
!WRITE(ppp_unit_ps,'(A)') '   /rgb {/cc exch def 1 cc sub /dd exch def'
!WRITE(ppp_unit_ps,'(A)') '  1 6 dd mul 3 sub abs sub mx mi /r2 exch def'
!WRITE(ppp_unit_ps,'(A)') '  6 dd mul 5 sub mx mi           /r3 exch def'
!WRITE(ppp_unit_ps,'(A)') '  1 12 dd mul sub mx mi          /r4 exch def'
!WRITE(ppp_unit_ps,'(A)') '  dd 4 mul 2 sub mx mi r2 add    /bb exch def'
!WRITE(ppp_unit_ps,'(A)') '  2 4 dd mul 2 sub abs sub       /gg exch def'
!WRITE(ppp_unit_ps,'(A)') '  2 dd 4 mul sub mx mi r2 add r3 add r4 sub /rr exch def'
!WRITE(ppp_unit_ps,'(A)') '  rr gg bb'
!WRITE(ppp_unit_ps,'(A)') '  } def'

return
end

! ======================================================================        
! ================= SYSTEM DEPENDENT PARTS BELOW =======================        
! ======================================================================        
module svg_parameters
  logical :: ppp_svg                        ! switch on/off 
  integer :: ppp_unit_svg                   ! unit number for svg file
  real    :: svg_height,svg_width,svg_x_shift,svg_y_shift
  real    :: svg_ZIXSAV, svg_ZIYSAV         ! the current position
  integer :: path_index                     ! keeps track of number of path defs
  integer :: svg_page_index                 ! the current number of pages
  integer :: charsize(8)                    ! font sizes
  integer :: svg_color                      ! the current color (1-256)
  integer :: svg_rgb(3)                     ! the current color (in rgb (1-256))
  real    :: svg_linewidth                  ! the current linewidth

  parameter (charsize = (/ 14, 20, 24, 28, 32, 36, 40, 48 /))
  parameter (ppp_unit_svg=52, ppp_svg = .false. )
  parameter (svg_height=780, svg_width=1024, svg_x_shift=0, svg_y_shift=0)
end module svg_parameters

module ps_parameters
  logical   :: ppp_ps                       ! switch on/off 
  integer   :: ppp_unit_ps                  ! unit number for postscript file
  integer   :: nql3, nql4                   ! quality for triangle fill                                     
  integer   :: ps_number_of_lines
  real      :: ps_ZIXSAV, ps_ZIYSAV         ! the current position
  integer   :: ps_page_index                ! the current number of pages
  integer   :: ps_color                     ! the current color (1-256)
  real      :: ps_rgb(3)                    ! the current color  (in rgb (0-1))
  real      :: ps_linewidth                 ! the current linewidth

  parameter(ppp_unit_ps=51, ppp_ps= .true.)
end module ps_parameters


subroutine DATI(D,T)    
!***********************************************************************        
!     THIS SUBROUTINE WRITES DATE AND TIME ONTO THE VARIABLES D AND T. *        
!                                                                      *        
!     WRITTEN HGO 3/12/85                                              *        
!     CHANGED NAME OF THE SUBROUTINE, HGO 2/8/91                       *        
!***********************************************************************  
CHARACTER D*10,T*8, DAT*20,TIM*20
CHARACTER YEAR*4,MONTH*2,DAY*2,HOUR*2,MINUT*2  
      
CALL DATE_AND_TIME(DAT,TIM)
YEAR  = DAT(1:4)
MONTH = DAT(5:6)
DAY   = DAT(7:8)
HOUR  = TIM(1:2)
MINUT = TIM(3:4)
WRITE(D,'(A2,A1,A2,A1,A4)') DAY,'/',MONTH,'/',YEAR
WRITE(T,'(A2,A1,A2)') HOUR,':',MINUT       

RETURN     
END        
  
       
SUBROUTINE WRTEXT(IUNIT)
!***********************************************************************        
!     BRANCHING TO WRTEXT1 (CALCOMP) / WRTEXT2 (POSTSCRIPT).           *        
!***********************************************************************        
use svg_parameters
use ps_parameters

COMMON /KPOS/KP(36)   
       
IF (PPP_SVG) CALL svg_WRTEXT(ppp_unit_svg)     
IF (PPP_PS)  CALL ps_WRTEXT(ppp_unit_ps)

KP(1) = 1  

RETURN     
 END        
       
    
SUBROUTINE svg_WRTEXT(IUNIT)      
!***********************************************************************        
!     THIS ROUTINE READS A LOCAL FILE, THAT IS OPENED IN THE CALLING   *        
! PROGRAM WITH THE UNIT NUMBER "IUNIT", AND WRITES IT TO THE GRAPHICS  *        
! FILE.  WRTEXT STARTS WRITING ON A NEW FRAME, UNLESS IUNIT < 0 WHEN   *        
! WRITING STARTS AT THE CURRENT IY POSITION OF THE DRAWING BEAM.  IT   *        
! AUTOMATICALLY ADVANCES A FRAME IF THE FILE NEEDS AN ADDITIONAL PAGE. *        
! TYPICAL USE FOR WRTEXT IS TO WRITE THE CURRENT UPDATE MODIFICATIONS  *        
! OF THE SOURCE OR THE NAMELIST INPUT ONTO THE GRAPHICS FILE.          *        
!                                                                      *        
!     WRITTEN BY DEBBY HYMAN, 8-79                                     *        
!     MODIFIED HGO 25/10/85: OPTION IUNIT < 0, IMPROVED LINE SPACING.  *        
!***********************************************************************      
      PARAMETER (ISPACE=4)  
      CHARACTER*80 LINE   
       
      MY = 16+ISPACE      
      IU = IABS(IUNIT)      
      IF(IUNIT.LT.0) THEN 
         CALL svg_SEELOC(ZIX,ZIY)         
         IY = INT(ZIY)-MY       
      ELSE       
         CALL svg_ADV(1)       
         IY = 780-MY      
      ENDIF      
              
      REWIND IU  
   10 READ(IU,'(A80)',END=40) LINE 
       
      DO 20 L=80,1,-1     
   20    IF(LINE(L:L).NE.' ') GOTO 30       
   30 CALL svg_DLCH(20,IY,LINE(1:L),L,2)
      IY = IY-MY 
       
      IF(IY.LT.0) THEN    
!        * RESET IY FOR ANOTHER PAGE OF TEXT.        
         CALL svg_ADV(1)       
         IY = 780-MY      
      ENDIF      
      GOTO 10    
       
   40 RETURN     
      END        
       
    
SUBROUTINE ps_WRTEXT(IUNIT)    
!***********************************************************************        
!     POSTSCRIPT VERSION.                                              *        
!***********************************************************************      
PARAMETER (ISPACE=4)  
CHARACTER*80 LINE   
       
MY = 17+ISPACE      
IU = IABS(IUNIT)      

IF(IUNIT.LT.0) THEN 
  CALL ps_SEELOC(ZIX,ZIY)         
  IY = INT(ZIY)-MY       
ELSE       
  CALL ps_ADV(1)       
  IY = 780-MY      
ENDIF      
       
REWIND IU  

   10 READ(IU,'(A80)',END=40) LINE 
       
      DO 20 L=80,1,-1     
   20    IF(LINE(L:L).NE.' ') GOTO 30       
   30 CALL ps_DLCH(20,IY,LINE(1:L),L,2)
      IY = IY-MY 
       
      IF(IY.LT.0) THEN    
!        * RESET IY FOR ANOTHER PAGE OF TEXT.        
         CALL ps_ADV(1)       
         IY = 780-MY      
      ENDIF      
GOTO 10    
       
40 RETURN     

END        
       
    
 
      BLOCK DATA EBDASC 
!***********************************************************************        
!     CONVERTS EBCDIC TO CORRESPONDING ASCII VALUES (AND VICE VERSA).  *        
!***********************************************************************      
      COMMON /NEBDASC/NEA(64:255)    
      COMMON /NASCEBD/NAE(32:126)    
       
      DATA (NEA(IE),IE=64,159)        &     
        /   32,   0,   0,   0,   0,   0,       &     
             0,   0,   0,   0,   0,  46,  60,  40,  43, 124,       &     
            38,   0,   0,   0,   0,   0,   0,   0,   0,   0,       &     
            33,  36,  42,  41,  59,  94,  45,  47,   0,   0,       &     
             0,   0,   0,   0,   0,   0,   0,  44,  37,  95,       &     
            62,  63,   0,   0,   0,   0,   0,   0,   0,   0,       &     
             0,  96,  58,  35,  64,  39,  61,  34,   0,  97,       &     
            98,  99, 100, 101, 102, 103, 104, 105,   0,   0,       &     
             0,   0,   0,   0,   0, 106, 107, 108, 109, 110,       &     
           111, 112, 113, 114,   0,   0,   0,   0,   0,   0 /  
     DATA (NEA(IE),IE=160,255)        &     
        /  0, 126, 115, 116, 117, 118, 119, 120, 121, 122,       &     
           0,   0,   0,  91,   0,   0,   0,   0,   0,   0,       &     
           0,   0,   0,   0,   0,   0,   0,   0,   0,  93,       &     
           0,   0, 123,  65,  66,  67,  68,  69,  70,  71,       &     
          72,  73,   0,   0,   0,   0,   0,   0, 125,  74,       &     
          75,  76,  77,  78,  79,  80,  81,  82,   0,   0,       &     
           0,   0,   0,   0,  92,   0,  83,  84,  85,  86,       &     
          87,  88,  89,  90,   0,   0,   0,   0,   0,   0,       &     
          48,  49,  50,  51,  52,  53,  54,  55,  56,  57,       &     
           0,   0,   0,   0,   0,   0   /  
      
     DATA (NAE(IA),IA=32,126)         &     
        /  64,  90, 127, 123,  91, 108,  80, 125,       &      
           77,  93,  92,  78, 107,  96,  75,  97, 240, 241,       &     
          242, 243, 244, 245, 246, 247, 248, 249, 122,  94,       &     
           76, 126, 110, 111, 124, 193, 194, 195, 196, 197,       &     
          198, 199, 200, 201, 209, 210, 211, 212, 213, 214,       &     
          215, 216, 217, 226, 227, 228, 229, 230, 231, 232,       &     
          233, 173, 224, 189,  95, 109, 121, 129, 130, 131,       &     
          132, 133, 134, 135, 136, 137, 145, 146, 147, 148,       &     
          149, 150, 151, 152, 153, 162, 163, 164, 165, 166,       &     
          167, 168, 169, 192,  79, 208, 161       /  
       
      END        

SUBROUTINE DLCH(IX,IY,STRING,NC,ISIZE)  
!***********************************************************************        
!     BRANCHING TO DLCH1 (SVG) / DLCH2 (POSTSCRIPT).                   *        
!***********************************************************************      
use svg_parameters
use ps_parameters

CHARACTER*(*) STRING         
       
IF (PPP_SVG) CALL svg_DLCH(IX,IY,STRING,NC,ISIZE)         
IF (PPP_PS)  CALL ps_DLCH(IX,IY,STRING,NC,ISIZE)         

RETURN     
END

SUBROUTINE DLCV(IX,IY,STRING,NC,ISIZE)    
use svg_parameters
use ps_parameters

CHARACTER*(*) STRING         

IF (PPP_SVG) CALL svg_DLCV(IX,IY,STRING,NC,ISIZE)         
IF (PPP_PS)  CALL ps_DLCV(IX,IY,STRING,NC,ISIZE)         

RETURN     
END        
       
SUBROUTINE DLCH_old(IX,IY,STRING,NC,ISIZE) 
!***********************************************************************        
!     THIS ROUTINE WILL PRINT ARBITRARILY LARGE CHARACTERS ON THE      *        
! GRAPHICS FILE, EITHER HORIZONTALLY OR VERTICALLY (WITH ENTRY DLCV).  *        
! HORIZONTAL PRINTING IS FROM LEFT TO RIGHT.  FOR VERTICAL PRINTING,   *        
! CHARACTERS ARE ROTATED 90 DEGREES COUNTERCLOCKWISE AND PRINTED FROM  *        
! BOTTOM TO TOP.  THE ROUTINE USES VECTORS TO DRAW CHARACTERS IN A     *        
! BASIC 5 BY 7 MATRIX.    *        
!       *        
!     THE NC CHARACTERS STORED IN STRING ARE WRITTEN WITH THE LOWER    *        
! LEFT-HAND CORNER OF THE FIRST CHARACTER AT (IX,IY) FOR BOTH HORIZON- *        
! TAL AND VERTICAL CHARACTERS.  CHARACTER AND LINE SPACING ARE AUTOMA- *        
! TIC IN EITHER DIRECTION WITH CHARACTER SIZES GIVEN BY MX=ISIZE*6 AND *        
! MY=ISIZE*8.  EACH LINE IS SPACED DOWN BY MY PLOTTING POSITIONS.  ON  *        
! SUBSEQUENT CALLS IF IX < 0, PRINTING WILL CONTINUE WHERE IT LEFT OFF *        
! ON THE PRECEDING PRINT.  IF IY < 0, THE FIRST CHARACTER IN STRING IS *        
! CENTERED AT IX,IY.      *        
!       *        
!     IF NC < 0, EACH OCCURRENCE OF THE CHARACTER '$' IN STRING CAUSES *        
! THE FONT TO BE CHANGED FROM NORMAL (NFONT=1, IC=32,126) TO SYMBOL    *        
! (NFONT=2, IC=192,254) AND VICE VERSA, WHERE '$' IS NOT COUNTED IN NC.*        
! IF STRING=' ' AND NC.NE.1, ONE SINGLE CHARACTER IS DRAWN REPRESENTED *        
! BY THE INTEGER CODE IC=NC.       *        
!       *        
!     FOR EXAMPLE,        *        
! THE LOWER CASE "a" IS PRINTED WITH:       *        
!     "CALL DLCH(IX,IY,'a',1,ISIZE)" OR "DLCH(IX,IY,' ',97,ISIZE)",    *        
! THE GREEK CHARACTER ALPHA WITH:  *        
!     "CALL DLCH(IX,IY,'$a',-1,ISIZE)" OR "DLCH(IX,IY,' ',225,ISIZE)", *        
! AND THE STRING "A single ALFA is printed" WITH:    *        
!     "CALL DLCH(IX,IY,'A single $A$ is printed',-21,ISIZE)". *        
!       *        
!     METHOD:    *        
! THE CHARACTERS ARE DRAWN USING VECTORS WHICH ARE TAKEN FROM TABLE.   *        
! TABLE IS THE COORDINATE MATRIX.  EACH CHARACTER IS DEFINED BY ONE    *        
! WORD CONSISTING OF A SERIES OF DX,DY'S REPRESENTING A DISPLACEMENT   *        
! IN A 5 BY 7 MATRIX.  THE STARTING AND ENDING OF THE VECTORS ARE      *        
! DEFINED BY:    *        
!     X1=X+DX1, Y1=Y+DY1, *        
!     X2=X+DX2, Y2=Y+DY2. *        
! X2,Y2 BECOME THE NEW X1,Y1 UNLESS DX=0 IN WHICH CASE THE VECTOR IS   *        
! STARTED ALL OVER WITH DY BECOMING THE NEXT DX.  IF DY=0, THE NEXT DX *        
! BECOMES DY WITH RESPECT TO A Y SHIFTED DOWNWARD BY AN AMOUNT OF 1/4  *        
! OF THE CHARACTER SIZE.  THE CHARACTER ORIGIN IS THE LOWER-LEFT COR-  *        
! NER X,Y (THE PARAMETERS IX,IY).  ISIZE IS THE CHARACTER SIZE AS A    *        
! MULTIPLICATIVE FACTOR OF THE DX,DY'S OF THE COORDINATE MATRIX.       *        
!       *        
!     MODIFIED BY HANS GOEDBLOED 18/10/85: REPLACING OCTAL CONVERSIONS *        
!     BY MACHINE-INDEPENDENT CHARACTER AND INTEGER MANIPULATIONS.      *        
!     MODIFIED BY GUIDO HUYSMANS 2/8/91: INTEGER COORDINATE TABLE OF   *        
!     ASCII CHARACTERS ADAPTED TO WORD LENGTHS OF 32 BIT MACHINES.     *        
!     MODIFIED BY HANS GOEDBLOED 11/11/91: EXTENDING CHARACTER TABLE   *        
!     TO CORRESPOND TO THE SYMBOL FONT OF DLCH2.     *        
!     MODIFIED BY ELISABETH SCHWARZ 4/12/91: FINISHED CHARACTER TABLE. *        
!***********************************************************************        
       
!*IF IBM       
!      COMMON /NEBDASC/NEA(64:255)    
!*ENDIF  
       
      CHARACTER*(*) STRING         
      INTEGER TABLE(3,2,32:126)      
      LOGICAL FVERT,FFONT,FSING    
!       
!     * TABLE COORDINATES ASCII CHARACTERS (HEBREW STYLE).    
!       
!      32. 33. 34. 35. 36. 37. 38. 39.      
!  !   "   #   $   %   &   '       
      DATA (((TABLE(L,N,I),L=1,3),N=1,1),I=32,39)      &       
       /   0,00000000,00000000,      &      
         222,31312220,72427372,      &      
           7,57454750,72715272,      &      
         353,10555101,47401272,      &      
           7,31302124,35516265,      &      
          15,24130736,27101175,      &      
          35,13122131,64735115,      &      
           0,00000000,53737453 /  
!      40. 41. 42. 43. 44. 45. 46. 47.      
!       (   )   *   +   ,   -   .   /       
      DATA (((TABLE(L,N,I),L=1,3),N=1,1),I=40,47)   & 
       /   0,00000000,74523214,      &      
           0,00000000,72543412,      &      
           0,00414506,22402264,      &      
           0,00000004,14506323,      &      
           0,00000000,13333413,      &      
           0,00000000,00004145,      &      
           0,00000023,13142423,      &      
           0,00000000,00001175 /  
!      48. 49. 50. 51. 52. 53. 54. 55.      
!       0   1   2   3   4   5   6   7       
      DATA (((TABLE(L,N,I),L=1,3),N=1,1),I=48,55)   & 
       /   0,00417274,45141241,     &      
           0,00000627,31301114,     &       
           0,00617274,65211115,     &       
         717,46544251,41104244,     &       
           0,00000733,13501474,     &       
           0,21121435,54517275,     &       
           0,41443514,12417274,     &       
           0,00000000,00717513 /  
!      56. 57. 58. 59. 60. 61. 62. 63.      
!       8   9   :   ;   <   =   >   ?       
      DATA (((TABLE(L,N,I),L=1,3),N=1,1),I=56,63)    &         
       /  74,55311214,35517274,     &       
           0,12144574,72514245,     &       
           2,43334240,64635464,     &       
         133,43313063,53546463,     &       
           0,00000000,00654125,     &       
           0,00000003,53105551,     &       
           0,00000000,00614521,     &       
           1,32303355,65747261 /  
!       
!      64. 65. 66. 67. 68. 69. 70. 71.  192.193.194.195.196.197.198.199.        
!       @   A   B   C   D   E   F   G    LE  AE  GE  CD DEL  IE PHI GAM         
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=64,71) &    
       /  34,43423113,35557351,    0,00000755,13501531,    &  
           0,00000117,31503432,    4,53442310,65546251,    &  
          41,44657471,11142544,    0,00000715,53101135,    &  
           0,25141221,61727465,    0,72744514,12314245,    &  
           0,00007174,45141171,    0,00000000,53151153,    &  
           0,00075711,11504441,    0,00252104,54106561,    &  
           0,00000757,11104144,       1373051,31222435,55646251,    &  
        3335,25141221,61727465,    0,00000657,57107212 /     
!      72. 73. 74. 75. 76. 77. 78. 79.  200.201.202.203.204.205.206.207.        
!       H   I   J   K   L   M   N   O   DIA INT the DIA LAM MIN NAB DOT         
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=72,79)         &    
       /   0,00711104,14507515,    0,00000041,23456341,   &   
           0,00727407,31301214,    0,00000000,74632312,   &   
           0,07375074,24131221, 4544,53736424,13225241,   &   
           0,00711103,17504215, 4541,23456341,33455341,   &   
           0,00000000,00711115,    0,00000000,00117315,   &   
           0,00000011,71337515,    0,00000000,00004541,   &   
           0,00000000,11711575,    0,00000000,51551351,   &   
 72,74652514,12216172,        425452,44423344,54635242 /     
!      80. 81. 82. 83. 84. 85. 86. 87.  208.209.210.211.212.213.214.215.        
!       P   Q   R   S   T   U   V   W    PI THE SRT SIG PER  NE  PM OME         
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=80,87)        &    
       /   0,00001171,74654441,   15,14740111,27207571,  &    
         331,50141241,72744514,       4442051,31121435,55747251,  &    
           1,17174654,44104315,    0,00000000,75134231,  &    
           0,21121425,61727465,    0,00657571,44111525,  &    
           0,00000007,17507313,    0,00000001,51101373,  &    
           0,00007121,12142575,    0,00642203,53105551,  &    
           0,00000000,00711375,    0,00414506,32302125,  &    
           0,00000071,12531475, 1112,32417274,45341415 /     
!      88. 89. 90. 91. 92. 93. 94. 95.  216.217.218.219.220.221.222.223.        
!       X   Y   Z   [   \   ]   ^   _   KSI PSI INF ARL ARD ARR ARU BAR         
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=88,95)       &     
       /   0,00000007,11501175,      34540444,20325207,57101511, &     
           0,00000714,31304375,   73,13014120,75454171, &     
           0,00000000,71751115,   41,31225445,35245241, &     
           0,00000000,74721214,    0,00000223,14203531, &     
           0,00000000,00002561,    0,00000342,33206323, &     
           0,00000000,72741412,    0,00000243,54403531, &     
           0,00000000,00547352,    0,00000546,35206323, &     
           0,00000000,00001511,    0,00000000,00003531 /     
!       
!      96. 97. 98. 99.100.101.102.103.  224.225.226.227.228.229.230.231.        
!       `   a   b   c   d   e   f   g   ovs alf bet chi del eps phi gam         
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=96,103)       &    
       /   0,00000000,73545373,    0,00000000,00007571,  &    
        3532,21121425,15555241,    0,00005512,21415215,  &    
           0,00007111,14355451,    1,16365434,20433513,  &    
           0,00000055,52311215,    0,00000515,21501155,  &    
           0,00007515,12315255,    0,75726125,14122143,  &    
           0,00313554,52311215,    0,05552311,21503531,  &    
          12,14052540,65746313, 1027,40543514,12315254,  &    
         121,42575725,13234405,    0,00516224,13225465 /     
!     104.105.106.107.108.109.110.111.  232.233.234.235.236.237.238.239.        
!       h   i   j   k   l   m   n   o   eta iot phi kap lam  mu  nu omi         
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=104,111)     &     
       /   0,00015455,45101171, 1524,65747362,03172701, &     
           0,12140737,37305313,    0,00000000,00431213, &     
           0,12132470,40747474, 1027,40543514,12315254, &     
           0,00000711,10143154,    0,00511103,15503115, &     
           0,00000727,31301214,    0,00000007,11504311, &     
         154,55453130,43525111,  443,34203544,75073101, &     
           0,01545545,24101151,    0,00000041,42122444, &     
           0,00543514,12315254,    0,00534121,13254553 /     
!     112.113.114.115.116.117.118.119.  240.241.242.243.244.245.246.247.        
!       p   q   r   s   t   u   v   w    pi the rho sig tau ups omb ome         
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=112,119)      &    
       /   0,00031345,57471101,   41,52445504,41401252,  &    
           0,00035325,17275105,    0,41727445,14124145,  &    
           0,00045545,24101151,    0,04233456,57463101,  &    
           0,00001114,25415255,    0,43352513,11415255,  &    
           0,00000732,31405452,    0,00053231,41505551,  &    
           0,00005525,14122151,    0,00000041,42121444,  &    
           0,00000000,00511355,  454,10423112,33143544,  &    
           0,00000051,12531455,    0,00423112,33143544 /     
!     120.121.122.123.124.125.126.      248.249.250.251.252.253.254.   
!       x   y   z   {   |   }   ~       ksi psi zet  
      DATA (((TABLE(L,N,I),L=1,3),N=1,2),I=120,126)      &    
       /   0,00000005,11501155,    6,56142440,31212515,  &    
        1214,25750453,43241701,    0,00074120,52313455,  &    
           0,00000000,51551115, 1420,51412216,20656271,  &    
           0,00141333,42537374,    0,00000000,00000000,  &    
           0,00000000,00001373,    0,00000000,00000000,  &    
           0,00121333,44537372,    0,00000000,00000000,  &    
           0,00000000,45345241,    0,00000000,00000000 /     
       
!     * NC=0 MAY BE USED TO SWITCH OFF PRINTING OF STRING.    
      IF(NC.EQ.0) RETURN  
       
!     * FLAG FOR ROTATE (DRAW VERTICAL CHARACTERS).  
      FVERT = .FALSE.     
       
!     * CHECK FOR IX AND IY WITHIN THE RANGES 0-1023 AND 0-779.        
      ISX = MIN(IABS(IX),1023)       
      ISY = MIN(IABS(IY),779)        
!     * IF IX<0, CONTINUE PRINTING AT PREVIOUS LOCATION.      
      IF(IX.LT.0) THEN
        CALL svg_SEELOC(ZISX,ZISY)  
        ISX=INT(ZISX)
        ISY=INT(ZISY) 
      ENDIF  
       
   10 CONTINUE   
       
!     * FLAG FOR INTERPRET '$' AS CHANGE THE FONT.   
      FFONT = .FALSE.     
      IF(NC.LT.0) FFONT = .TRUE.   
!     * DEFAULT FOR CHARACTER FONT.         
      NFONT = 1  
       
!     * FLAG FOR SINGLE CHARACTER. 
      FSING = .FALSE.     
      IF((LEN(STRING).EQ.1).AND.(NC.NE.1)) THEN      
         IC = NC 
         IF(IC.LT.32.OR.(126.LT.IC.AND.IC.LT.192).OR.IC.GT.254) RETURN 
         FSING = .TRUE.   
         NCHR = 1         
      ELSE       
!        * MAXIMUM FOR NC IS 80 CHARACTERS. 
         NCHR = MIN(IABS(NC),80)     
      ENDIF      
!       
!     * SET UP THE SPACING FOR 5 BY 7 CHARACTER MATRIX.       
      JSIZE = IABS(ISIZE)   
      IF(JSIZE.EQ.1) THEN 
         MX = 10 
         MY = 13 
         MYD = 3 
      ELSE       
         MX = JSIZE*6     
         MY = JSIZE*8     
         MYD = JSIZE*2    
      ENDIF      
!       
!     * SAVE ISX FOR LINE OVERFLOW.         
      ISXOLD = ISX        
!     * IF IY < 0, CENTER FIRST CHARACTER OF STRING AT IX,IY. 
      IF(IY.LT.0) THEN    
         ISX = ISX-MX/2   
         ISY = ISY-MY/2   
         IF(FVERT) ISY = ISY+MY    
         IF(FSING) THEN   
!  * ADDITIONAL POSITION CORRECTIONS FOR SINGLE CENTERED DOT  
!  * (ONLY PRECISE IF ISIZE EVEN) AND LOWER CASE CHARACTERS.  
   IF(IC.EQ.46) THEN      
      ISX = ISX-MX/12     
      ISY = ISY+5*MY/16   
      IF(FVERT) ISY = ISY-5*MY/8   
   ELSEIF((96.LE.IC.AND.IC.LE.126).OR. (224.LE.IC.AND.IC.LE.254)) THEN    
      ISY = ISY+MY/8      
      IF(FVERT) ISY = ISY-MY/4     
   ENDIF         
         ENDIF   
      ENDIF      
       
!     * GO THROUGH STRING PICKING OFF CHARACTERS ONE BY ONE.  
!       
      M = 0      
      DO 60 N=1,NCHR      
!       
   20    M = M+1 
         IF(FFONT.AND.(STRING(M:M).EQ.'$')) THEN     
!  * THE FONT IS CHANGED. 
   NFONT =  -NFONT+3      
   GOTO 20       
         ENDIF   
         IF(FSING) THEN   
   IF(IC.GE.192) THEN     
      IC = IC-128         
      NFONT = 2  
   ENDIF         
         ELSE    
!  * GET ASCII CHARACTER VALUE.    
   IC = ICHAR(STRING(M:M))  
!*IF IBM       
!CMS         IC = NEA(IC)    
!*ENDIF  
   IF(NFONT.EQ.2.AND.IC.LT.64) RETURN       
         ENDIF   
!       
!        * POSITION CORRECTION FOR OVERSTRIKE SYMBOL.         
         IF(NFONT.EQ.2.AND.IC.EQ.96) ISX = ISX+MX    
       
!        * CONTINUE PRINTING ON THE NEXT LINE IF STRING IS TOO LONG.   
         IF(((ISX+MX.GE.1023).AND.(.NOT.FVERT)) .OR. ((ISX+MX.GE. 779).AND.(     FVERT))) THEN         
   ISX = ISXOLD  
   IF(.NOT.FVERT) ISY = ISY-(MY+2*JSIZE)    
   IF(FVERT)      ISY = ISY+(MY+2*JSIZE)    
         ENDIF   
       
!        * PICK UP CONTROL WORD FROM TABLE FOR DRAWING THE CHARACTER   
!        * WITH VECTORS.  
       
         ICW = TABLE(3,NFONT,IC)     
         ISYA = ISY       
         J = 1   
         ISTART = 1       
       
!        * SOME CHARACTERS TAKE MORE OR LESS STROKES.         
!        * PICK UP ONE DIGIT AT A TIME FROM ICW UNTIL ICW=0.  
       
   30    IF(J.EQ.9)  ICW = TABLE(2,NFONT,IC)         
         IF(J.EQ.17) ICW = TABLE(1,NFONT,IC)         
         IF((ICW.EQ.0).AND.(J.NE.8).AND.(J.NE.16)) GOTO 50    
         IDX = MOD(ICW,10)  
         ICW = ICW/10     
         J = J+1 
         IF(IDX.EQ.0) THEN         
!  * START ANEW ON "0" IN DX LOCATION.      
   ISTART = 1    
   GOTO 30       
         ENDIF   
         IF(JSIZE.EQ.1) THEN       
   IX1 = IDX/2+IDX        
         ELSE    
   IX1 = IDX*JSIZE        
         ENDIF   
   40    IF(J.EQ.9)  ICW = TABLE(2,NFONT,IC)         
         IF(J.EQ.17) ICW = TABLE(1,NFONT,IC)         
         IDY = MOD(ICW,10)  
         ICW = ICW/10     
         J = J+1 
         IF(IDY.EQ.0) THEN         
!  * DOWNWARD DISPLACEMENT ON "0" IN DY LOCATION FOR CHARACTERS        
!  * LIKE L.C. Y AND G.   
   ISYA = ISYA-MYD        
   IF(FVERT) ISYA = ISYA+2*MYD     
   GOTO 40       
         ENDIF   
         IF(JSIZE.EQ.1) THEN       
   IY1 = IDY/2+IDY        
         ELSE    
   IY1 = IDY*JSIZE        
         ENDIF   
       
         IF(ISTART.EQ.1) THEN      
!  * POSITION THE BEAM.   
   IF(FVERT) THEN         
!     * ROTATE FOR VERTICAL CHARACTERS.     
      CALL MOVABS(real(ISYA-IY1),real(ISX+IX1))        
   ELSE 
      CALL MOVABS(real(ISX+IX1),real(ISYA+IY1))        
   ENDIF         
   ISTART = 0    
         ELSE    
!  * DRAW VECTOR.         
   IF(FVERT) THEN         
!     * ROTATE FOR VERTICAL CHARACTERS.     
      CALL DRWABS(real(ISYA-IY1),real(ISX+IX1))        
   ELSE 
      CALL DRWABS(real(ISX+IX1),real(ISYA+IY1))        
   ENDIF         
         ENDIF   
         GOTO 30 
       
!        * POSITION FOR NEXT CHARACTER.     
   50    IF(.NOT.(NFONT.EQ.2.AND.IC.EQ.96)) ISX = ISX+MX      
       
   60 CONTINUE   
       
!     * POST BEAM POSITION.        
      IF(.NOT.FVERT) CALL MOVABS(real(ISX),real(ISY))  
      IF(FVERT)      CALL MOVABS(real(ISY),real(ISX))
      RETURN     
       
!     * ENTRY FOR DRAWING VERTICALLY.       
      ENTRY DLCV_OLD(IX,IY,STRING,NC,ISIZE)    
      IF(NC.EQ.0) RETURN  
      FVERT = .TRUE.      
      ISX = MIN(IABS(IY),779)        
      ISY = MIN(IABS(IX),1023)       
      IF(IX.LT.0) THEN
        CALL svg_SEELOC(ZISY,ZISX)
        ISX = INT(ZISX)
        ISY = INT(ZISY)
      ENDIF     
      GOTO 10    
      END        

subroutine asci_to_unicode(iascii,unicode_string)
!***********************************************************************
!* converts ascii to unicode                                           *
!* ascii A (65) = unicode 0041                                         *
!* ascii a (97) = unicode 0061                                         *
!***********************************************************************
implicit none
character*4 :: unicode_string
integer     :: iascii, i4,i3,i2,i1
character*1 :: int_byte(16)

parameter (int_byte = (/'0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'/))

i4 = iascii / 4096 
i3 = (iascii - i4 * 4096) / 256
i2 = (iascii - i4 * 4096 - i3 * 256) / 16
i1 =  iascii - i4 * 4096 - i3 * 256 - i2 * 16

if (16*i2+i1 .lt. 32) then ! replace characters below 32 by a space(32)
  i2=2
  i1=0
endif

write(unicode_string,'(A,A,A,A)') int_byte(i4+1),int_byte(i3+1),int_byte(i2+1),int_byte(i1+1)

return
end

SUBROUTINE svg_DLCH(IX,IY,STRING,NC,ISIZE) 
!***********************************************************************        
!* svg version print characters                                        *
!***********************************************************************        
use svg_parameters
implicit none
integer       :: ix, iy, nc, isize
CHARACTER*(*) :: STRING     
CHARACTER*4   :: unicode_string

IF (NC.EQ.0) RETURN  

IF (string .eq. " ") then ! single character ascii value NC

  call asci_to_unicode(abs(NC),unicode_string)
  if (iy .ge. 0) then  
    
    write(ppp_unit_svg,'(A,f9.3,A,f9.3,A,i2,A,A,A)') '<text '//&
                   'x="',real(ABS(IX)),'" y="',SVG_HEIGHT-real(ABS(IY)), &
                   '" font-family="Arial" font-size="',charsize(min(abs(isize),8)),'"> &#x',unicode_string,'; </text>'
  else

!  baseline-shift="-50%" does not seem to work
    write(ppp_unit_svg,'(A,f9.3,A,f9.3,A,i2,A,A,A)') '<text text-anchor="middle" display-align="center" '//&
                   'x="',real(ABS(IX)),'" y="',SVG_HEIGHT-real(ABS(IY)), &
                   '" font-family="Arial" font-size="',charsize(min(abs(isize),8)),'"> &#x',unicode_string,'; </text>'
  endif

  return

endif

!return  ! case no handled yet

write(ppp_unit_svg,'(A,f9.3,A,f9.3,A,i2,A,A,A)') '<text '//&
                   'x="',real(IX),'" y="',SVG_HEIGHT-real(IY), &
                   '" font-family="Arial" font-size="',charsize(min(abs(isize),8)),'">',string(1:NC),'</text>'

!  write(ppp_unit_svg,'(A,f9.3,A,f9.3,A,i3,A,A,A)') '<text x="',xp(2),'" y="',SVG_HEIGHT-yp(2), &
!       '" font-family="Arial" font-size="',isize,'" text-anchor="end">',text,'</text>'

return
end

SUBROUTINE svg_DLCV(IX,IY,STRING,NC,ISIZE) 
!***********************************************************************        
!* svg version print characters                                        *
!***********************************************************************        
use svg_parameters
CHARACTER*(*) STRING         
CHARACTER*6   path_name

IF(NC.EQ.0) RETURN  

IF (string .eq. " ") return  ! case no handled yet

!path_index = path_index + 1

!write(path_name,'(A,i4.4)') 'vp',path_index
!write(ppp_unit_svg,'(A,A,A,2F9.3,A,2F9.3,A)') '<defs> <path id="',path_name,&
!            '" d="M ',real(IX),SVG_HEIGHT-real(IY),' L ',real(IX),0.,' z"/> </defs> '

!write(ppp_unit_svg,'(A,i2,A)') '<text font-family="Arial" font-size="',charsize(min(abs(isize),8)),'">'
!write(ppp_unit_svg,'(A,A,A,A,A)') '<textPath xlink:href="#',path_name,'"> ',string(1:NC),' </textPath> </text>'

write(ppp_unit_svg,'(A,i2,A)') '<text font-family="Arial" font-size="',charsize(min(abs(isize),8)),'" '
write(ppp_unit_svg,'(A,f6.2,A,f6.2,A,A,A)') 'transform="translate(',real(IX),',',svg_height-real(IY),') rotate(-90)">'
write(ppp_unit_svg,'(A,A)') string,' </text>'


return
end

SUBROUTINE ps_DLCH(IX,IY,STRING,NC,ISIZE) 
!***********************************************************************        
!     POST-SCRIPT VERSION, EXPLOITING THE LASERWRITER FONT HELVETICA   *        
! (NFONT=1) AND AN ADAPTED VERSION OF THE SYMBOL FONT (NFONT=2).       *        
! FOR ISIZE>0, THE CHARACTER SPACING HAS BEEN MODIFIED TO CONSTANT     *        
! PITCH (TYPEWRITER STYLE).  FOR ISIZE<0, THE ORIGINAL PROPORTIONAL    *        
! FONTS ARE EXPLOITED.                                                 *        
!                                                                      *        
!     WRITTEN BY GUIDO HUYSMANS, EGBERT WESTERHOF, AND HANS GOEDBLOED  *        
!      11/11/91                                                        *        
!***********************************************************************      
use ps_parameters
       
      COMMON /ADVPAGE/ADVP
      LOGICAL ADVP

!*IF IBM       
!      COMMON /NEBDASC/NEA(64:255)    
!      COMMON /NASCEBD/NAE(32:126)    
!*ENDIF  
       
      CHARACTER*(*) STRING         
      CHARACTER CHR*1, STROUT*81, FORM*17   
      CHARACTER*4 OCT, SYMB(192:255) 
      CHARACTER*1 BS1     
      LOGICAL FVERT, FFONT, FSING, FCHANGE, FLINETL, FOCT     
       
      SAVE NFONT, MAGN    
       
      DATA NFONT, MAGN / 0, 0/     

!     BACKSLASH ON IBM, THIS IS A ONE CHARARCTER VARIABLE. IT 
      BS1='\\'

!     * OCTAL VALUES FOR SYMBOL FONT CHARACTERS.    
       
!*IF IBM
!      DATA (SYMB(IC),IC=192,223)     
!    IC = 192     193     194     195     196     197     198     199     
!    >/'\\243','\\273','\\263','\\266','\\104','\\272','\\106','\\107',   
!    IC = 200     201     202     203     204     205     206     207     
!    > '\\340','\\362','\\112','\\250','\\114','\\055','\\321','\\267',   
!    IC = 208     209     210     211     212     213     214     215     
!     > '\\120','\\121','\\326','\\123','\\136','\\271','\\261','\\127',   
!    IC = 216     217     218     219     220     221     222     223     
!    > '\\130','\\131','\\245','\\254','\\257','\\256','\\255','\\276'/  
       
!      DATA (SYMB(IC),IC=224,254)     
!    IC = 224     225     226     227     228     229     230     231     
!    >/'\\040','\\141','\\142','\\143','\\144','\\145','\\146','\\147',   
!    IC = 232     233     234     235     236     237     238     239     
!    > '\\150','\\151','\\152','\\153','\\154','\\155','\\156','\\157',   
!    IC = 240     241     242     243     244     245     246     247     
!    > '\\160','\\161','\\162','\\163','\\164','\\165','\\166','\\167',   
!    IC = 248     249     250     251     252     253     254   
!    > '\\170','\\171','\\172','\   ','\   ','\   ','\   ' /         
!*ELSE
      DATA (SYMB(IC),IC=192,223)   &         
!        IC = 192    193    194    195    196    197    198    199     
         / '\243','\273','\263','\266','\104','\272','\106','\107',   &         
!        IC = 200    201    202    203    204    205    206    207     
  '\340','\362','\112','\250','\114','\055','\321','\267',   &         
!        IC = 208    209    210    211    212    213    214    215     
  '\120','\121','\326','\123','\136','\271','\261','\127',   &         
!        IC = 216    217    218    219    220    221    222    223     
  '\130','\131','\245','\254','\257','\256','\255','\276' /  
!       
      DATA (SYMB(IC),IC=224,247)   &         
!        IC = 224    225    226    227    228    229    230    231     
         / '\040','\141','\142','\143','\144','\145','\146','\147',   &         
!        IC = 232    233    234    235    236    237    238    239     
  '\150','\151','\152','\153','\154','\155','\156','\157',   &         
!        IC = 240    241    242    243    244    245    246    247     
  '\160','\161','\162','\163','\164','\165','\166','\167' /   
!        IC = 248    249    250    251    252    253    254   
!*ENDIF
       
!     * NC=0 MAY BE USED TO SWITCH OFF PRINTING OF STRING.    

      IF(NC.EQ.0) RETURN  
       
!     * FLAG FOR ROTATE (DRAW VERTICAL CHARACTERS).  
      FVERT = .FALSE.     
       
!     * CHECK FOR IX AND IY WITHIN THE RANGES 0-1023 AND 0-779.        
      ISX = MIN(IABS(IX),1023)       
      ISY = MIN(IABS(IY),779)        
!     * IF IX<0, CONTINUE PRINTING AT PREVIOUS LOCATION.      
      IF(IX.LT.0) THEN
        CALL ps_SEELOC(ZISX,ZISY)
        ISX = INT(ZISX)
        ISY = INT(ZISY) 
      ENDIF    
       
   10 CONTINUE   
!       
      FOCT = .FALSE.
      JSIZE = IABS(ISIZE)   
      IF(JSIZE.EQ.1) THEN 
         MX = 9  
         MY = 13 
      ELSE       
         MX = JSIZE*6     
         MY = JSIZE*8.5   
      ENDIF      
      MX1 = MX
      IF (ISIZE.LT.0) MX1 = MX / 2
!       
      IF((MAGN.NE.MY).OR.(NFONT.NE.1).OR.(ADVP)) THEN     
         MAGN = MY        
         NFONT = 1        
         WRITE(ppp_unit_ps,'(I4,A5)') MY,' scaH'    
         ADVP = .FALSE.
      ENDIF      
      FCHANGE = .FALSE.   
      FFONT = .FALSE.     
      IF(NC.LT.0) FFONT = .TRUE.   
!       
!     * FLAG FOR SINGLE CHARACTER. 
      FSING = .FALSE.     
      IF((LEN(STRING).EQ.1).AND.(NC.NE.1)) THEN      
         IC = NC 
         IF(IC.LT.32.OR.(126.LT.IC.AND.IC.LT.192).OR.IC.GT.254) RETURN 
         FSING = .TRUE.   
         NCHR = 1         
      ELSE       
!        * MAXIMUM FOR NC IS 80 CHARACTERS. 
         NCHR = MIN(IABS(NC),80)     
      ENDIF      
!       
!     * SAVE ISX AND ISY FOR LINE OVERFLOW. 
      ISXOLD = ISX        
      ISYOLD = ISY        
      IF(.NOT.FVERT) WRITE(ppp_unit_ps,'(I4,1X,I4,A2)') ISX,ISY,' m'  
      IF(FVERT) WRITE(ppp_unit_ps,'(A9,I4,1X,I4,A21,I1,1X,I1,A2)') &   
    'st gsave ',ISY,ISX,' translate 90 rotate ',0,0,' m'         
!       
!     * DRAWING SINGLE CHARACTERS. 
!       
      IF(FSING) THEN      
         IF(192.LE.IC.AND.IC.LE.254) THEN   
   NFONT = 2     
   WRITE(ppp_unit_ps,'(I4,A5)') MY,' scaS' 
   FOCT = .FALSE.         
   IF(IC.LE.223.OR.IC.GT.250) THEN 
      OCT = SYMB(IC)        
      FOCT = .TRUE.       
   ENDIF         
         ENDIF   
         IF(IC.EQ.46.AND.IY.LT.0) THEN      
!  * SIZE CORRECTION FOR SINGLE CENTERED DOT.        
   WRITE(ppp_unit_ps,'(I4,A5)') MY+8,' scaH'        
   MAGN = MY+8   
         ENDIF   
         IF(IC.LE.126.OR.IC.GE.224) THEN    
   IF(IC.GE.224) IC = IC-128       
!*IF IBM       
!CMS         IC = NAE(IC)    
!*ENDIF  
   CHR = CHAR(IC)  
         ENDIF   
         IF(IY.LT.0) THEN 
   IF(FOCT) THEN 
      WRITE(ppp_unit_ps,'(I4,1X,I7,A10)') ISX,ISY,' ('//OCT//') tc'   
   ELSEIF(CHR.EQ.'('.OR.CHR.EQ.')'.OR.CHR.EQ.BS1) THEN        
   WRITE(ppp_unit_ps,'(I4,1X,I4,A2,2A1,A4)')ISX,ISY,' (',BS1,CHR,') tc'    
   ELSE 
      WRITE(ppp_unit_ps,'(I4,1X,I4,A7)')  ISX,ISY,' ('//CHR//') tc'   
   ENDIF         
         ELSE    
   IF(FOCT) THEN 
      WRITE(ppp_unit_ps,'(I4,A10)') MX,' ('//OCT//') tw'     
   ELSEIF(CHR.EQ.'('.OR.CHR.EQ.')'.OR.CHR.EQ.BS1) THEN        
      WRITE(ppp_unit_ps,'(I4,A2,2A1,A4)')  MX,' (',BS1,CHR,') tw'    
   ELSE 
      WRITE(ppp_unit_ps,'(I4,A7)')  MX,' ('//CHR//') tw'     
   ENDIF         
         ENDIF   
         FOCT = .FALSE.   
         GOTO 50 
      ENDIF      
!       
!     * DRAWING STRING OF CHARACTERS.       
!       
      NUM = 0    
      M = 1      
      FLINETL = .FALSE.   
       
   20 CONTINUE   
       
         STROUT = 'X'     
         ILEN = 0         
         N = 0   
       
!        * PROCESSING PART OF STRING WITH THE SAME FONT.      
   30    CONTINUE         
   IF(NUM.GE.NCHR) GOTO 40         
   IF(((ISX+(N+1)*MX1.GE.1100).AND.(.NOT.FVERT)).OR.     &      
     ((ISX+(N+1)*MX.GE. 779).AND.(     FVERT))) THEN         
      IF(NUM.EQ.0) RETURN 
      FLINETL = .TRUE.    
      GOTO 40    
   ENDIF         
   IF(FFONT.AND.(STRING(M:M).EQ.'$')) THEN  
      M = M+1    
      FCHANGE = .TRUE.    
      GOTO 40    
   ENDIF         
   CHR = STRING(M:M)        
   IF(NFONT.EQ.2.AND.CHR.NE.' ') THEN       
      IC = ICHAR(CHR)       
!*IF IBM       
!CMS   IC = NEA(IC) 
!*ENDIF  
      IF(IC.LT.64) RETURN 
      FOCT = .FALSE.      
      IF(IC.LE.95.OR.IC.GT.122) THEN        
         OCT = SYMB(IC+128) 
         FOCT = .TRUE.    
      ENDIF      
   ENDIF         
   IF(CHR.EQ.'('.OR.CHR.EQ.')'.OR.CHR.EQ.BS1) THEN   
!     * INTERCEPT SPECIAL POSTSCRIPT CHARACTERS.     
      STROUT = STROUT(1:ILEN+1)//BS1//CHR   
      ILEN = ILEN+2   
   ELSEIF(FOCT) THEN      
!     * INTERCEPT SPECIAL SYMBOLS. 
      STROUT = STROUT(1:ILEN+1)//OCT        
      ILEN = ILEN+4       
      FOCT = .FALSE.      
   ELSE 
      STROUT = STROUT(1:ILEN+1)//CHR        
      ILEN = ILEN+1       
   ENDIF         
   M = M+1       
   NUM = NUM+1   
   N = N+1       
         GOTO 30 
       
!        * WRITING PART OF STRING WITH THE SAME FONT.         
   40    IF(ILEN.NE.0) THEN        
   IF(ISIZE.GT.0) THEN    
      IF(ILEN.LE.70) THEN 
         WRITE(FORM,'(A8,I3,A4)') '(I4,A2,A',ILEN,',A4)'      
         WRITE(ppp_unit_ps,FORM) MX,' (',STROUT(2:ILEN+1),') tw'      
      ELSE       
         WRITE(ppp_unit_ps,'(I4,A2,A70,A4)')  MX,' (',STROUT(2:71),') tw' 
         WRITE(FORM,'(A8,I3,A4)') '(I4,A2,A',ILEN-70,',A4)'   
         WRITE(ppp_unit_ps,FORM) MX,' (',STROUT(72:ILEN+1),') tw'     
      ENDIF      
   ELSE 
      IF(ILEN.LE.70) THEN 
         WRITE(FORM,'(A5,I3,A4)') '(A1,A',ILEN,',A4)'         
         WRITE(ppp_unit_ps,FORM) '(',STROUT(2:ILEN+1),') sh' 
      ELSE       
         WRITE(ppp_unit_ps,'(A1,A70,A4)') '(',STROUT(2:71),') sh'     
         WRITE(FORM,'(A5,I3,A4)') '(A1,A',ILEN-70,',A4)'      
         WRITE(ppp_unit_ps,FORM) '(',STROUT(72:ILEN+1),') sh'         
      ENDIF      
   ENDIF         
         ENDIF   
         ISX = ISX+ILEN*MX         
         IF(FLINETL) THEN 
   ISX = ISXOLD  
   IF(.NOT.FVERT) ISY = ISY-(MY+2*JSIZE)    
   IF(FVERT)      ISY = ISY+(MY+2*JSIZE)    
   IF(ISY.LT.0) RETURN    
   IF(.NOT.FVERT) WRITE(ppp_unit_ps,'(I4,1X,I4,A2)') ISX,ISY,' m'     
   IF(FVERT)      WRITE(ppp_unit_ps,'(I4,1X,I4,A2)') 0,ISYOLD-ISY,' m'         
   FLINETL = .FALSE.      
         ENDIF   
         IF(FCHANGE) THEN 
   NFONT = -NFONT+3       
   IF(NFONT.EQ.1) WRITE(ppp_unit_ps,'(I4,A5)') MY,' scaH'    
   IF(NFONT.EQ.2) WRITE(ppp_unit_ps,'(I4,A5)') MY,' scaS'    
   FCHANGE = .FALSE.      
         ENDIF   
       
      IF(NUM.LT.NCHR) GOTO 20      
       
!     * POST BEAM POSITION.        
   50 IF(.NOT.FVERT) CALL ps_MOVABS(real(ISX),real(ISY))  
      IF(FVERT) WRITE(ppp_unit_ps,'(A8)') 'grestore'         
      IF(FVERT) CALL ps_MOVABS(real(ISY),real(ISX))
      RETURN     
!       
!     * ENTRY FOR DRAWING VERTICALLY.       
      ENTRY ps_DLCV(IX,IY,STRING,NC,ISIZE)    
      IF(NC.EQ.0) RETURN  
      FVERT = .TRUE.      
      ISX = MIN(IABS(IY),779)        
      ISY = MIN(IABS(IX),1023)       
      IF(IX.LT.0) THEN
        CALL ps_SEELOC(ZISY,ZISX)
        ISX = INT(ZISX)
        ISY = INT(ZISY) 
      ENDIF    
      GOTO 10    
      END        
       
     
SUBROUTINE BEGPLT(NAME) 
!***********************************************************************        
!     BRANCHING TO BEGPLT1 (SVG) / BEGPLT2 (POSTSCRIPT).               *        
!***********************************************************************      
use svg_parameters
use ps_parameters

CHARACTER*(*) NAME  
       
COMMON /LHEAD1/LABTOP,LABBOT,D,T      
CHARACTER LABTOP*80,LABBOT*40,D*10,T*8 
COMMON /LHEAD2/NCT,NCB       
COMMON /KPOS/KP(36)   
       
KP = 0
 
LABTOP = ' '
LABBOT = ' '
D      = ' '
T      = ' '        

NCT = 1
NCB = 1   

IF (PPP_SVG) CALL SVG_BEGPLT(NAME//'.svg')      
IF (PPP_PS)  CALL PS_BEGPLT(NAME//'.ps')      

RETURN     
END        
       
    
SUBROUTINE SVG_BEGPLT(NAME)      
!***********************************************************************        
! first attempt SVG output
!***********************************************************************      
use svg_parameters

CHARACTER*(*) :: NAME
integer       :: xpos, ypos
       
OPEN(ppp_unit_svg,FILE=NAME)   

svg_page_index = 1

write(ppp_unit_svg,'(A)') '<?xml version="1.0"?>'

write(ppp_unit_svg,'(A)') '<svg version="1.1" baseProfile="full" onload="init(evt);" '
write(ppp_unit_svg,'(A)') '    xmlns="http://www.w3.org/2000/svg" '
write(ppp_unit_svg,'(A)') '    xmlns:xlink="http://www.w3.org/1999/xlink" '
!write(ppp_unit_svg,'(A)') '    width="20.00cm" height="15.23cm" '
write(ppp_unit_svg,'(A)') '    xml:space="preserve" '
write(ppp_unit_svg,'(A,2I5,A)')   'viewBox="0 0 ',INT(SVG_WIDTH)+10,INT(SVG_HEIGHT)+50,'" preserveAspectRatio="xMinYMin"'
write(ppp_unit_svg,'(A)') '>' 

write(ppp_unit_svg,'(A)') ' <defs> '

write(ppp_unit_svg,'(A)') ' <filter id="colorAdd"> '
write(ppp_unit_svg,'(A)') '  <feComposite in="SourceGraphic" in2="BackgroundImage" operator="arithmetic" ' // &
                          ' k1="0" k2="1." k3="1." k4="0"/> '
write(ppp_unit_svg,'(A)') ' </filter> ' 

write(ppp_unit_svg,'(A)') ' <filter id="A">'
write(ppp_unit_svg,'(A)') '  <feColorMatrix type="matrix" values=" '
write(ppp_unit_svg,'(A)') '     1 0 0 0 0'
write(ppp_unit_svg,'(A)') '     0 1 0 0 0'
write(ppp_unit_svg,'(A)') '     0 0 1 0 0'
write(ppp_unit_svg,'(A)') '     1 1 1 1 0'
write(ppp_unit_svg,'(A)') '     0 0 0 0 1 "/> '
write(ppp_unit_svg,'(A)') '</filter>'  

write(ppp_unit_svg,'(A)') '<linearGradient id="ig0" gradientUnits="userSpaceOnUse" x1="100." y1="100." x2="400." y2="300.">'
write(ppp_unit_svg,'(A)') '   <stop offset="0%"   stop-color="rgb(256,0,0)"  stop-opacity="1"/>'
write(ppp_unit_svg,'(A)') '   <stop offset="100%" stop-color="rgb(256,0,0)"  stop-opacity="0"/>'
write(ppp_unit_svg,'(A)') '</linearGradient>'

write(ppp_unit_svg,'(A)') '<path id="t0" d="M 100 100 L 500 100 L 300 500 z"  />'

write(ppp_unit_svg,'(A)') '<linearGradient id="ig1" gradientUnits="userSpaceOnUse" x1="500." y1="100." x2="200." y2="300.">'
write(ppp_unit_svg,'(A)') '   <stop offset="0%"   stop-color="rgb(0,256,0)"  stop-opacity="1" />'
write(ppp_unit_svg,'(A)') '   <stop offset="100%" stop-color="rgb(0,256,0)"  stop-opacity="0" />'
write(ppp_unit_svg,'(A)') '</linearGradient>'

write(ppp_unit_svg,'(A)') '<linearGradient id="ig2" gradientUnits="userSpaceOnUse" x1="300." y1="500." x2="300." y2="100.">'
write(ppp_unit_svg,'(A)') '   <stop offset="0%"   stop-color="rgb(0,0,256)"  stop-opacity="1" />'
write(ppp_unit_svg,'(A)') '   <stop offset="100%" stop-color="rgb(0,0,256)"  stop-opacity="0" />'
write(ppp_unit_svg,'(A)') '</linearGradient>'

write(ppp_unit_svg,'(A)') '<linearGradient id="button_fill" x1="0" y1="0" x2="0" y2="1">'
write(ppp_unit_svg,'(A)') '  <stop offset="1" stop-color="navy" />'
write(ppp_unit_svg,'(A)') '  <stop offset="0" stop-color="navy" />'
write(ppp_unit_svg,'(A)') '</linearGradient>'

write(ppp_unit_svg,'(A)') '<linearGradient id="button_light1" x1="0" y1="0" x2="0" y2="1">'
write(ppp_unit_svg,'(A)') '  <stop offset="0" stop-color="white" />'
write(ppp_unit_svg,'(A)') '  <stop offset="1" stop-color="white" stop-opacity="0" />'
write(ppp_unit_svg,'(A)') '</linearGradient>'

write(ppp_unit_svg,'(A)') '<linearGradient id="button_light2" x1="0" y1="0" x2="0" y2="1">'
write(ppp_unit_svg,'(A)') '  <stop offset="0" stop-color="black" stop-opacity="0" />'
write(ppp_unit_svg,'(A)') '  <stop offset="1" stop-color="black" />'
write(ppp_unit_svg,'(A)') '</linearGradient>'

write(ppp_unit_svg,'(A)') '<linearGradient id="button_light3" x1="0" y1="0" x2="0" y2="1">'
write(ppp_unit_svg,'(A)') '<stop offset="0" stop-color="white" stop-opacity="0" />'
write(ppp_unit_svg,'(A)') '<stop offset="1" stop-color="white" />'
write(ppp_unit_svg,'(A)') '</linearGradient>'

write(ppp_unit_svg,'(A)') '<linearGradient id="button_light4" x1="0" y1="0" x2="0" y2="1">'
write(ppp_unit_svg,'(A)') '<stop offset="0" stop-color="black"  />'
write(ppp_unit_svg,'(A)') '<stop offset="1" stop-color="black" stop-opacity="0"/>'
write(ppp_unit_svg,'(A)') '</linearGradient>'

write(ppp_unit_svg,'(A)') '<linearGradient id="button_light5" x1="0" y1="0" x2="0" y2="1">'
write(ppp_unit_svg,'(A)') '<stop offset="0" stop-color="rgb(200,200,100)"  />'
write(ppp_unit_svg,'(A)') '<stop offset="1" stop-color="rgb(200,200,200)" stop-opacity="0"/>'
write(ppp_unit_svg,'(A)') '</linearGradient>'

write(ppp_unit_svg,'(A)') ' <filter id = "high_light">'
write(ppp_unit_svg,'(A)') '        <feSpecularLighting result = "specOut" in = "SourceAlpha" specularExponent = "60" ' // &
                          ' lighting-color = "white">'
write(ppp_unit_svg,'(A)') '            <fePointLight x = "-5" y = "5" z = "75"/>'
write(ppp_unit_svg,'(A)') '        </feSpecularLighting>'
write(ppp_unit_svg,'(A)') '        <feComposite in = "SourceGraphic" in2 = "specOut" operator = "arithmetic" k1 = "0" '// &
                          ' k2 = "1" k3 = "1" k4 = "0"/>'
write(ppp_unit_svg,'(A)') '</filter>'

write(ppp_unit_svg,'(A)') '<g id="page_button_up" >'
write(ppp_unit_svg,'(A)') '  <rect x="0"   y="0"  width="40" height="30"  rx="5" ry="5" fill="url(#button_fill)" />'
write(ppp_unit_svg,'(A)') '  <rect x="1"   y="1"  width="38" height="17"  rx="4" ry="4" fill="url(#button_light1)" />'
write(ppp_unit_svg,'(A)') '  <rect x="1"   y="21" width="38" height="8"   rx="4" ry="4" fill="url(#button_light2)" />'
write(ppp_unit_svg,'(A)') '</g>'

write(ppp_unit_svg,'(A)') '<g id="page_button_down">'
write(ppp_unit_svg,'(A)') '  <rect x="0" y="0"  width="40" height="30" rx="5" ry="5" fill="navy" />'
write(ppp_unit_svg,'(A)') '  <rect x="1" y="1"  width="38" height="8"  rx="4" ry="4" fill="url(#button_light4)" />'
write(ppp_unit_svg,'(A)') '  <rect x="1" y="12" width="38" height="17" rx="4" ry="4" fill="url(#button_light3)" />'
write(ppp_unit_svg,'(A)') '  <rect x="-2"  y="-2" width="44" height="34"  rx="7" ry="7" fill="none" '//&
                          ' stroke="rgb(240, 240, 30)" stroke-width="4" />'
write(ppp_unit_svg,'(A)') '</g>'

write(ppp_unit_svg,'(A)') ' </defs>'

write(ppp_unit_svg,'(A)') '<title>PPPLIB Version 11 (SVG)</title>' 
write(ppp_unit_svg,'(A)') '<style type="text/css">'
write(ppp_unit_svg,'(A)') 'path {stroke:black;stroke-width:1;}'
write(ppp_unit_svg,'(A)') 'line {stroke:black;stroke-width:1;}'
write(ppp_unit_svg,'(A)') '</style>'
write(ppp_unit_svg,'(A)') '<defs>'
write(ppp_unit_svg,'(A)') '<script type="text/ecmascript"> <![CDATA['
write(ppp_unit_svg,'(A)') 'var current_page;'
write(ppp_unit_svg,'(A)') 'var current_button;'
write(ppp_unit_svg,'(A)') '   function init(evt) {'
write(ppp_unit_svg,'(A)') '     current_page   = document.getElementById("page001")'
write(ppp_unit_svg,'(A)') '     current_button = document.getElementById("button001")'
write(ppp_unit_svg,'(A)') '     }'
write(ppp_unit_svg,'(A)') '   function change_page(evt,index) {'
write(ppp_unit_svg,'(A)') '     var page_id=''page''+index;'
write(ppp_unit_svg,'(A)') '     var button_id=''button''+index;'
write(ppp_unit_svg,'(A)') '     var this_button = document.getElementById(button_id)'
write(ppp_unit_svg,'(A)') '     var this_page   = document.getElementById(page_id);'
write(ppp_unit_svg,'(A)') '     if (this_page != current_page) {'
write(ppp_unit_svg,'(A)') '       this_button.setAttribute("xlink:href","#page_button_down");'
write(ppp_unit_svg,'(A)') '       current_button.setAttribute("xlink:href","#page_button_up");'
write(ppp_unit_svg,'(A)') '       current_page.setAttribute("display","none");'
write(ppp_unit_svg,'(A)') '       this_page.setAttribute("display","");'
write(ppp_unit_svg,'(A)') '       current_page   = this_page;'
write(ppp_unit_svg,'(A)') '       current_button = this_button;'
write(ppp_unit_svg,'(A)') '     }'
write(ppp_unit_svg,'(A)') '   }'
write(ppp_unit_svg,'(A)') '   function big_button(evt) {'                               
write(ppp_unit_svg,'(A)') '     var mouse_target = evt.currentTarget;'         
write(ppp_unit_svg,'(A)') '     mouse_target.setAttribute("filter","url(#high_light)");'
write(ppp_unit_svg,'(A)') '     }'                                                             
write(ppp_unit_svg,'(A)') '   function normal_button(evt) {'                               
write(ppp_unit_svg,'(A)') '     var mouse_target = evt.currentTarget;'         
write(ppp_unit_svg,'(A)') '     mouse_target.setAttribute("filter"," ");'
write(ppp_unit_svg,'(A)') '     } '                                                            

write(ppp_unit_svg,'(A)') ' ]]> </script>'
write(ppp_unit_svg,'(A)') '</defs>'
                    
write(ppp_unit_svg,'(A)')
!write(ppp_unit_svg,'(A,I5,A,I5,A)') '<rect x="1" y="1" width="',INT(SVG_WIDTH-1),'" height="',INT(SVG_HEIGHT-1), &
!                                    '" style="stroke:black; stroke-width:1; fill:#EEEEEE" />'

!write(ppp_unit_svg,'(A)') ' <pageSet>'      ! for multipage SVG documents (but not well supported yet)
!write(ppp_unit_svg,'(A)') ' <page>'

xpos=20
ypos = svg_height+10

write(ppp_unit_svg,'(A,i3,A,i3,A,i3.3,A)') '<g transform="translate(',xpos,',',ypos,')" '// &
                                           ' onclick="change_page(evt,''',svg_page_index,''')" >'
write(ppp_unit_svg,'(A,i3.3,A)') '<g onmouseover=''big_button(evt);'' onmouseout=''normal_button(evt);'' >' 					   
write(ppp_unit_svg,'(A,i3.3,A)') '<use id="button',svg_page_index,'" xlink:href="#page_button_down" />'
write(ppp_unit_svg,'(A,i3,A)') '<text x="18" y="22" text-anchor="middle" fill="white" font-family="Helvetica" font-size="20">', &
                               svg_page_index,'</text>'
write(ppp_unit_svg,'(A)') '</g></g>'

write(ppp_unit_svg,'(A)') ' <g id="page001">'

svg_rgb = (/ 0, 0, 0/)
svg_linewidth = 1.

RETURN     
END        
       
    
SUBROUTINE ps_BEGPLT(NAME)
!***********************************************************************        
!     POSTSCRIPT VERSION: WRITE HEADER AND DEFINITIONS.                *        
!***********************************************************************      
use ps_parameters
       
CHARACTER*(*) NAME 
       
OPEN(ppp_unit_ps,FILE=NAME)   
WRITE(ppp_unit_ps,'(A/A9,A8/A/A/A/A/A/A/A)')      & 
'%!PS-Adobe-2.0',     &      
'%%Title: ',NAME,     &
'%%Creator: PPPLIB',  &
'%%LanguageLevel: 3', &
'%%Pages: (atend)',   &
'%%For: PPPLIB',      &      
'%%EndComments',      &      
'%%EndProlog',        &      
'%%Begin Setup'
WRITE(ppp_unit_ps,'(7(A/),A)')         &      
'.60 .60 scale 900 180 translate 90 rotate',  &         
'1. setlinewidth', & 
'/l {lineto} def /m {moveto} def /sf {setrgbcolor fill} def',   &         
'/rl {rlineto} def /rm {rmoveto} def',        &         
'/sh {show} def /st {stroke} def',   &         
'/pt {l .4 setlinewidth st .1 setlinewidth} def',      & 
'/scaH {/Helvetica findfont exch scalefont setfont} def',       &         
'/scaS {/Symbol findfont exch scalefont setfont} def'  
WRITE(ppp_unit_ps,'(17(A/),A)')    &
'/tw  % typewrite (str) with dx=skip.',      & 
' {/str exch def /skip exch def',   & 
'  str {/charcode exch def /char ( ) dup 0 charcode put def',  & 
'   skip 2 div 0 rm gsave',         & 
'   char stringwidth pop 2 div neg 0 rm',    & 
'   char show grestore skip 2 div 0 rm} forall} def', & 
'/tc  % type centered character.',  & 
' {/ch exch def /y exch def /x exch def',    & 
'  gsave newpath 0 0 m',   & 
'  ch true charpath flattenpath pathbbox',   & 
'  /ury exch def /urx exch def /lly exch def /llx exch def',   & 
'  urx llx add 2 div /dx exch def', & 
'  ury lly add 2 div /dy exch def grestore', & 
'  x dx sub y dy sub m ch sh} def'   
WRITE(ppp_unit_ps,'(A)') '/sh_left { /str exch def /angle exch def /y1 exch def /x1 exch def'
WRITE(ppp_unit_ps,'(A)') '          gsave x1 y1 translate angle rotate 0 0 moveto str show grestore} def'
WRITE(ppp_unit_ps,'(A)') '/sh_right { /str exch def /angle exch def /y1 exch def /x1 exch def str' 
WRITE(ppp_unit_ps,'(A)') '          stringwidth /wy exch def /wx exch def' 
WRITE(ppp_unit_ps,'(A)') '          gsave x1 wx angle cos mul sub y1 wx angle sin mul sub translate' 
WRITE(ppp_unit_ps,'(A)') '	        angle rotate 0 0 moveto str show grestore } def'
WRITE(ppp_unit_ps,'(A)') '/rgb2 {/cc exch def' 
WRITE(ppp_unit_ps,'(A)') '2 cc 4 mul sub     /bb exch def    %blue'
WRITE(ppp_unit_ps,'(A)') '2 4 cc mul 2 sub abs sub    /gg exch def    %green'
WRITE(ppp_unit_ps,'(A)') 'cc 4 mul 2 sub     /rr exch def    %red'
WRITE(ppp_unit_ps,'(A)') 'rr gg bb'
WRITE(ppp_unit_ps,'(A)') '} def'
WRITE(ppp_unit_ps,'(A)') ' /mx {/v1 exch def v1 1 gt {1} {v1} ifelse} def'
WRITE(ppp_unit_ps,'(A)') ' /mi {/v1 exch def v1 0 lt {0} {v1} ifelse} def'
WRITE(ppp_unit_ps,'(A)') '   /rgb {/cc exch def 1 cc sub /dd exch def'
WRITE(ppp_unit_ps,'(A)') '  1 6 dd mul 3 sub abs sub mx mi /r2 exch def'
WRITE(ppp_unit_ps,'(A)') '  6 dd mul 5 sub mx mi           /r3 exch def'
WRITE(ppp_unit_ps,'(A)') '  1 12 dd mul sub mx mi          /r4 exch def'
WRITE(ppp_unit_ps,'(A)') '  dd 4 mul 2 sub mx mi r2 add    /bb exch def'
WRITE(ppp_unit_ps,'(A)') '  2 4 dd mul 2 sub abs sub       /gg exch def'
WRITE(ppp_unit_ps,'(A)') '  2 dd 4 mul sub mx mi r2 add r3 add r4 sub /rr exch def'
WRITE(ppp_unit_ps,'(A)') '  rr gg bb'
WRITE(ppp_unit_ps,'(A)') '  } def'

WRITE(ppp_unit_ps,'(A)') '/tri0 { add add 3 div rgb'                           
WRITE(ppp_unit_ps,'(A)') ' moveto lineto lineto closepath fill '              
WRITE(ppp_unit_ps,'(A)') '} def'                                              
WRITE(ppp_unit_ps,'(A)') '0 /nql exch def'                                    
WRITE(ppp_unit_ps,'(A)') '/ftr {/nq exch def'                                 
WRITE(ppp_unit_ps,'(A)') '%recursive,lowest level,fill triangle with average' 
WRITE(ppp_unit_ps,'(A)') 'nq 0 eq  { tri0 } if'                                
WRITE(ppp_unit_ps,'(A)') '% next level'                                       
WRITE(ppp_unit_ps,'(A)') 'nq 0 gt {'                                          
WRITE(ppp_unit_ps,'(A)') 'nq 1 sub /nq exch def'                              
WRITE(ppp_unit_ps,'(A)') '/c3 exch def /c2 exch def /c1 exch def'             
WRITE(ppp_unit_ps,'(A)') '/y3 exch def /x3 exch def'                          
WRITE(ppp_unit_ps,'(A)') '/y2 exch def /x2 exch def'                          
WRITE(ppp_unit_ps,'(A)') '/y1 exch def /x1 exch def'                          
WRITE(ppp_unit_ps,'(A)') 'x1 x2 add 2 div /x12 exch def'                      
WRITE(ppp_unit_ps,'(A)') 'y1 y2 add 2 div /y12 exch def'                      
WRITE(ppp_unit_ps,'(A)') 'c1 c2 add 2 div /c12 exch def'                      
WRITE(ppp_unit_ps,'(A)') 'x1 x3 add 2 div /x13 exch def'                      
WRITE(ppp_unit_ps,'(A)') 'y1 y3 add 2 div /y13 exch def'                      
WRITE(ppp_unit_ps,'(A)') 'c1 c3 add 2 div /c13 exch def'                      
WRITE(ppp_unit_ps,'(A)') 'x2 x3 add 2 div /x23 exch def'                      
WRITE(ppp_unit_ps,'(A)') 'y2 y3 add 2 div /y23 exch def'                      
WRITE(ppp_unit_ps,'(A)') 'c2 c3 add 2 div /c23 exch def'                      
WRITE(ppp_unit_ps,'(A)') 'x1  y1     x12 y12    x13 y13   c1  c12 c13  nq'    
WRITE(ppp_unit_ps,'(A)') 'x3  y3     x13 y13    x23 y23   c3  c13 c23  nq'    
WRITE(ppp_unit_ps,'(A)') 'x2  y2     x12 y12    x23 y23   c2  c12 c23  nq'    
WRITE(ppp_unit_ps,'(A)') 'x12 y12    x13 y13    x23 y23   c12 c13 c23  nq'    
WRITE(ppp_unit_ps,'(A)') 'ftr ftr ftr ftr '                                   
WRITE(ppp_unit_ps,'(A)') '} if'                                               
WRITE(ppp_unit_ps,'(A)') '} def'                                              

!----------------------------------------- psotscript level 3 shading (Guido Huysmans 5/6/2006)
WRITE(ppp_unit_ps,'(A)')  '/tri{'                                                                                                 
WRITE(ppp_unit_ps,'(A)')  ' /c3 exch def /c2 exch def /c1 exch def '   
WRITE(ppp_unit_ps,'(A)')  ' /y3 exch def /x3 exch def '               
WRITE(ppp_unit_ps,'(A)')  ' /y2 exch def /x2 exch def '       
WRITE(ppp_unit_ps,'(A)')  ' /y1 exch def /x1 exch def '             
WRITE(ppp_unit_ps,'(A)')  '     << /ShadingType 4           '       
WRITE(ppp_unit_ps,'(A)')  '        /ColorSpace /DeviceRGB   '       
WRITE(ppp_unit_ps,'(A)')  '        /DataSource [            '       
WRITE(ppp_unit_ps,'(A)')  '       % flag     x   y   R G B  '         
WRITE(ppp_unit_ps,'(A)')  '           0     x1  y1   c1 rgb '       
WRITE(ppp_unit_ps,'(A)')  '           0     x2  y2   c2 rgb '       
WRITE(ppp_unit_ps,'(A)')  '           0     x3  y3   c3 rgb '       
WRITE(ppp_unit_ps,'(A)')  '           ] '                             
WRITE(ppp_unit_ps,'(A)')  '     >> shfill '                           
WRITE(ppp_unit_ps,'(A)')  '   } def'   
WRITE(ppp_unit_ps,'(A)') '/quad{'
WRITE(ppp_unit_ps,'(A)') '  /b4 exch def /g4 exch def /r4 exch def  /y4 exch def /x4 exch def'
WRITE(ppp_unit_ps,'(A)') '  /b3 exch def /g3 exch def /r3 exch def  /y3 exch def /x3 exch def'
WRITE(ppp_unit_ps,'(A)') '  /b2 exch def /g2 exch def /r2 exch def  /y2 exch def /x2 exch def'
WRITE(ppp_unit_ps,'(A)') '  /b1 exch def /g1 exch def /r1 exch def  /y1 exch def /x1 exch def'     
WRITE(ppp_unit_ps,'(A)') '      << /ShadingType 4           '
WRITE(ppp_unit_ps,'(A)') '         /ColorSpace /DeviceRGB   '
WRITE(ppp_unit_ps,'(A)') '         /DataSource [            '
WRITE(ppp_unit_ps,'(A)') '        % flag     x   y   R G B     '
WRITE(ppp_unit_ps,'(A)') '            0     x1  y1   r1 g1 b1  ' 
WRITE(ppp_unit_ps,'(A)') '            0     x2  y2   r2 g2 b2  ' 
WRITE(ppp_unit_ps,'(A)') '            0     x3  y3   r3 g3 b3  '
WRITE(ppp_unit_ps,'(A)') '            0     x4  y4   r4 g4 b4  '
WRITE(ppp_unit_ps,'(A)') '            0     x3  y3   r3 g3 b3  '
WRITE(ppp_unit_ps,'(A)') '	    0     x1  y1   r1 g1 b1 '
WRITE(ppp_unit_ps,'(A)') '	    ] '
WRITE(ppp_unit_ps,'(A)') '      >> shfill' 
WRITE(ppp_unit_ps,'(A)') '} def'

WRITE(ppp_unit_ps,'(A)')  '%%End Setup'
WRITE(ppp_unit_ps,'(A)')  'newpath'
WRITE(ppp_unit_ps,'(A)')  '%%Page: 1 1'
WRITE(ppp_unit_ps,'(A)')  '%%start plotting' 
 
ps_page_index = 1    
ps_ZIXSAV = 0.  
ps_ZIYSAV = 0.  
ps_rgb = (/ 0., 0., 0. /)
ps_linewidth = 1.

RETURN     
END        
       
           
SUBROUTINE FINPLT 
!***********************************************************************        
!     BRANCHING TO FINPLT1 (SVG) / FINPLT2 (POSTSCRIPT).  *        
!***********************************************************************      
use svg_parameters
use ps_parameters

IF (PPP_SVG) CALL svg_FINPLT   
IF (PPP_PS)  CALL ps_FINPLT

RETURN     
END        
       
    
SUBROUTINE svg_FINPLT         
!***********************************************************************        
!     THIS ROUTINE IS CALLED AFTER ALL PLOTTING IN A CODE IS FINISHED. *        
!     SVG version                                                      *     
!***********************************************************************        
use svg_parameters

write(ppp_unit_svg,*) '</g>'
!write(ppp_unit_svg,*) '</page>'
!write(ppp_unit_svg,*) '</pageSet>'
write(ppp_unit_svg,*) '</svg>'
       
close(ppp_unit_svg)

RETURN     
END        
       
    
SUBROUTINE ps_FINPLT       
!***********************************************************************        
!     POSTSCRIPT VERSION. *        
!***********************************************************************      
use ps_parameters

WRITE(ppp_unit_ps,'(A/A)')   'stroke gsave showpage grestore','%%Trailer'      
WRITE(ppp_unit_ps,'(A,I5)')  '%%Pages: ',ps_page_index
WRITE(ppp_unit_ps,'(A)')     '%%EOF'   
CLOSE(ppp_unit_ps)   
       
RETURN     
END        
       
        
SUBROUTINE ADV(N) 
!***********************************************************************        
!     BRANCHING TO ADV1 (CALCOMP) / ADV2 (POSTSCRIPT).        *        
!***********************************************************************      
use svg_parameters
use ps_parameters

IF (PPP_SVG) CALL svg_ADV(N)     
IF (PPP_PS)  CALL ps_ADV(N)     

RETURN     
END        
       
       
SUBROUTINE svg_ADV(N)      
!***********************************************************************        
!     THIS ROUTINE ADVANCES N PLOTTING PAGES (ONLY SENSIBLE FOR N=1).  *        
!***********************************************************************      
use svg_parameters
integer :: xpos, ypos

svg_page_index = svg_page_index + N         

write(ppp_unit_svg,'(A)') '</g>'
!write(ppp_unit_svg,'(A)') '</page>'
!write(ppp_unit_svg,'(A)') '<page>'

xpos = 20 + 45*(svg_page_index-1)
ypos = svg_height + 10

write(ppp_unit_svg,'(A,i3,A,i3,A,i3.3,A)') '<g transform="translate(',xpos,',',ypos,')" '// &
                                           ' onclick="change_page(evt,''',svg_page_index,''')" >'
write(ppp_unit_svg,'(A,i3.3,A)') '<g onmouseover=''big_button(evt);'' onmouseout=''normal_button(evt);'' >' 					   
write(ppp_unit_svg,'(A,i3.3,A)') '<use id="button',svg_page_index,'" xlink:href="#page_button_up" />'
write(ppp_unit_svg,'(A,i3,A)') '<text x="18" y="22" text-anchor="middle" fill="white" font-family="Helvetica" font-size="20">', &
                               svg_page_index,'</text>'
write(ppp_unit_svg,'(A)') '</g></g>'

write(ppp_unit_svg,'(A,i3.3,A)') '<g id="page',svg_page_index,'" display="none" >'

RETURN     
END        
       
       
SUBROUTINE ps_ADV(N)      
!***********************************************************************        
!     POSTSCRIPT VERSION.                                              *        
!***********************************************************************       
use ps_parameters
       
COMMON /ADVPAGE/ADVP
LOGICAL ADVP
       
ps_page_index = ps_page_index + n

if (ps_page_index .NE. 0) then
  WRITE(ppp_unit_ps,'(A30/A8,I3,I3)') 'stroke gsave showpage grestore','%%Page: ',ps_page_index,ps_page_index
endif
      
ADVP= .TRUE.

RETURN     
END        
       
        
SUBROUTINE DRV(ZIX1,ZIY1,ZIX2,ZIY2)         
!***********************************************************************        
!     BRANCHING TO svg_ADV1 (SVG) / ps_ADV (POSTSCRIPT).        *        
!***********************************************************************        
use svg_parameters
use ps_parameters

IF (PPP_SVG) CALL svg_DRV(ZIX1,ZIY1,ZIX2,ZIY2)       
IF (PPP_PS)  CALL ps_DRV(ZIX1,ZIY1,ZIX2,ZIY2)       

RETURN     
END        
       
       
SUBROUTINE svg_DRV(ZIX1,ZIY1,ZIX2,ZIY2)        
!***********************************************************************        
!     THIS ROUTINE DRAWS A LINE VECTOR FROM (IX1,IY1) TO (IX2,IY2).    *        
!***********************************************************************        
use svg_parameters

write(ppp_unit_svg,'(A,2F9.3,A,2F9.3,A,A,i3,A,i3,A,i3,A,f5.2,A)') &
     '<path d="M ',ZIX1,SVG_HEIGHT-ZIY1,' L ',ZIX2,SVG_HEIGHT-ZIY2,' z" ', &
     'style="stroke:rgb(',svg_rgb(1),',',svg_rgb(2),',',svg_rgb(3),'); stroke-width:',svg_linewidth,' " />'

svg_ZIXSAV = ZIX2         
svg_ZIYSAV = ZIY2         

RETURN     
END        
       
       
SUBROUTINE ps_DRV(ZIX1,ZIY1,ZIX2,ZIY2)        
!***********************************************************************        
!     POSTSCRIPT VERSION.                                              *        
!***********************************************************************      
use ps_parameters

SAVE NUMLIN         
DATA NUMLIN / 0 /   
       
NUMLIN = NUMLIN+1   
IF(NUMLIN.GE.30) THEN        
  WRITE(ppp_unit_ps,'(A2)') 'st'    
  NUMLIN = 0       
ENDIF      
  
WRITE(ppp_unit_ps,'(F8.3,1X,F8.3,A3,F8.3,1X,F8.3,A2)')  ZIX1,ZIY1,' m ',ZIX2,ZIY2,' l'         
       
ps_ZIXSAV = ZIX2         
ps_ZIYSAV = ZIY2         

RETURN     
END        
       
        
SUBROUTINE DRP(ZIX,ZIY)
!***********************************************************************        
!     BRANCHING TO DRP1 (CALCOMP) / DRP2 (POSTSCRIPT).        *        
!***********************************************************************      
use svg_parameters
use ps_parameters

IF (PPP_SVG) CALL svg_DRP(ZIX,ZIY) 
IF (PPP_PS)  CALL ps_DRP(ZIX,ZIY) 

RETURN     
END        
       
       
SUBROUTINE svg_DRP(ZIX,ZIY) 
!***********************************************************************        
!     THIS ROUTINE DRAWS A POINT AT THE LOCATION (IX,IY).     *        
!***********************************************************************        
use svg_parameters

! put a svg circle here

RETURN     
END        
       
       
SUBROUTINE ps_DRP(ZIX,ZIY)
!***********************************************************************        
!     POSTSCRIPT VERSION. *        
!***********************************************************************      
use ps_parameters       
       
WRITE(ppp_unit_ps,'(a3,F8.3,1X,F8.3,A3,F8.3,1X,F8.3,A3)') 'st ',ZIX+1.,ZIY,' m ',ZIX,ZIY,' pt' 

ps_ZIXSAV = ZIX 
ps_ZIYSAV = ZIY 

RETURN     
END        
   
SUBROUTINE MOVABS(ZIX,ZIY)
!***********************************************************************        
!     BRANCHING TO MOVABS1 (CALCOMP) / MOVABS2 (POSTSCRIPT).  *        
!***********************************************************************      
use svg_parameters
use ps_parameters

IF(PPP_SVG) CALL svg_MOVABS(ZIX,ZIY)     
IF(PPP_PS)  CALL ps_MOVABS(ZIX,ZIY)     

RETURN    
END        
       
    
SUBROUTINE svg_MOVABS(ZIX,ZIY)
!***********************************************************************        
!     THIS ROUTINE MOVES THE DRAWING BEAM TO THE LOCATION (IX,IY).     *        
!***********************************************************************      
use svg_parameters

svg_ZIXSAV = ZIX 
svg_ZIYSAV = ZIY 

RETURN     
END        
       
    
SUBROUTINE ps_MOVABS(ZIX,ZIY) 
!***********************************************************************        
!     POSTSCRIPT VERSION.                                              *        
!***********************************************************************      
use ps_parameters
              
SAVE NUMBIN         
DATA NUMBIN / 0 /   
       
NUMBIN = NUMBIN+1   
IF(NUMBIN.GE.50) THEN        
   WRITE(ppp_unit_ps,'(A2)') 'st'    
   NUMBIN = 0       
ENDIF      
WRITE(ppp_unit_ps,'(F8.3,1X,F8.3,A2)') ZIX,ZIY,' m' 

ps_ZIXSAV = ZIX 
ps_ZIYSAV = ZIY 

RETURN     
END        
       
     
SUBROUTINE DRWABS(ZIX,ZIY)
!***********************************************************************        
!     BRANCHING TO DRWABS1 (CALCOMP) / DRWABS2 (POSTSCRIPT).  *        
!***********************************************************************      
use svg_parameters
use ps_parameters

IF (PPP_SVG) CALL svg_DRWABS(ZIX,ZIY)     
IF (PPP_PS)  CALL ps_DRWABS(ZIX,ZIY)     

RETURN     
END        
       
    
SUBROUTINE svg_DRWABS(ZIX,ZIY) 
!***********************************************************************        
!     THIS ROUTINE DRAWS A LINE VECTOR FROM THE CURRENT BEAM POSITION  *        
! TO (IX,IY), WHICH BECOMES THE NEW BEAM POSITION.                     *        
!***********************************************************************        
use svg_parameters

IF (ZIX .EQ. svg_ZIXSAV .AND. ZIY .EQ. svg_ZIYSAV) THEN  

  WRITE(ppp_unit_svg,'(A3,F8.3,1X,F8.3,A3,F8.3,1X,F8.3,A4,F8.3,1X,F8.3,A2)') &        
                    'st ',ZIX+1.,ZIY,' m ',ZIX,ZIY,' pt ',ZIX,ZIY,' m'    
ELSE       
  write(ppp_unit_svg,'(A,2F9.3,A,2F9.3,A,A,i3,A,i3,A,i3,A,f5.2,A)') &
             '<path d="M ',svg_ZIXSAV,SVG_HEIGHT-svg_ZIYSAV,' L ',ZIX,SVG_HEIGHT-ZIY,' z" ', &
             'style="stroke:rgb(',svg_rgb(1),',',svg_rgb(2),',',svg_rgb(3),') stroke-width:',svg_linewidth,'" />'
ENDIF      

svg_ZIXSAV = ZIX 
svg_ZIYSAV = ZIY 
      
RETURN     
END        
       
    
SUBROUTINE ps_DRWABS(ZIX,ZIY)   
!***********************************************************************        
!     POSTSCRIPT VERSION.                                              *        
!***********************************************************************      
use ps_parameters
       
IF (ZIX .EQ. ps_ZIXSAV .AND. ZIY .EQ. ps_ZIYSAV) THEN  

  WRITE(ppp_unit_ps,'(A3,F8.3,1X,F8.3,A3,F8.3,1X,F8.3,A4,F8.3,1X,F8.3,A2)') &        
                    'st ',ZIX+1.,ZIY,' m ',ZIX,ZIY,' pt ',ZIX,ZIY,' m'    
ELSE       
  WRITE(ppp_unit_ps,'(F8.3,1X,F8.3,A2)') ZIX,ZIY,' l'       
ENDIF      

ps_ZIXSAV = ZIX 
ps_ZIYSAV = ZIY 

RETURN     
END        
       
    
SUBROUTINE svg_SEELOC(ZIX,ZIY)
!***********************************************************************        
!     THIS ROUTINE LOOKS UP THE CURRENT POSITION OF THE DRAWING BEAM.  *        
! COMMON /LIB8X1/ ITSELF SHOULD NOT BE USED FOR THIS PURPOSE SINCE ITS *        
! CONTENTS SHOULD REMAIN SHARED AND AFFECTED ONLY BY THE LOWEST-LEVEL  *        
! SYSTEM-DEPENDENT DRAWING ROUTINES.        *        
! NOTE THAT THERE IS NO BRANCHING ROUTINE SEELOC SINCE THE OUTPUT VAR- *        
! IABLES IX,IY OF SEELOC1 AND SEELOC2 MAY DIFFER IN PRINCIPLE.         *        
!***********************************************************************      
use svg_parameters

ZIX = svg_ZIXSAV 
ZIY = svg_ZIYSAV

RETURN     
END        
       
    
SUBROUTINE ps_SEELOC(ZIX,ZIY)
!***********************************************************************        
!     POSTSCRIPT VERSION.                                              *        
!***********************************************************************      
use ps_parameters
       
ZIX = ps_ZIXSAV 
ZIY = ps_ZIYSAV 
RETURN     
END        
       
       
SUBROUTINE PLOT(A,B,I)         
RETURN
END

SUBROUTINE PLOTS(A,B,NAME)     
CHARACTER*10 NAME
RETURN
END

   
SUBROUTINE ppp_set_linewidth(width)
!***********************************************************************   
! ROUTINE TO CHANGE THE CURRENT COLOR                                  *
!***********************************************************************
use ppp_parameters
use ps_parameters
use svg_parameters
real :: width

IF (PPP_SVG) call ppp_set_svg_linewidth(width)
IF (PPP_PS)  call ppp_set_ps_linewidth(width)

return
end 


SUBROUTINE ppp_set_ps_linewidth(width)
!***********************************************************************   
! ROUTINE TO CHANGE COLOR IN POST-SCRIPT VERSION ONLY                  *
!***********************************************************************
use ppp_parameters
use ps_parameters
real :: width

ps_linewidth = width

write(ppp_unit_ps,*) ' stroke'
write(ppp_unit_ps,'(f4.1,A)') width,' setlinewidth'

return
end 


SUBROUTINE ppp_set_svg_linewidth(width)
!***********************************************************************   
! ROUTINE TO CHANGE COLOR IN POST-SCRIPT VERSION ONLY                  *
!***********************************************************************
use ppp_parameters
use svg_parameters
real :: width

svg_linewidth = width

return
end 
   
SUBROUTINE ppp_set_color(icol)
!***********************************************************************   
! ROUTINE TO CHANGE THE CURRENT COLOR                                  *
!***********************************************************************
use ppp_parameters
use ps_parameters
use svg_parameters

icol2 = min(max(icol+1,1),10)

IF (PPP_SVG) call ppp_set_svg_color(icol2)
IF (PPP_PS)  call ppp_set_ps_color(icol2)

return
end 


SUBROUTINE ppp_set_ps_color(icol)
!***********************************************************************   
! ROUTINE TO CHANGE COLOR IN POST-SCRIPT VERSION ONLY                  *
!***********************************************************************
use ppp_parameters
use ps_parameters

write(ppp_unit_ps,*) ' stroke'
write(ppp_unit_ps,'(3f6.2,A)') some_colors(:,icol),' setrgbcolor'

return
end 

SUBROUTINE ppp_set_svg_color(ICOL)
!***********************************************************************   
! ROUTINE TO CHANGE COLOR IN POST-SCRIPT VERSION ONLY                  *
!***********************************************************************
use ppp_parameters
use svg_parameters

svg_rgb = int(255*some_colors(:,icol))

return
end 

SUBROUTINE ppp_set_rgb(rgb)
!***********************************************************************   
! ROUTINE TO CHANGE THE CURRENT COLOR                                  *
!***********************************************************************
use ppp_parameters
use ps_parameters
use svg_parameters

real :: rgb(3)

IF (PPP_SVG) call ppp_set_svg_rgb(rgb)
IF (PPP_PS)  call ppp_set_ps_rgb(rgb)

return
end 


SUBROUTINE ppp_set_ps_rgb(rgb)
!***********************************************************************   
! ROUTINE TO CHANGE COLOR                                              *
!***********************************************************************
use ps_parameters
real :: rgb(3)

ps_rgb = rgb

write(ppp_unit_ps,*) ' stroke'
write(ppp_unit_ps,'(i5,i5,i5,A)') ps_rgb,' setrgbcolor'

return
end 

SUBROUTINE ppp_set_svg_rgb(rgb)
!***********************************************************************   
! ROUTINE TO CHANGE COLOR                                              *
!***********************************************************************
use svg_parameters
real :: rgb(3)

svg_rgb = int(255*rgb)

return
end 

subroutine filltria(x,y,z,zmin,zmax)
use ps_parameters
use svg_parameters
REAL X(3),Y(3),Z(3), ZMIN, ZMAX

if (PPP_PS)  call ps_filltria(x,y,z,zmin,zmax)
if (PPP_SVG) call svg_filltria(x,y,z,zmin,zmax)

return
end

SUBROUTINE ps_filltria(X,Y,Z,ZMIN,ZMAX)
use ps_parameters

REAL X(3),Y(3),Z(3),Z2(3), ZMIN, ZMAX

DO I=1,3
  Z2(I) = (Z(I)-ZMIN)/(ZMAX-ZMIN)
ENDDO

WRITE(ppp_unit_ps,'(6f8.2,3f8.3,A)') X(1),Y(1),X(2),Y(2),X(3),Y(3), &
                                     max(0.,min(1.,Z2(1))),max(0.,min(1.,Z2(2))), max(0.,min(1.,Z2(3))),' tri'
      
RETURN
END


SUBROUTINE svg_filltria(X,Y,Z,ZMIN,ZMAX)
use svg_parameters

real    :: X(3),Y(3),Z(3),Z2(3), ZMIN, ZMAX, color, rgb(3)
integer :: irgb(3)

DO I=1,3
  Z2(I) = (Z(I)-ZMIN)/(ZMAX-ZMIN)
ENDDO

color = (z2(1)+z2(2)+z2(3))/3.

call palette(color,rgb)

irgb = 255*rgb

write(ppp_unit_svg,'(A,2F9.3,A,2F9.3,A,2F9.3,A,A,i3,A,i3,A,i3,A,f5.2,A,i3,A,i3,A,i3,A)') &
                   '<path d="M ',X(1),SVG_HEIGHT-Y(1),' L ',X(2),SVG_HEIGHT-Y(2),' L ',X(3),SVG_HEIGHT-Y(3),' z" ', &
                   'style="stroke:none;shape-rendering:crispEdges;fill:rgb(',irgb(1),',',irgb(2),',',irgb(3),') " />'

return
end

SUBROUTINE COLORBAR(ZC,NLAB,XR,YT,YB)
!-----------------------------------------------------------------------
! subroutine to add a colorbar next to a contour plot using the default
! colors defined in begplt2
!-----------------------------------------------------------------------
use ps_parameters

REAL X(3),Y(3),Z(3),ZC(*)
CHARACTER*19 ALAB

xwidth=15.
xoff=10.
NLABC = 51
ZMAX = ZC(NLAB)
ZMIN = ZC(1)

DO I=1,NLABC-1
  Z1   = (ZMAX-ZMIN)*real(I-1)/real(NLABC-1)+ZMIN
  Z(2) = Z1
  Z3   = (ZMAX-ZMIN)*real(I)/real(NLABC-1)+ZMIN
  Z(3) = Z3
  Z(1) = Z1
  Y(1) = YB+XOFF + real(I-1)/real(NLABC-1)*(YT-YB-2*XOFF) 
  Y(3) = YB+XOFF + real(I)  /real(NLABC-1)*(YT-YB-2*XOFF)
  Y(2) = Y(1)
  X(1) = XR + XOFF
  X(3) = XR + XOFF + XWIDTH
  X(2) = X(3)	
  CALL FILLTRIA(X,Y,Z,ZMIN,ZMAX)
  X(2) = X(1)
  Y(2) = Y(3)
  Z(1) = Z1
  Z(2) = Z3
  Z(3) = Z3	
  CALL FILLTRIA(X,Y,Z,ZMIN,ZMAX)
ENDDO	

WRITE(ppp_unit_ps,*)  '0. 0. 0. setrgbcolor'

DO I=1,NLAB-1
  Z1 = ZC(I)
  Y1 = YB+XOFF + (ZC(I)-ZMIN)/(ZMAX-ZMIN)*(YT-YB-2*XOFF)
  X3 = XR + XOFF + XWIDTH
  WRITE(ALAB,'(1PE9.2)') Z1       
  CALL ps_DLCH(INT(X3+5.),INT(Y1),ALAB,19,-1)
  WRITE(ppp_unit_ps,'(2f8.2,A2)')  X3-xwidth/2.,Y1,' m'
  WRITE(ppp_unit_ps,'(2f8.2,A2)')  X3,Y1,' l'
ENDDO	

WRITE(ALAB,'(1PE9.2)') ZMAX       
CALL DLCH(INT(X3+5.),INT(YT-XOFF),ALAB,19,-1)

WRITE(ppp_unit_ps,'(2f8.2,A2)')  XR+xoff,YT-xoff,' m'
WRITE(ppp_unit_ps,'(2f8.2,A2)')  XR+xoff+xwidth,YT-xoff,' l'
WRITE(ppp_unit_ps,'(2f8.2,A2)')  XR+xoff+xwidth,YB+xoff,' l'
WRITE(ppp_unit_ps,'(2f8.2,A2)')  XR+xoff,YB+xoff,' l'
WRITE(ppp_unit_ps,'(2f8.2,A2)')  XR+xoff,YT-xoff,' l'

return
end


!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!----------- some routines only available in POSTSCRIPT ----------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

SUBROUTINE LINCOL(ICOL)
!***********************************************************************   
! ROUTINE TO CHANGE COLOR IN POST-SCRIPT VERSION ONLY                  *
! (OBSOLETE but kept for compatibility)                                 *
!************************************************************************
use ps_parameters

write(ppp_unit_ps,*) ' stroke'

if (icol.eq.0) write(ppp_unit_ps,*) ' 0.0 0.0 0.0 setrgbcolor'
if (icol.eq.1) write(ppp_unit_ps,*) ' 1.0 0.0 0.0 setrgbcolor'
if (icol.eq.2) write(ppp_unit_ps,*) ' 0.0 1.0 0.0 setrgbcolor'
if (icol.eq.3) write(ppp_unit_ps,*) ' 0.0 0.0 1.0 setrgbcolor'
if (icol.eq.4) write(ppp_unit_ps,*) ' 1.0 1.0 0.0 setrgbcolor'
if (icol.eq.5) write(ppp_unit_ps,*) ' 0.0 1.0 1.0 setrgbcolor'
if (icol.eq.6) write(ppp_unit_ps,*) ' 1.0 0.0 1.0 setrgbcolor'
if (icol.eq.7) write(ppp_unit_ps,*) ' 1.0 0.6 0.0 setrgbcolor'
if (icol.eq.8) write(ppp_unit_ps,*) ' 0.6 0.0 1.0 setrgbcolor'
if (icol.eq.9) write(ppp_unit_ps,*) ' 0.0 1.0 0.6 setrgbcolor'

return
end 


!------------------------------------------------------------------------
!------------------------------------------------------------------------
! 3D extension of ppplib (G. Huysmans, 2008)
!    3D surface plot (tplot3D)
!    3D contour plot  (cplot3D)
!------------------------------------------------------------------------
!------------------------------------------------------------------------

subroutine cplot3D(x,y,z,p,nx,ny,nz,values,nc)
!-------------------------------------------------------------------------
! calculates the contour surface(s) and adds them to the lists of points
! and patches
!
!  reduction of number of pints is incomplete!
!-------------------------------------------------------------------------
use ppp_data
real    :: x(*), y(*), z(*), p(nx,ny,nz), values(*)
integer :: nx,ny,nz,nc
real    :: x8(8),y8(8),z8(8),p8(8), centre(3)
integer,allocatable :: index_sort(:), new_index(:)
real,   allocatable :: x_sort(:)

iv_start = ppp_npoints 
ip_start = ppp_npatches

!------------------- contour all cubes
do i=1,nx-1
  do j=1,ny-1
    do k=1,nz-1
      x8(1) = x(i);   y8(1) = y(j);   z8(1) = z(k);   p8(1) = p(i,j,k)
      x8(2) = x(i+1); y8(2) = y(j);   z8(2) = z(k);   p8(2) = p(i+1,j,k)
      x8(3) = x(i+1); y8(3) = y(j+1); z8(3) = z(k);   p8(3) = p(i+1,j+1,k)
      x8(4) = x(i);   y8(4) = y(j+1); z8(4) = z(k);   p8(4) = p(i,j+1,k)
      x8(5) = x(i);   y8(5) = y(j);   z8(5) = z(k+1); p8(5) = p(i,j,k+1)
      x8(6) = x(i+1); y8(6) = y(j);   z8(6) = z(k+1); p8(6) = p(i+1,j,k+1)
      x8(7) = x(i+1); y8(7) = y(j+1); z8(7) = z(k+1); p8(7) = p(i+1,j+1,k+1)
      x8(8) = x(i);   y8(8) = y(j+1); z8(8) = z(k+1); p8(8) = p(i,j+1,k+1)
    
      call cube_contour(x8,y8,z8,p8,values,nc)
    enddo
  enddo
enddo

! collect identical points and merge into one

np = ppp_npoints - iv_start 

write(*,*) ' npoints, npatches ',ppp_npoints,ppp_npatches

allocate(x_sort(np),index_sort(np))

do i= iv_start+1, ppp_npoints
  x_sort(i-iv_start) = ppp_points(i)%x(1)
enddo

call R_mrgrnk(x_sort,index_sort,np)    ! sort with respect to x value

deallocate(x_sort)
allocate(new_index(np))

tolerance = 1.e-3
tol2 = tolerance**2

iv = index_sort(1)
new_index(iv) = iv + iv_start 

do i=1,np - 1
  
  iv  = index_sort(i)   ; iv_shift  = iv  + iv_start
  iv1 = index_sort(i+1) ; iv1_shift = iv1 + iv_start

  distance = ( (ppp_points(iv_shift)%x(1) - ppp_points(iv1_shift)%x(1))**2  &
           +   (ppp_points(iv_shift)%x(2) - ppp_points(iv1_shift)%x(2))**2  &
           +   (ppp_points(iv_shift)%x(3) - ppp_points(iv1_shift)%x(3))**2 )

  if ( distance .lt. tol2) then
    new_index(iv1) = new_index(iv)        
  else
    new_index(iv1) = iv1_shift
  endif

enddo

do ip = ip_start+1, ppp_npatches
  do k = 1, ppp_patches(ip)%nv
    
    iv = ppp_patches(ip)%vertex(k)

    iv_new = new_index(iv - iv_start)
    ppp_patches(ip)%vertex(k) = iv_new
  enddo
enddo

! remove not used nodes from ppp_points, these are the points that do not point to themselves in new_index

centre = (/ 0., 0., 0. /)

call normal_vector(ip_start+1,ppp_npatches,iv_start+1,ppp_npoints,centre)

deallocate(index_sort,new_index)

np = ppp_npoints - iv_start 

write(*,*) ' npoints, npatches ',ppp_npoints,ppp_npatches

return
end

subroutine cube_contour(x8,y8,z8,p8,values,nc)   
!-------------------------------------------------------------------------
! adds the the contour surface(s) of a single cube and adds them to 
! the lists of points
!-------------------------------------------------------------------------
real :: x8(8),   y8(8),   z8(8),   p8(8), values(*)
real :: x4(4),   y4(4),   z4(4),   p4(4)
real :: x15(15), y15(15), z15(15), p15(15)

integer :: index(4,24)

index(:,1)  = (/ 1, 2, 13, 15 /)
index(:,2)  = (/ 2, 3, 13, 15 /)
index(:,3)  = (/ 3, 4, 13, 15 /)
index(:,4)  = (/ 4, 1, 13, 15 /)
index(:,5)  = (/ 1, 2,  9, 15 /)
index(:,6)  = (/ 2, 6,  9, 15 /)
index(:,7)  = (/ 6, 5,  9, 15 /)
index(:,8)  = (/ 5, 1,  9, 15 /)
index(:,9)  = (/ 2, 3, 10, 15 /)
index(:,10) = (/ 3, 7, 10, 15 /)
index(:,11) = (/ 7, 6, 10, 15 /)
index(:,12) = (/ 6, 2, 10, 15 /)
index(:,13) = (/ 3, 4, 11, 15 /)
index(:,14) = (/ 4, 8, 11, 15 /)
index(:,15) = (/ 8, 7, 11, 15 /)
index(:,16) = (/ 7, 3, 11, 15 /)
index(:,17) = (/ 1, 4, 12, 15 /)
index(:,18) = (/ 4, 8, 12, 15 /)
index(:,19) = (/ 8, 5, 12, 15 /)
index(:,20) = (/ 5, 1, 12, 15 /)
index(:,21) = (/ 5, 6, 14, 15 /)
index(:,22) = (/ 6, 7, 14, 15 /)
index(:,23) = (/ 7, 8, 14, 15 /)
index(:,24) = (/ 8, 5, 14, 15 /)

x15(1:8) = x8(1:8);  y15(1:8) = y8(1:8); z15(1:8) = z8(1:8); p15(1:8) = p8(1:8)

x15( 9) = (x8(1) + x8(2) + x8(5) + x8(6))/4. ; y15(9) = (y8(1) + y8(2) + y8(5) + y8(6))/4.  
z15( 9) = (z8(1) + z8(2) + z8(5) + z8(6))/4. ; p15(9) = (p8(1) + p8(2) + p8(5) + p8(6))/4. 

x15(10) = (x8(2) + x8(3) + x8(6) + x8(7))/4. ; y15(10) = (y8(2) + y8(3) + y8(6) + y8(7))/4.  
z15(10) = (z8(2) + z8(3) + z8(6) + z8(7))/4. ; p15(10) = (p8(2) + p8(3) + p8(6) + p8(7))/4. 

x15(11) = (x8(3) + x8(4) + x8(7) + x8(8))/4. ; y15(11) = (y8(3) + y8(4) + y8(7) + y8(8))/4.  
z15(11) = (z8(3) + z8(4) + z8(7) + z8(8))/4. ; p15(11) = (p8(3) + p8(4) + p8(7) + p8(8))/4. 

x15(12) = (x8(4) + x8(1) + x8(8) + x8(5))/4. ; y15(12) = (y8(4) + y8(1) + y8(8) + y8(5))/4.  
z15(12) = (z8(4) + z8(1) + z8(8) + z8(5))/4. ; p15(12) = (p8(4) + p8(1) + p8(8) + p8(5))/4. 

x15(13) = (x8(1) + x8(2) + x8(3) + x8(4))/4. ; y15(13) = (y8(1) + y8(2) + y8(3) + y8(4))/4.  
z15(13) = (z8(1) + z8(2) + z8(3) + z8(4))/4. ; p15(13) = (p8(1) + p8(2) + p8(3) + p8(4))/4. 

x15(14) = (x8(5) + x8(6) + x8(7) + x8(8))/4. ; y15(14) = (y8(5) + y8(6) + y8(7) + y8(8))/4.  
z15(14) = (z8(5) + z8(6) + z8(7) + z8(8))/4. ; p15(14) = (p8(5) + p8(6) + p8(7) + p8(8))/4. 

x15(15) = (x15(13) + x15(14))/2. ; y15(15) = (y15(13) + y15(14))/2.  
z15(15) = (z15(13) + z15(14))/2. ; p15(15) = (p15(13) + p15(14))/2. 

do ic = 1, 24

  x4(1) = x15(index(1,ic)); x4(2) = x15(index(2,ic)); x4(3) = x15(index(3,ic)); x4(4) = x15(index(4,ic))     
  y4(1) = y15(index(1,ic)); y4(2) = y15(index(2,ic)); y4(3) = y15(index(3,ic)); y4(4) = y15(index(4,ic))     
  z4(1) = z15(index(1,ic)); z4(2) = z15(index(2,ic)); z4(3) = z15(index(3,ic)); z4(4) = z15(index(4,ic))     
  p4(1) = p15(index(1,ic)); p4(2) = p15(index(2,ic)); p4(3) = p15(index(3,ic)); p4(4) = p15(index(4,ic))     

  call pyramid_contour(x4,y4,z4,p4,values,nc)

enddo

return
end

subroutine pyramid_contour(x4,y4,z4,p4,values,nc)   
!-------------------------------------------------------------------------
! adds the (nc) contour surface(s) of a single (4 cornered) pyramid 
! and adds them to the lists of points
!-------------------------------------------------------------------------
use ppp_parameters
use ppp_data
real :: x4(4),y4(4),z4(4),p4(4),values(*)
real :: xc(4), yc(4),zc(4),color(3)
integer  :: index(6,2)

pmax = maxval(p4(1:4)); pmin = minval(p4(1:4))

ip_base = ppp_npatches
iv_base = ppp_npoints

do ic=1,nc

  if ((values(ic) .le. pmax) .and. ( values(ic) .ge. pmin )) then

    index(:,1) = (/ 1,2,3,4,1,2 /)
    index(:,2) = (/ 2,3,4,1,3,4 /)
      
    np = 0
    do iv = 1, 4
      if (p4(iv) .gt. values(ic)) np = np + 1
    enddo

    if ( np .eq. 2 ) then
      if ((p4(1) - values(ic))*(p4(2) - values(ic)) .gt. 0.) then
        index(:,1) = (/ 1,3,2,4,1,3 /)
        index(:,2) = (/ 3,2,4,1,2,4 /)
      elseif ((p4(2) - values(ic))*(p4(3) - values(ic)) .gt. 0.) then
        index(:,1) = (/ 1,2,4,3,1,2 /)
        index(:,2) = (/ 2,4,3,1,4,3 /)
      endif
    endif

    nv = 0

    do i=1, 6

      iv1 = index(i,1)
      iv2 = index(i,2)
      if ((p4(iv1) - values(ic))*(p4(iv2) - values(ic)) .lt. 0.) then

        nv = nv + 1
        xc(nv) = x4(iv1) + (values(ic)-p4(iv1))/(p4(iv2)-p4(iv1)) * (x4(iv2)-x4(iv1))
        yc(nv) = y4(iv1) + (values(ic)-p4(iv1))/(p4(iv2)-p4(iv1)) * (y4(iv2)-y4(iv1))
        zc(nv) = z4(iv1) + (values(ic)-p4(iv1))/(p4(iv2)-p4(iv1)) * (z4(iv2)-z4(iv1))

      endif
    enddo
         
    iv_start = iv_base

    do j=1,nv

      ppp_points(iv_base+1)%x(1) = xc(j)   
      ppp_points(iv_base+1)%x(2) = yc(j)
      ppp_points(iv_base+1)%x(3) = zc(j)
      ppp_points(iv_base+1)%normal = (/ 0., 0., 0. /)

      value = values(ic) / 5.
      call ppp_palette(value,color)
      ppp_points(iv_base+1)%color        = color
      ppp_points(iv_base+1)%transparency = ppp_transparency
      iv_base = iv_base + 1
    enddo
    do j=1,nv
      ppp_patches(ip_base+1)%vertex(j) = iv_start + j
    enddo
    ppp_patches(ip_base+1)%nv = nv

    ip_base = ip_base + 1
  endif
enddo

ppp_npoints  = iv_base
ppp_npatches = ip_base

return
end



subroutine ppp_scale_Z(scale_Z)
!-----------------------------------------------------------------------
! scales the Z_direction values to the same order as the X and Y 
! directions
! the scale is set in ppp_Z_start and ppp_Z_scale
!-----------------------------------------------------------------------
use ppp_parameters
use ppp_data
logical :: scale_Z

x_max  = -1.e20 ; y_max  = -1.e20 ; z_max  = -1.e20
x_min  = +1.e20 ; y_min  = +1.e20 ; z_min  = +1.e20

do i=1,ppp_npatches

  nvi = ppp_patches(i)%nv
  
  do j=1,nvi
  
    iv = ppp_patches(i)%vertex(j)

    Zvalue = ppp_Z_start + ( ppp_points(i)%x(3) - ppp_XY_scale/2. ) / ppp_Z_scale

    x_max = amax1(x_max,ppp_points(iv)%x(1));   x_min = amin1(x_min,ppp_points(iv)%x(1))  ! find the scales after projection
    y_max = amax1(y_max,ppp_points(iv)%x(2));   y_min = amin1(y_min,ppp_points(iv)%x(2))  ! find the scales after projection
    z_max = amax1(z_max,Zvalue);                z_min = amin1(z_min,Zvalue)  ! find the scales after projection

  enddo

enddo

ppp_X_scale  = x_max - x_min
ppp_Y_scale  = y_max - y_min
ppp_XY_scale = max(ppp_X_scale,ppp_Y_scale)

if ( scale_Z) then

  ppp_Z_scale  = 0.75  * ppp_XY_scale / (z_max - z_min)
  ppp_Z_start  = Z_min 
  
  do i=1,ppp_npoints

    ppp_points(i)%x(3)      = - ppp_XY_scale/2. + ( ppp_points(i)%x(3) - ppp_Z_start ) * ppp_Z_scale 
    ppp_points(i)%normal(3) =   ppp_points(i)%normal(3) / ppp_Z_scale 

    scale = ppp_inner_product(ppp_points(i)%normal,ppp_points(i)%normal)
    if ( scale .eq. 0. ) scale = 1.
    ppp_points(i)%normal = ppp_points(i)%normal / sqrt(scale)

  enddo

endif

return
end

subroutine nframe3D(xmin,xmax,ymin,ymax,zmin,zmax)
!-----------------------------------------------------------------------
! subroutine draws adds the 3D frame to the points/patches
!-----------------------------------------------------------------------
use ppp_parameters
use ppp_data
character*7 :: tick_label
character*7 :: axis_label
real        :: line_width

transparency_keep = ppp_transparency

ppp_azimuth = mod(ppp_azimuth,360.)
if (ppp_azimuth .gt. 180.) ppp_azimuth = ppp_azimuth - 360.


XLN = XMIN; XRN = XMAX; nxchar = +7; 
YFN = YMIN; YBN = YMAX; nychar = -7; 
ZBN = ZMIN; ZTN = ZMAX; nzchar = -7; 
if ((ppp_azimuth .gt. -90.) .and. (ppp_azimuth .lt. 0.))  then
  XLN = XMAX; XRN = XMIN; nxchar = -nxchar; nychar=-nychar; nzchar = -nzchar
elseif  (ppp_azimuth .gt. 90.) then
  YFN = YMAX; YBN = YMIN; nxchar = -nxchar; nychar=-nychar; nzchar = -nzchar
elseif  (ppp_azimuth .lt. -90.) then
 XLN = XMAX; XRN = XMIN; 
 YFN = YMAX; YBN = YMIN; 
endif

call ASCL(3,AMIN1(XLN,XRN),AMAX1(XLN,XRN),MAJOR_X,MINOR_X,KF_X)  
call ASCL(3,AMIN1(YFN,YBN),AMAX1(YFN,YBN),MAJOR_Y,MINOR_Y,KF_Y)  
call ASCL(3,ZBN,ZTN,MAJOR_Z,MINOR_Z,KF_Z)  

X_length = 0.05 * (XRN - XLN)
Y_length = 0.05 * (YBN - YFN)
Z_length = 0.05 * (ZTN - ZBN)

!XY_length = (X_length + Y_length)/2.
XY_length = max(X_length,Y_length)

write(*,'(A,6f10.5)') ' nframe3D : ',xmin,xmax,ymin,ymax,zmin,zmax
write(*,'(A,6f10.5)') '            ',xln,xrn,yfn,ybn,zbn,ztn

npieces    = 11
line_width = 0.05 * abs(XY_length)
ntht       = 8
!-------------------------------- axes
do i=1,npieces
  X_start  = XLN + (XRN - XLN) * real(i-1)/real(npieces)
  X_end    = XLN + (XRN - XLN) * real(i  )/real(npieces)  
!  call ppp_line((/ X_start, YBN, ZBN /),(/ X_end,   YBN, ZBN /),ppp_line_color)
  call ppp_cylinder((/ X_start, YBN, ZBN /),(/ X_end,   YBN, ZBN /),line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  X_start  = XLN + (XRN - XLN) * real(i-1)/real(npieces)
  X_end    = XLN + (XRN - XLN) * real(i  )/real(npieces)
  call ppp_cylinder((/ X_start, YFN, ZBN /),(/ X_end,   YFN, ZBN /),line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  X_start  = XLN + (XRN - XLN) * real(i-1)/real(npieces)
  X_end    = XLN + (XRN - XLN) * real(i  )/real(npieces)
  call ppp_cylinder((/ X_start, YBN, ZTN /),(/ X_end,   YBN, ZTN /),line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  X_start  = XLN + (XRN - XLN) * real(i-1)/real(npieces)
  X_end    = XLN + (XRN - XLN) * real(i  )/real(npieces)
  call ppp_cylinder((/ X_start, YFN, ZTN /),(/ X_end,   YFN, ZTN /) ,line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  Y_start  = YBN + (YFN - YBN) * real(i-1)/real(npieces)
  Y_end    = YBN + (YFN - YBN) * real(i  )/real(npieces)
  call ppp_cylinder((/ XLN, Y_start, ZBN /),(/ XLN, Y_end,   ZBN /) ,line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  Y_start  = YBN + (YFN - YBN) * real(i-1)/real(npieces)
  Y_end    = YBN + (YFN - YBN) * real(i  )/real(npieces)
  call ppp_cylinder((/ XRN, Y_start, ZBN /) ,(/ XRN, Y_end,   ZBN /),line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  Y_start  = YBN + (YFN - YBN) * real(i-1)/real(npieces)
  Y_end    = YBN + (YFN - YBN) * real(i  )/real(npieces)
  call ppp_cylinder((/ XLN, Y_start, ZTN /),(/ XLN, Y_end,   ZTN /),line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  Y_start  = YBN + (YFN - YBN) * real(i-1)/real(npieces)
  Y_end    = YBN + (YFN - YBN) * real(i  )/real(npieces)
  call ppp_cylinder((/ XRN, Y_start, ZTN /),(/ XRN, Y_end,   ZTN /),line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  Z_start  = ZBN + (ZTN - ZBN) * real(i-1)/real(npieces)
  Z_end    = ZBN + (ZTN - ZBN) * real(i  )/real(npieces)
  call ppp_cylinder((/ XLN, YFN, Z_start /),(/ XLN, YFN, Z_end /),line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  Z_start  = ZBN + (ZTN - ZBN) * real(i-1)/real(npieces)
  Z_end    = ZBN + (ZTN - ZBN) * real(i  )/real(npieces)
  call ppp_cylinder((/ XRN, YFN, Z_start /),(/ XRN, YFN, Z_end /),line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  Z_start  = ZBN + (ZTN - ZBN) * real(i-1)/real(npieces)
  Z_end    = ZBN + (ZTN - ZBN) * real(i  )/real(npieces)
  call ppp_cylinder((/ XLN, YBN, Z_start /),(/ XLN, YBN, Z_end /),line_width,ppp_line_color,ntht)
enddo

do i=1,npieces
  Z_start  = ZBN + (ZTN - ZBN) * real(i-1)/real(npieces)
  Z_end    = ZBN + (ZTN - ZBN) * real(i  )/real(npieces)
  call ppp_cylinder((/ XRN, YBN, Z_start /),(/ XRN, YBN, Z_end /),line_width,ppp_line_color,ntht)
enddo

!-------------------------------- tickmarks

Z_tick_height = ZBN; if (ppp_elevation .gt. 80.)Z_tick_height = ZTN

do i=1,major_x
  X_tick   = XLN + (XRN - XLN) * real(i-1)/real(major_x-1)

  call ppp_cylinder((/ X_tick, YBN, Z_tick_height /) ,(/ X_tick, YBN - Y_length, Z_tick_height /) ,line_width,ppp_line_color,ntht)
  
  write(tick_label,'(f6.2)') X_tick
  
  if ( ((ppp_azimuth .ge. 0.) .and. (ppp_azimuth .le. 90.)) .or. (ppp_azimuth .lt. -90.) ) then
    call ppp_string((/ X_tick, YBN ,           Z_tick_height - Z_length /), &
                    (/ X_tick, YBN + Y_length, Z_tick_height - Z_length /), tick_label,ppp_text_color,20,nxchar)
  else
    call ppp_string((/ X_tick, YBN + Y_length , Z_tick_height - Z_length /), &
                    (/ X_tick, YBN ,            Z_tick_height - Z_length /), tick_label,ppp_text_color,20,nxchar)
  endif
enddo

do i=1,major_y
  Y_tick   = YFN + (YBN - YFN) * real(i-1)/real(major_y-1)
  
  call ppp_cylinder((/ XLN, Y_tick, Z_tick_height /),(/ XLN + X_length, Y_tick, Z_tick_height /) ,line_width,ppp_line_color,ntht)

  write(tick_label,'(f6.2)') Y_tick
  if ( ((ppp_azimuth .ge. 0.) .and. (ppp_azimuth .le. 90.)) .or. (ppp_azimuth .lt. -90.) ) then
    call ppp_string((/ XLN - X_length, Y_tick, Z_tick_height - Z_length /), &
                    (/ XLN,               Y_tick, Z_tick_height - Z_length /), tick_label,ppp_text_color,20,nychar)
  else
    call ppp_string((/ XLN,               Y_tick, Z_tick_height - Z_length /), &
                    (/ XLN - X_length, Y_tick, Z_tick_height - Z_length /), tick_label,ppp_text_color,20,nychar)
  endif
enddo

do i=1,major_z

  Z_tick   = ZBN + (ZTN - ZBN) * real(i-1)/real(major_z-1)

  call ppp_cylinder((/ XLN, YFN, Z_tick /) ,(/ XLN + X_length, YFN, Z_tick /) ,line_width,ppp_line_color,ntht)
  call ppp_cylinder((/ XLN, YFN, Z_tick /) ,(/ XLN,            YFN+ Y_length, Z_tick /) ,line_width,ppp_line_color,ntht)
 
  write(tick_label,'(f6.2)') Z_tick
  if ( ((ppp_azimuth .ge. 0.) .and. (ppp_azimuth .le. 90.)) .or. (ppp_azimuth .lt. -90.) ) then
    call ppp_string((/ XLN - 1.2*X_length, YFN - 1.2*Y_length, Z_tick /),(/ XLN, YFN, Z_tick /), &
                    tick_label,ppp_text_color,20,nzchar)  
  else
    call ppp_string((/ XLN, YFN, Z_tick /),(/ XLN - 1.2*X_length, YFN - 1.2*Y_length, Z_tick /), &
                    tick_label,ppp_text_color,20,nzchar)  
  endif
enddo

!-------------------------------- axis labels
if ( ((ppp_azimuth .ge. 0.) .and. (ppp_azimuth .le. 90.)) .or. (ppp_azimuth .lt. -90.) ) then
  X_start = (2.*XRN + 3.*XLN) / 5.
  Y_start = (2.*YBN + 3.*YFN) / 5.
  Z_start = (3.*ZBN + 2.*ZTN) / 5.
else
  X_start = (2.*XLN + 3.*XRN) / 5.
  Y_start = (2.*YFN + 3.*YBN) / 5.
  Z_start = (3.*ZBN + 2.*ZTN) / 5.
endif

axis_label = 'X_axis'
if ( ((ppp_azimuth .ge. 0.) .and. (ppp_azimuth .le. 90.)) .or. (ppp_azimuth .lt. -90.) ) then
  call ppp_string((/ X_start, YBN + 3.*Y_length, ZBN - 2.*Z_length /) ,(/ XRN, YBN + 3.*Y_length, ZBN - 2.*Z_length /),& 
                     axis_label,ppp_text_color,24,abs(nxchar)) 
else
  call ppp_string((/ X_start, YBN + 3.*Y_length, ZBN - 2.*Z_length /) ,(/ XLN, YBN + 3.*Y_length, ZBN - 2.*Z_length /),&
                     axis_label,ppp_text_color,24,abs(nxchar)) 
endif

axis_label = 'Y_axis'
if ( ((ppp_azimuth .ge. 0.) .and. (ppp_azimuth .le. 90.)) .or. (ppp_azimuth .lt. -90.) ) then
  call ppp_string((/ XLN - 3.*X_length, Y_start, ZBN - 2.*Z_length /) ,(/ XLN - 3.*X_length, YBN, ZBN - 2.*Z_length /), &
                     axis_label,ppp_text_color,24,abs(nychar)) 
else
  call ppp_string((/ XLN - 3.*X_length, Y_start, ZBN - 2.*Z_length /) ,(/ XLN - 3.*X_length, YFN, ZBN - 2.*Z_length /), &
                     axis_label,ppp_text_color,24,abs(nychar)) 
endif

axis_label = 'Z_axis'
call ppp_string((/ XLN - 3.*X_length, YFN - 3.*Y_length, Z_start /) ,(/ XLN - 3.*X_length, YFN - 3.*Y_length , ZTN /), &
                   axis_label,ppp_text_color,24,abs(nzchar)) 

ppp_transparency = transparency_keep 
!-------------------------------- colorbar

return
end

subroutine ppp_string(x_begin,x_end,text,colors,isize,nchar)
!-----------------------------------------------------------------------
! adds a string of text to the list of points/patches
!-----------------------------------------------------------------------
use ppp_type_definitions
use ppp_parameters
use ppp_data
real          :: x_begin(3),x_end(3),colors(3,*)
CHARACTER*(*) :: text

ppp_points(ppp_npoints+1)%x            = x_begin 
ppp_points(ppp_npoints+1)%color        = colors(1:3,1)
ppp_points(ppp_npoints+1)%transparency = ppp_transparency
ppp_points(ppp_npoints+1)%normal       = (/ 0., 0., 0./)

ppp_points(ppp_npoints+2)%x            = x_end 
ppp_points(ppp_npoints+2)%color        = colors(1:3,1)
ppp_points(ppp_npoints+2)%transparency = ppp_transparency
ppp_points(ppp_npoints+2)%normal       = (/ 0., 0., 0./)

ppp_patches(ppp_npatches + 1)%vertex(1:2) =  (/ ppp_npoints + 1, ppp_npoints + 2 /) 
ppp_patches(ppp_npatches + 1)%nv = 2 
ppp_patches(ppp_npatches+1)%point_label = text
ppp_patches(ppp_npatches+1)%nchar       = nchar
ppp_patches(ppp_npatches+1)%char_size   = isize


ppp_npoints  = ppp_npoints  + 2
ppp_npatches = ppp_npatches + 1

return
end

subroutine ppp_palette(Value,Color)
!-----------------------------------------------------------------------
! given a value between 0. and 1. ppp_palette returns a color
!-----------------------------------------------------------------------
real :: Value, Color(3), Hue, Saturation, Intensity, red, green ,blue

Saturation = 0.9
Intensity  = 0.6
Hue        = (1. - Value) * 360.

call HLS_to_rgb(Hue,Intensity,Saturation,red,green,blue)

Color(1) = red
Color(2) = green
Color(3) = blue

return
end

subroutine tplot3d(xx,yy,zz,nx,ny,ldz)
!-----------------------------------------------------------------------
! surface plot of a regular grid
!-----------------------------------------------------------------------
use ppp_data
use ppp_parameters
real :: xx(*),yy(*),zz(ldz,*)
real :: centre(3), normals(4,3), length, color(3)


centre = (/ 0., 0., -10000. /)

xx_min = minval(xx(1:nx))
xx_max = maxval(xx(1:nx))
yy_min = minval(yy(1:nx))
yy_max = maxval(yy(1:nx))
zz_min = minval(zz(1:nx,1:ny))
zz_max = maxval(zz(1:nx,1:ny))

call nframe3D(xx_min,xx_max,yy_min,yy_max,zz_min,zz_max)

!---------------------------------- construct the list of patches
iv_start = ppp_npoints 
iv_base  = ppp_npoints 

do i = 1, nx
  do j = 1, ny
     ppp_points(iv_base+1)%x(1) = xx(i)   
     ppp_points(iv_base+1)%x(2) = yy(j)
     ppp_points(iv_base+1)%x(3) = zz(i,j) 
     ppp_points(iv_base+1)%normal = (/ 0., 0., 0. /)

     value = ( zz(i,j) - zz_min)/(zz_max - zz_min)
     call ppp_palette(value,color)
     ppp_points(iv_base+1)%color        = color
     ppp_points(iv_base+1)%transparency = ppp_transparency
     iv_base = iv_base + 1
  enddo
enddo

ppp_npoints = iv_base

ip_start = ppp_npatches
ip_base  = ppp_npatches
do i=1, nx-1
  do j=1, ny-1
    ppp_patches(ip_base+1)%vertex(1) = iv_start + (i-1)*ny + j
    ppp_patches(ip_base+1)%vertex(2) = iv_start + (i-1)*ny + j + 1
    ppp_patches(ip_base+1)%vertex(3) = iv_start + (i  )*ny + j + 1
    ppp_patches(ip_base+1)%vertex(4) = iv_start + (i  )*ny + j
    ppp_patches(ip_base+1)%nv = 4
    ip_base = ip_base + 1
  enddo
enddo
ppp_npatches = ip_base

call normal_vector(ip_start+1,ppp_npatches,iv_start+1,ppp_npoints,centre)

write(*,*) ' tplot3D : ',ppp_npatches,ppp_npoints
return
end

subroutine normal_vector(ip_start,ip_end,iv_start,iv_end,centre)
!-----------------------------------------------------------------------
! calculates the average normal at vertices of the patches 
! (rectangles and triangles)
!-----------------------------------------------------------------------
use ppp_type_definitions
use ppp_data
real                :: vec1(3), vec2(3), centre(3), normal(3), length

do iv = iv_start,iv_end
  ppp_points(iv)%normal = 0.
enddo

do ip = ip_start,ip_end

  nvi = ppp_patches(ip)%nv

  if ((nvi .eq. 3) .or. (nvi .eq. 4)) then
     
    do iv=1, nvi

      i1 = mod(iv-1,nvi) + 1
      i2 = mod(iv,nvi)   + 1
      i3 = mod(iv+nvi-2,nvi) + 1

      iv1 = ppp_patches(ip)%vertex(i1)
      iv2 = ppp_patches(ip)%vertex(i2)
      iv3 = ppp_patches(ip)%vertex(i3)

      vec1 = ppp_points(iv2)%x - ppp_points(iv1)%x 
      vec2 = ppp_points(iv1)%x - ppp_points(iv3)%x 
  
      call ppp_cross_product(vec1,vec2,normal)

      if (maxval(normal) .ne. 0.) then

        length = sqrt(ppp_inner_product(normal,normal))  ! normals are normalised

! select normal pointing away from centre

        direction = sign(1.,ppp_inner_product(normal,ppp_points(iv1)%x-centre))

        ppp_points(iv1)%normal = ppp_points(iv1)%normal + direction * normal / length
    
      endif 
    
    enddo

  endif
      
enddo

do iv=iv_start,iv_end
  length =  sqrt(abs(ppp_inner_product(ppp_points(iv)%normal,ppp_points(iv)%normal))) 
  if (length .ne. 0.) then
    ppp_points(iv)%normal =  ppp_points(iv)%normal / length
  endif
enddo  

return
end

subroutine add_normals(vector_scale)
!-----------------------------------------------------------------------
! adds normals to the lists of points and patches
!-----------------------------------------------------------------------
use ppp_data
real :: vector_scale, length

iv_base = ppp_npoints
ip_base = ppp_npatches

do i=1,ppp_npoints
  
  length =  ppp_inner_product(ppp_points(i)%normal,ppp_points(i)%normal)

  if (length .gt. 0.5) then 
    ppp_points(iv_base + 1)%x = ppp_points(i)%x + vector_scale * ppp_points(i)%normal 
    ppp_points(iv_base + 1)%normal = 0. 
  
    ppp_patches(ip_base + 1)%vertex(1) = i 
    ppp_patches(ip_base + 1)%vertex(2) = iv_base + 1
    ppp_patches(ip_base + 1)%nv = 2

    iv_base = iv_base + 1
    ip_base = ip_base + 1
  endif

enddo

ppp_npoints  = iv_base  
ppp_npatches = ip_base 

return
end

function ppp_inner_product(vec1,vec2)
!-----------------------------------------------------------------------
! returns the inner product of two vectors
!-----------------------------------------------------------------------
real  :: vec1(3),vec2(3)
  
ppp_inner_product = vec1(1)*vec2(1) + vec1(2)*vec2(2) + vec1(3)*vec2(3)

return
end

subroutine ppp_cross_product(vec1_in,vec2_in,vec_out)
!-----------------------------------------------------------------------
! returns the cross product of two vectors in vec_out
!-----------------------------------------------------------------------
real  :: vec1_in(3),vec2_in(3),vec_out(3)

vec_out(1) = vec1_in(2)*vec2_in(3) - vec1_in(3)*vec2_in(2)        ! cross-product
vec_out(2) = vec1_in(3)*vec2_in(1) - vec1_in(1)*vec2_in(3)
vec_out(3) = vec1_in(1)*vec2_in(2) - vec1_in(2)*vec2_in(1)

return
end

subroutine ppp_paint_patches
!-----------------------------------------------------------------------
! paints the list of patches, after sorting with respect to their depth
! (classic painting algorithme for hidden line removal)
!-----------------------------------------------------------------------
use ppp_type_definitions
use ppp_parameters
use ppp_data
use SVG_parameters
use ppp_transformations

integer,     allocatable :: index(:)
real,        allocatable :: depth(:)
real                     :: xp(4),yp(4)   ! projected coordinates  
real                     :: xc(3),   a_vector(3),   light_vector(3)
real                     :: spec(3), cam_vector(3), length
real                     :: total_intensity, colors(3,4), H, L, S
character*7              :: text

allocate(depth(ppp_npatches))
allocate(index(ppp_npatches))

write(*,*) ' Painting patches : ',ppp_npatches

call set_camera_transformation(ppp_azimuth,ppp_elevation,ppp_roll,R_camera)

camera(1) = focal_point(1) + camera_distance * sin(ppp_azimuth * 3.1415926535 / 180.) * cos(ppp_elevation * 3.1415926535 / 180.)
camera(2) = focal_point(2) + camera_distance * cos(ppp_azimuth * 3.1415926535 / 180.) * cos(ppp_elevation * 3.1415926535 / 180.)
camera(3) = focal_point(3) + camera_distance * sin(ppp_elevation * 3.1415926535 / 180.)

xp_max = -1.e20; xp_min = 1.e20; yp_max = -1.e20; yp_min = 1.e20

do i=1,ppp_npatches

  nvi = ppp_patches(i)%nv
  
  depth(i) = 0.

  do j=1,nvi
  
    iv = ppp_patches(i)%vertex(j)
    call transform_to_camera(ppp_points(iv)%x,xc)

    depth(i) = depth(i) + xc(2)

    xp_max = amax1(xp_max,xc(1));   xp_min = amin1(xp_min,xc(1))  ! find the scales after projection
    yp_max = amax1(yp_max,xc(3));   yp_min = amin1(yp_min,xc(3))  ! find the scales after projection
 
  enddo

  depth(i) = depth(i) / real(nvi)

enddo


call R_mrgrnk(depth,index,ppp_npatches)    ! sort according to depth to paint deepest squares first


do i=1,ppp_npatches

  is = index(i)
  
!  call normal_vector(patch_list(is))

  nvi = ppp_patches(is)%nv

  if (nvi .eq. 2) then
    nchar      = ppp_patches(is)%nchar
  else
    nchar = 0
  endif

  transparency = 0.

  do j=1, nvi
    
    iv = ppp_patches(is)%vertex(j)

    transparency = transparency + ppp_points(iv)%transparency/real(nvi)

    if (nvi .gt. 2) then   

!---------------------------------------------- the light vector
      a_vector     = light - ppp_points(iv)%x
      length       = sqrt(a_vector(1)*a_vector(1) + a_vector(2)*a_vector(2) + a_vector(3)*a_vector(3))
      light_vector = a_vector / length

!---------------------------------------------- the camera vector  
      a_vector     = camera - ppp_points(iv)%x
      length       = sqrt(a_vector(1)*a_vector(1) + a_vector(2)*a_vector(2) + a_vector(3)*a_vector(3))
      cam_vector   = a_vector / length

!---------------------------------------------- diffuse lighting
      diff_intensity = ppp_points(iv)%normal(1)*light_vector(1) &
                     + ppp_points(iv)%normal(2)*light_vector(2) & 
                     + ppp_points(iv)%normal(3)*light_vector(3)

!---------------------------------------------- specular reflection
      spec =  2. * diff_intensity * ppp_points(iv)%normal - light_vector

      spec_intensity  = amax1(0.,(spec(1)*cam_vector(1) + spec(2)*cam_vector(2) + spec(3)*cam_vector(3))) ** specular_power

      diff_intensity  = amax1(0.,diff_intensity)

      total_intensity = ambient_fraction &
                      + diffusive_fraction * diff_intensity &
                      + specular_fraction  * spec_intensity
  
!--------------------------------------------- keep hue and saturation but adjust intensity with lighting  
      call rgb_to_HLS(ppp_points(iv)%color(1),ppp_points(iv)%color(2),ppp_points(iv)%color(3),H,L,S)

      L =  total_intensity

      call HLS_to_rgb(H,L,S,red,green,blue)
 
      colors(1,j) = red
      colors(2,j) = green
      colors(3,j) = blue
    
    endif

    call transform_to_camera(ppp_points(iv)%x,xc)  
    
    scale = amin1( SVG_width / (xp_max - xp_min),  SVG_height / (yp_max - yp_min) )

    xp(j) = 0.05 * SVG_width  + ( xc(1) - xp_min ) * 0.9*scale
    yp(j) = 0.05 * SVG_height + ( xc(3) - yp_min ) * 0.9*scale

  enddo

  if (nvi .eq. 4 ) then 

    call svg_rectangle(xp,yp,colors,transparency)
    call ps_rectangle(xp,yp,colors)

  elseif (nvi .eq. 3) then

    call svg_triangle(xp,yp,colors,transparency)
    call ps_triangle(xp,yp,colors)

  elseif (nvi .eq. 2) then

    if (nchar .eq. 0) then

      colors(1:3,1) = ppp_line_color
      call ps_line(xp,yp,colors)
      call svg_line(xp,yp,colors,transparency)

    else

      colors(1:3,1) = ppp_text_color
      text          = ppp_patches(is)%point_label
      isize         = ppp_patches(is)%char_size
      call ps_string(xp,yp,colors,text,isize,nchar)
      call svg_string(xp,yp,colors,text,isize,nchar)  

    endif

  endif

enddo

deallocate(depth)
deallocate(index)

return
end


subroutine rotate_X(angle,x_in,x_out)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
use ppp_transformations
real x_in(3), x_out(3)

Pi = 2.*asin(1.)
angle_r = angle * Pi / 180.

x_out(1) =  x_in(1)
x_out(2) =          + x_in(2)*cos(angle_r) + x_in(3)*sin(angle_r)
x_out(3) =          - x_in(2)*sin(angle_r) + x_in(3)*cos(angle_r)
return
end

subroutine rotate_Y(angle,x_in,x_out)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
use ppp_transformations
real x_in(3), x_out(3)

Pi = 2.*asin(1.)
angle_r = angle * Pi / 180.

x_out(1) =   x_in(1)*cos(angle_r)            + x_in(3)*sin(angle_r)
x_out(2) =                     + x_in(2)
x_out(3) = - x_in(1)*sin(angle_r)            + x_in(3)*cos(angle_r)

return
end

subroutine rotate_Z(angle,x_in,x_out)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
use ppp_transformations
real x_in(3), x_out(3)

Pi = 2.*asin(1.)
angle_r = angle * Pi / 180.

x_out(1) =   x_in(1)*cos(angle_r) + x_in(2)*sin(angle_r)
x_out(2) =  -x_in(1)*sin(angle_r) + x_in(2)*cos(angle_r)
x_out(3) =                                               + x_in(3)
return
end



subroutine transform_to_camera(x_in,x_out)
!-----------------------------------------------------------------------
! transforms a coordinate to  the coordinate system aligned with the
! camera, (centered at the focal point not yet)
! no perspective yet
!-----------------------------------------------------------------------
use ppp_transformations
real x_in(3), x_out(3)

x_out = matmul(R_camera,x_in)

return
end



subroutine svg_rectangle(xp,yp,colors,transparency)
!***********************************************************************
!* subroutine to paint a rectangular shape filled with color with a    *
!* specified transparency (this only works in SVG)                     *
!***********************************************************************
use svg_parameters
real         ::  xp(*), yp(*), colors(3,*)
character*16 :: color_string

nv = 4
rav = 0.; gav = 0. ; bav = 0.

do j=1, nv
  rav = rav + colors(1,j)
  gav = gav + colors(2,j)
  bav = bav + colors(3,j)
enddo
rav = rav / real(nv); gav = gav / real(nv);  bav = bav / real(nv)

ired  =  min(max(int(rav * 256.),0),256)
igreen = min(max(int(gav * 256.),0),256)
iblue =  min(max(int(bav * 256.),0),256)

write(color_string,'(A,I3,A,I3,A,I3,A)') 'rgb(',ired,',',igreen,',',iblue,')'

write(color_string,'(A,I3,A,I3,A,I3,A)') 'rgb(',ired,',',igreen,',',iblue,')'

write(ppp_unit_svg,'(A,A,A,F5.2,A,2f9.3,A,2f9.3,A,2f9.3,A,2f9.3,A)') '<path class="style1" stroke="none" fill="',color_string,&
      '" opacity="',1.-transparency,'" d="M ',&
      xp(1),SVG_HEIGHT-yp(1),' L ',xp(2),SVG_HEIGHT-yp(2),' L ',xp(3),SVG_HEIGHT-yp(3),' L',xp(4),SVG_HEIGHT-yp(4),' z"/>'  

return
end

subroutine svg_triangle(xp,yp,colors,transparency)
!***********************************************************************
!* subroutine to paint a rectangular shape filled with color with a    *
!* specified transparency (this only works in SVG)                     *
!***********************************************************************
use svg_parameters
real         ::  xp(*), yp(*), colors(3,*)
character*16 :: color_string

nv = 3
rav = 0.; gav = 0. ; bav = 0.

do j=1, nv
  rav = rav + colors(1,j)
  gav = gav + colors(2,j)
  bav = bav + colors(3,j)
enddo
rav = rav / real(nv); gav = gav / real(nv);  bav = bav / real(nv)

ired  =  min(max(int(rav * 256.),0),256)
igreen = min(max(int(gav * 256.),0),256)
iblue =  min(max(int(bav * 256.),0),256)

write(color_string,'(A,I3,A,I3,A,I3,A)') 'rgb(',ired,',',igreen,',',iblue,')'

write(color_string,'(A,I3,A,I3,A,I3,A)') 'rgb(',ired,',',igreen,',',iblue,')'
write(ppp_unit_svg,'(A,A,A,F5.2,A,2f9.3,A,2f9.3,A,2f9.3,A,2f9.3,A)') '<path class="style1" fill="',color_string,&
      '" opacity="',1.-transparency,'" d="M ',&
      xp(1),SVG_HEIGHT-yp(1),' L ',xp(2),SVG_HEIGHT-yp(2),' L ',xp(3),SVG_HEIGHT-yp(3),' z"/>'  

return
end
                                                                   
subroutine svg_line(xp,yp,colors,transparency)       
!***********************************************************************        
!  writes a line to postscript file                                    *        
!***********************************************************************                                                                                       
use ppp_parameters
use svg_parameters              
real :: xp(*),yp(*),colors(3,*),transparency                                           
      
write(ppp_unit_svg,'(A,2F9.3,A,2F9.3,A)') '<path d="M ',xp(1),SVG_HEIGHT-yp(1),' L ',xp(2),SVG_HEIGHT-yp(2),' z"/>'
                                                              
return                                                                  
end                                                                       

subroutine svg_string(xp,yp,colors,text,isize,nchar)  
!***********************************************************************        
!  writes a line to svg file                                           *        
!***********************************************************************                                                                                       
use ppp_parameters
use svg_parameters              
real          :: xp(*),yp(*),colors(3,*)
character*(*) :: text
logical       :: align_left

angle = 180.*atan2(yp(2)-yp(1),xp(2)-xp(1))/ 3.14159265358979  
angle = mod(angle+360.,180.)

align_left = .true.
if (nchar .lt. 0 ) align_left = .false.

if (angle .gt. 90.) angle =  angle - 180.

!colors(1:3,1) = ppp_text_color(1:3)

!write(ppp_unit_svg,'(3f5.2,A)') colors(1:3,1),' setrgbcolor'
!write(ppp_unit_svg,'(i3,A)') isize,' scaH' 

if (align_left) then
  write(ppp_unit_svg,'(A,f9.3,A,f9.3,A,i3,A,A,A)') '<text x="',xp(1),'" y="',SVG_HEIGHT-yp(1), &
       '" font-family="Arial" font-size="',isize,'">',text,'</text>'
else
  write(ppp_unit_svg,'(A,f9.3,A,f9.3,A,i3,A,A,A)') '<text x="',xp(2),'" y="',SVG_HEIGHT-yp(2), &
       '" font-family="Arial" font-size="',isize,'" text-anchor="end">',text,'</text>'
endif
! <text x="250" y="150" font-family="Verdana" font-size="55" fill="blue" >
  
return                                                                    
end           

subroutine ps_rectangle(xp,yp,colors)
!***********************************************************************
!* subroutine to paint a rectangular shape filled with a color         *
!* at each corner                                                      *
!***********************************************************************
use ps_parameters
real     :: xp(*),yp(*)
real     :: colors(3,*) ! (colors(vertex,rgb)

write(ppp_unit_ps,'(2f9.3,3f6.3,2f9.3,3f6.3,2f9.3,3f6.3,2f9.3,3f6.3,A)')  &
               xp(1),yp(1),colors(1,1),colors(2,1),colors(3,1), &
               xp(2),yp(2),colors(1,2),colors(2,2),colors(3,2), &
               xp(3),yp(3),colors(1,3),colors(2,3),colors(3,3), &
               xp(4),yp(4),colors(1,4),colors(2,4),colors(3,4),' quad'

!write(ppp_unit_ps,*) '0 0 0 setrgbcolor'
!write(ppp_unit_ps,'(8f9.3,A)') xp(1),yp(1),xp(2),yp(2),xp(3),yp(3),xp(4),yp(4),' moveto lineto lineto lineto closepath stroke'
return
end

subroutine ps_triangle(xp,yp,colors)
!-----------------------------------------------------------------------
! write a filled triangle to postscript file using a recursive
! division to interpolate the colors at the corners
!-----------------------------------------------------------------------
use ps_parameters
real    :: xp(*),yp(*)
real    :: colors(3,*)  ! (colors(vertex,rgb)


write(ppp_unit_ps,'(2f10.3,3f6.3,2f10.3,3f6.3,2f10.3,3f6.3,A)')  &
               xp(1),yp(1),colors(1,1),colors(2,1),colors(3,1), &
               xp(2),yp(2),colors(1,2),colors(2,2),colors(3,2), &
               xp(3),yp(3),colors(1,3),colors(2,3),colors(3,3),' nql3 ftr'

return
end

subroutine ps_line(xp,yp,colors)  
!***********************************************************************        
!  writes a line to postscript file                                    *        
!***********************************************************************                                                                                       
use ppp_parameters
use ps_parameters              
real :: xp(*),yp(*),colors(3,*)
                                                      
ps_number_of_lines = ps_number_of_lines + 1                                                         

if ( ps_number_of_lines .ge. 50) then                                                     
!   write(ppp_unit_ps,'(A2)') 'st'                                                 
   ps_number_of_lines = 0                                                             
endif  

!colors(1:3,1) = ppp_line_color(1:3)

write(ppp_unit_ps,'(7F9.3,F5.2,A5)') xp(2),yp(2),xp(1),yp(1),colors(1:3,1),ppp_line_width,' cmls'         
                                                                               
return                                                                    
end                

subroutine ps_string(xp,yp,colors,text,isize,nchar)  
!***********************************************************************        
!  writes a line to postscript file                                    *        
!***********************************************************************                                                                                       
use ppp_parameters
use ps_parameters              
real          :: xp(*),yp(*),colors(3,*)
character*(*) :: text
logical       :: align_left

angle = 180.*atan2(yp(2)-yp(1),xp(2)-xp(1))/ 3.14159265358979  
angle = mod(angle+360.,180.)

align_left = .true.
if (nchar .lt. 0 ) align_left = .false.

if (angle .gt. 90.) angle =  angle - 180.

!colors(1:3,1) = ppp_text_color(1:3)

write(ppp_unit_ps,'(3f5.2,A)') colors(1:3,1),' setrgbcolor'
write(ppp_unit_ps,'(i3,A)') isize,' scaH' 

if (align_left) then
  write(ppp_unit_ps,'(3f9.3,A,A,A)') xp(1), yp(1), angle,' (',text,') sh_left'
else
  write(ppp_unit_ps,'(3f9.3,A,A,A)') xp(2), yp(2), angle,' (',text,') sh_right'
endif
         

return                                                                    
end           

subroutine ppp_line(x_begin,x_end,colors)
!-----------------------------------------------------------------------
! adds a line to the list of points/patches
!-----------------------------------------------------------------------
use ppp_type_definitions
use ppp_parameters
use ppp_data
real :: x_begin(3),x_end(3),colors(3,*)

ppp_points(ppp_npoints+1)%x            = x_begin 
ppp_points(ppp_npoints+1)%color        = colors(1:3,1)
ppp_points(ppp_npoints+1)%transparency = ppp_transparency
ppp_points(ppp_npoints+1)%normal       = (/ 0., 0., 0./)

ppp_points(ppp_npoints+2)%x            = x_end 
ppp_points(ppp_npoints+2)%color        = colors(1:3,1)
ppp_points(ppp_npoints+2)%transparency = ppp_transparency
ppp_points(ppp_npoints+2)%normal       = (/ 0., 0., 0./)

ppp_patches(ppp_npatches + 1)%vertex(1:2) =  (/ ppp_npoints + 1, ppp_npoints + 2 /) 
ppp_patches(ppp_npatches + 1)%nv          = 2 
ppp_patches(ppp_npatches + 1)%nchar       = 0
ppp_npoints  = ppp_npoints  + 2
ppp_npatches = ppp_npatches + 1

return
end

subroutine ppp_sphere(radius,position,color,ntht,nphi)
!-----------------------------------------------------------------------
! produces a sphere to test the specular reflection
! patches (points) are added to the their respective lists
!-----------------------------------------------------------------------
use ppp_type_definitions
use ppp_data
use ppp_parameters
integer     :: ntht
real        :: radius, position(3),color(3)

Pi = 2.*asin(1.)

ip_start = ppp_npatches
ip_base  = ppp_npatches
iv_start = ppp_npoints
iv_base  = ppp_npoints

red   = color(1)
green = color(2)
blue  = color(3)


!-------------------------------- define the points

do j=1,nphi

  phi1 = -Pi / 2. + Pi * real(j-1)/real(nphi-1)

  do i=1,ntht
  
    tht1 = 2.* Pi * real(i-1)/real(ntht-1)

    iv_base = iv_base + 1

    ppp_points(iv_base)%x(1)  = position(1) + radius * cos(tht1) * cos(phi1) 
    ppp_points(iv_base)%x(2)  = position(2) + radius * sin(tht1) * cos(phi1)
    ppp_points(iv_base)%x(3)  = position(3) + radius * sin(phi1)
    
    ppp_points(iv_base)%color(1) = red 
    ppp_points(iv_base)%color(2) = green 
    ppp_points(iv_base)%color(3) = blue
    ppp_points(iv_base)%transparency = ppp_transparency 

    ppp_points(iv_base)%normal(1)  = cos(tht1) * cos(phi1)  
    ppp_points(iv_base)%normal(2)  = sin(tht1) * cos(phi1)
    ppp_points(iv_base)%normal(3)  = sin(phi1) 

  enddo
enddo


do j=1,nphi-1
  
   do i=1,ntht
   
    ip_base = ip_base + 1

    ppp_patches(ip_base)%vertex(1) = iv_start + (j-1) * ntht + i
    ppp_patches(ip_base)%vertex(2) = iv_start + (j-1) * ntht + mod(i,ntht) + 1
    ppp_patches(ip_base)%vertex(3) = iv_start + j    * ntht + mod(i,ntht) + 1
    ppp_patches(ip_base)%vertex(4) = iv_start + j    * ntht + i
    
    ppp_patches(ip_base)%nv = 4

  enddo

enddo

ppp_npoints  = iv_base
ppp_npatches = ip_base

return
end

subroutine ppp_cylinder(X_start,X_end,radius,color,ntht)
!-----------------------------------------------------------------------
! produces a hollow cylinder to test the 3D transformation
!-----------------------------------------------------------------------
use ppp_type_definitions
use ppp_data
use ppp_parameters
integer     :: ntht
real        :: radius, length, position(3),color(3),x_in(3),x_out(3)
real        :: X_start(3), X_end(3)

Pi = 2.*asin(1.)

red   = color(1)
green = color(2)
blue  = color(3)

iv_start = ppp_npoints
ip_start = ppp_npatches
iv_base  = ppp_npoints
ip_base  = ppp_npatches

length = sqrt(ppp_inner_product(X_start-X_end,X_start-X_end))

do i=1,ntht

  iv_base = iv_base + 1
  
  theta = 2. * Pi * real(i-1)/real(ntht)

  x1 = radius * cos(theta) 
  y1 = radius * sin(theta)  
  z1 = 0. ; z2 = length

  xn1 = cos(theta)  
  yn1 = sin(theta) 
  zn1 = 0.         ; zn2 = 0.

  ppp_points(iv_base)%x(1)      = x1;  ppp_points(iv_base)%x(2)      = y1;     ppp_points(iv_base)%x(3)      = z1 
  ppp_points(iv_base)%normal(1) = xn1; ppp_points(iv_base)%normal(2) = yn1;    ppp_points(iv_base)%normal(3) = zn1 
  ppp_points(iv_base)%color(1)  = red; ppp_points(iv_base)%color(2)  = green;  ppp_points(iv_base)%color(3)  = blue 
  ppp_points(iv_base)%transparency = ppp_transparency 

    
  iv_base = iv_base + 1
  
  ppp_points(iv_base)%x(1)      = x1;   ppp_points(iv_base)%x(2)      = y1;     ppp_points(iv_base)%x(3)      = z2 
  ppp_points(iv_base)%normal(1) = xn1;  ppp_points(iv_base)%normal(2) = yn1;    ppp_points(iv_base)%normal(3) = zn2 
  ppp_points(iv_base)%color(1)  = red;  ppp_points(iv_base)%color(2)  = green;  ppp_points(iv_base)%color(3)  = blue 
  ppp_points(iv_base)%transparency = ppp_transparency

enddo

do i=1,ntht
 
  ip_base = ip_base + 1

  ppp_patches(ip_base)%nv = 4

  ppp_patches(ip_base)%vertex(1) = ppp_npoints + 2*(i-1) + 1
  ppp_patches(ip_base)%vertex(2) = ppp_npoints + 2*(i-1) + 2
  ppp_patches(ip_base)%vertex(3) = ppp_npoints + 2*mod(i,ntht) + 2
  ppp_patches(ip_base)%vertex(4) = ppp_npoints + 2*mod(i,ntht) + 1
  
enddo

ppp_npoints = iv_base 
ppp_npatches  = ip_base 

XY_length = sqrt((X_end(1)-X_start(1))**2+(X_end(2)-X_start(2))**2)
!------------------------------------- rotate over X-axis
angle_X = 180./3.14159265358979*atan2(XY_length,X_end(3)-X_start(3))

do i=1,2*ntht
  iv_base = iv_start + i   
  x_in = ppp_points(iv_base)%x
  call rotate_X(angle_X,x_in,x_out)
  ppp_points(iv_base)%x = x_out

  x_in = ppp_points(iv_base)%normal
  call rotate_X(angle_X,x_in,x_out)
  ppp_points(iv_base)%normal = x_out
enddo

!------------------------------------- rotate over Z-axis
angle_Z = 180./3.14159265358979*atan2(X_end(1)-X_start(1),X_end(2)-X_start(2))

do i=1,2*ntht
  iv_base = iv_start + i   
  x_in = ppp_points(iv_base)%x
  call rotate_Z(angle_Z,x_in,x_out)
  ppp_points(iv_base)%x =  X_start + x_out

  x_in = ppp_points(iv_base)%normal
  call rotate_Z(angle_Z,x_in,x_out)
  ppp_points(iv_base)%normal = x_out
enddo

return
end




subroutine ppp_defaults
!-----------------------------------------------------------------------
! subroutine defining the default parameters which can be changed
! the ppplib subroutines by using the module ppp_parameters
!-----------------------------------------------------------------------
use ppp_parameters
use ppp_data

!------------- 2D parameters

ppp_line_color(1:3) = 0.6     ! color of the line in rgb notation
ppp_line_width      = 1.     ! line width
ppp_text_color(1:3) = 0.6     ! color of the line in rgb notation
ppp_transparency    = 0.

font = 'helvetica'                ! the font for the labels

!------------- 3D parameters

ppp_azimuth       = 30.            ! position of the camera with respect to fixed focal point
ppp_elevation     = 30. 
ppp_roll          = 0.  

focal_point(1)          = 0.
focal_point(2)          = 0.
focal_point(3)          = 0.

camera_distance    = 10.
camera_view_angle_horizontal = 30.  
camera_view_angle_vertical   = 30.

camera(1) = focal_point(1) + camera_distance * sin(ppp_azimuth * 3.1415926535 / 180.) * cos(ppp_elevation * 3.1415926535 / 180.)
camera(2) = focal_point(2) + camera_distance * cos(ppp_azimuth * 3.1415926535 / 180.) * cos(ppp_elevation * 3.1415926535 / 180.)
camera(3) = focal_point(3) + camera_distance * sin(ppp_elevation * 3.1415926535 / 180.)

light(1) = -10.; light(2) = 0.; light(3) = 10.

specular_power      = 10.
specular_fraction   = 0.5
diffusive_fraction  = 0.4
ambient_fraction    = 0.3
ppp_finish = 'F_Glass6' ! finish used in POVray output 

ppp_Z_scale  = 1.
ppp_Z_start  = 0.
ppp_X_scale  = 1.
ppp_Y_scale  = 1.
ppp_XY_scale = 1.

ppp_npoints  = 0
ppp_npatches = 0
return
end


subroutine set_camera_transformation(azimuth,elevation,roll,R_camera)
!-----------------------------------------------------------------------
! initialising transformation matrix (fixed camera position)
!   elevation : rotation around x-axis 
!   roll      : rotation around y-axis
!   azimuth   : rotation around z_axis 
!
! order of rotations : [roll]*[elevation]*[azimuth]
!-----------------------------------------------------------------------
real :: R_camera(3,3)

Pi = 2. *asin(1.)

azimuth_r   = azimuth   * Pi / 180
elevation_r = elevation * Pi / 180.
roll_r      = roll      * Pi / 180.

R_camera(1,1) =   cos(azimuth_r)*cos(roll_r)                  + sin(azimuth_r)*sin(elevation_r)*sin(roll_r)
R_camera(2,1) = - sin(azimuth_r)*cos(elevation_r)
R_camera(3,1) =   sin(azimuth_r)*sin(elevation_r)*cos(roll_r) - cos(azimuth_r)*sin(roll_r)

R_camera(1,2) =   sin(azimuth_r)*cos(roll_r)                  - cos(azimuth_r)*sin(elevation_r)*sin(roll_r)
R_camera(2,2) =   cos(azimuth_r)*cos(elevation_r)
R_camera(3,2) = - cos(azimuth_r)*sin(elevation_r)*cos(roll_r) - sin(azimuth_r)*sin(roll_r)

R_camera(1,3) =   cos(elevation_r)*sin(roll_r)
R_camera(2,3) =   sin(elevation_r)
R_camera(3,3) =   cos(elevation_r)*cos(roll_r)

return
end

Subroutine R_mrgrnk(XDONT, IRNGT, NVAL)
! __________________________________________________________
!   MRGRNK = Merge-sort ranking of an array
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! _________________________________________________________
Real     :: XDONT(NVAL)
Integer  :: IRNGT(NVAL)
! __________________________________________________________
Real :: XVALA, XVALB
!
Integer, Dimension (SIZE(IRNGT)) :: JWRKT
Integer :: LMTNA, LMTNC, IRNG1, IRNG2
Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
!      NVAL = Min (SIZE(XDONT), SIZE(IRNGT))

Select Case (NVAL)
  Case (:0)
     Return
  Case (1)
     IRNGT (1) = 1
     Return
  Case Default
     Continue
End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XDONT(IIND-1) <= XDONT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 2) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B. This line may be activated when the
!   initial set is already close to sorted.
!
!          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
!
!  One steps in the C subset, that we build in the final rank array
!
!  Make a copy of the rank array for the merge iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
!
            XVALA = XDONT (JWRKT(IINDA))
            XVALB = XDONT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XDONT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XDONT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
Return
!
End Subroutine R_mrgrnk
     
                               
subroutine rgb2togreyscale(r,g,b,grayscale)
real :: r,g,b,greyscale
  Grayscale = 0.299*r + 0.587*g + 0.114*b
return
end

subroutine rgb2cmy(r,g,b,c,m,y)
real :: r,g,b,c,m,y
c = 1.-r
m = 1.-g
y = 1.-b
return
end

subroutine cmy2rgb(c,m,y,r,g,b)
real :: c,m,y,r,g,b
  r=1.-c
  g=1.-m
  b=1.-y
return
end

subroutine rgb2YCbCr(r,g,b,Y,Cr,Cb)
real :: Y,Cb,Cr,r,g,b
  Y  =  0.29900*r + 0.58700*g + 0.11400*b
  Cb = -0.16874*r - 0.33126*g + 0.50000*b 
  Cr =  0.50000*r - 0.41869*g - 0.08131*b
return
end

subroutine YCbCr2rgb(Y,Cr,Cb,r,g,b)
real :: Y,Cb,Cr,r,g,b
  R = 1.00000*Y + 1.40200*Cr 
  G = 1.00000*Y - 0.34414*Cb - 0.71414*Cr
  B = 1.00000*Y + 1.77200*Cb 
return
end

subroutine rgb2HSI(r,g,b,H,S,I)
real :: r,g,b,H,S,I

  if ((r .eq. g) .and. (r .eq. b) .and. (g .eq. b)) then
    H = 0.
    I = ( r + g + b )/3.
    S = 1. - min(r,min(g,b)) / I
  else
    I = ( r + g + b )/3.
    S = 1. - min(r,min(g,b)) / I
    H = 180. / 3.14159265358979 * acos( (0.5*(r-g) + 0.5*(r-b))/ sqrt((r-g)**2 + (r-b)*(g-b)) )
  
    if (H .lt. 0.) H = H + 360.
    if (b .gt. g)  H = 360. - H
  endif

return
end


subroutine HSI2rgb(H,S,I,r,g,b)
real :: r,g,b,H,S,I
 
  a60 = 1.047197551196597746
  
  if (H .lt. 120.) then
    Hr = H*3.141592653589793238/180.

    b = I * ( 1.- S)
    r = I * ( 1. + S*cos(Hr)/cos(a60 - Hr)) 
    g = 3.*I - ( r + b )

  elseif (H .lt. 240.) then
    
    Hr = (H-120.)*3.141592653589793238/180.
    
    g = I * ( 1. + S*cos(Hr) / cos(a60 - Hr) )
    r = I * ( 1. - S)
    b = 3.*I - ( r + g )

  else
    
    Hr = (H-240.)*3.141592653589793238/180.

    g = I * ( 1. - S)
    b = I * ( 1. + S*cos(Hr) / cos(a60 - Hr) )
    r = 3.*I - ( g + b )

  endif

! put in the correction due to the diffusive light

return
end
subroutine hsv_to_rgb ( h, s, v, r, g, b )
real :: H,S,V,r,g,b!

  if ( s == 0. ) then
    r = v
    g = v
    b = v
  else

    hue = h / 60.

    i = int ( hue )
    f = hue - real ( i )
    p = v * ( 1. - s )
    q = v * ( 1. - s * f )
    t = v * ( 1. - s + s * f )

    if ( i == 0 ) then
      r = v; g = t; b = p
    else if ( i == 1 ) then
      r = q; g = v; b = p
    else if ( i == 2 ) then
      r = p; g = v; b = t
    else if ( i == 3 ) then
      r = p; g = q; b = v
    else if ( i == 4 ) then
      r = t; g = p; b = v
    else if ( i == 5 ) then
      r = v; g = p; b = q
    end if

  end if
  return
end

subroutine rgb_to_hsv ( r, g, b, h, s, v )
real :: r,g,b,H,S,V

  rgbmax = max ( r, g, b )
  rgbmin = min ( r, g, b )

  v = rgbmax

  if ( rgbmax /= 0.0E+00 ) then
    s = ( rgbmax - rgbmin ) / rgbmax
  else
    s = 0.0E+00
  end if
  if ( s == 0.0E+00 ) then
    h = 0.0E+00
  else
    rc = ( rgbmax - r ) / ( rgbmax - rgbmin )
    gc = ( rgbmax - g ) / ( rgbmax - rgbmin )
    bc = ( rgbmax - b ) / ( rgbmax - rgbmin )

    if ( r == rgbmax ) then
      h = bc - gc
    else if ( g == rgbmax ) then
      h = 2.0E+00 + rc - bc
    else
      h = 4.0E+00 + gc - rc
    end if

    h = h * 60.0E+00

  end if
  return
end

subroutine rgb_to_hls ( r, g, b, h, l, s )
!
!*******************************************************************************
!
!! RGB_TO_HLS converts RGB to HLS color coordinates.
!
!
!  Definition:
!
!    The RGB color system describes a color based on the amounts of the 
!    base colors red, green, and blue.  Thus, a particular color
!    has three coordinates, (R,G,B).  Each coordinate must be between
!    0 and 1.  
!
!    The HLS color system describes a color based on the qualities of
!    hue, lightness, and saturation.  A particular color has three 
!    coordinates, (H,L,S).  The L and S coordinates must be between
!    0 and 1, while the H coordinate must be between 0 and 360, and
!    is interpreted as an angle.
!
!  Reference:
!
!    Foley, van Dam, Feiner, and Hughes,
!    Computer Graphics, Principles and Practice,
!    Addison Wesley, Second Edition, 1990.
!
!  Modified:
!
!    29 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real R, G, B, the RGB color coordinates to be converted.
!
!    Output, real H, L, S, the corresponding HLS color coordinates.
!
  implicit none
!
  real b
  real bc
  real g
  real gc
  real h
  real l
  real r
  real rc
  real rgbmax
  real rgbmin
  real r_modp
  real s
!
!  Compute lightness.
!
  rgbmax = max ( r, g, b )
  rgbmin = min ( r, g, b )
  l = ( rgbmax + rgbmin ) / 2.0E+00
!
!  Compute saturation.
!
  if ( rgbmax == rgbmin ) then

    s = 0.0E+00

  else

    if ( l <= 0.5E+00 ) then
      s = ( rgbmax - rgbmin ) / ( rgbmax + rgbmin )
    else
      s = ( rgbmax - rgbmin ) / ( 2.0E+00 - rgbmax - rgbmin )
    end if

  end if
!
!  Compute the hue.
!
  if ( rgbmax == rgbmin ) then

    h = 0.0E+00

  else

    rc = ( rgbmax - r ) / ( rgbmax - rgbmin )
    gc = ( rgbmax - g ) / ( rgbmax - rgbmin )
    bc = ( rgbmax - b ) / ( rgbmax - rgbmin )

    if ( r == rgbmax ) then
      h = bc - gc
    else if ( g == rgbmax ) then
      h = 2.0E+00 + rc - bc
    else
      h = 4.0E+00 + gc - rc
    end if

    h = h * 60.0E+00
!
!  Make sure H lies between 0 and 360.0.
!
    h = r_modp ( h, 360.0E+00 )

  end if

  return
end

subroutine hls_to_rgb ( h, l, s, r, g, b )
!
!*******************************************************************************
!
!! HLS_TO_RGB converts HLS to RGB color coordinates.
!
!
!  Definition:
!
!    The HLS color system describes a color based on the qualities of
!    hue, lightness, and saturation.  A particular color has three 
!    coordinates, (H,L,S).  The L and S coordinates must be between
!    0 and 1, while the H coordinate must be between 0 and 360, and
!    is interpreted as an angle.
!
!    The RGB color system describes a color based on the amounts of the 
!    base colors red, green, and blue.  Thus, a particular color
!    has three coordinates, (R,G,B).  Each coordinate must be between
!    0 and 1.  
!
!  Reference:
!
!    Foley, van Dam, Feiner, and Hughes,
!    Computer Graphics, Principles and Practice,
!    Addison Wesley, Second Edition, 1990.
!
!  Modified:
!
!    29 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real H, L, S, the HLS color coordinates to be converted.
!
!    Output, real R, G, B, the corresponding RGB color coordinates.
!
  implicit none
!
  real b
  real g
  real h
  real hls_value
  real l
  real m1
  real m2
  real r
  real s
!
  if ( l <= 0.5E+00 ) then
    m2 = l + l * s
  else
    m2 = l + s - l * s
  end if

  m1 = 2.0E+00 * l - m2

  if ( s == 0.0E+00 ) then
    r = l
    g = l
    b = l
  else
    r = hls_value ( m1, m2, h + 120.0E+00 )
    g = hls_value ( m1, m2, h )
    b = hls_value ( m1, m2, h - 120.0E+00 )
  end if

  return
end

function hls_value ( n1, n2, h )
!
!*******************************************************************************
!
!! HLS_VALUE is a utility function used by HLS_TO_RGB.
!
!
!  Reference:
!
!    Foley, van Dam, Feiner, and Hughes,
!    Computer Graphics, Principles and Practice,
!    Addison Wesley, Second Edition, 1990.
!
!  Modified:
!
!    29 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real N1, N2, H.
!
!    Output, real HLS_VALUE.
!
  implicit none
!
  real h
  real hls_value
  real hue
  real n1
  real n2
  real r_modp
!
!  Make sure HUE lies between 0 and 360.
!
  hue = r_modp ( h, 360.0E+00 )

  if ( hue < 60.0E+00 ) then
    hls_value = n1 + ( n2 - n1 ) * hue / 60.0E+00
  else if ( hue < 180.0E+00 ) then
    hls_value = n2
  else if ( hue < 240.0E+00 ) then
    hls_value = n1 + ( n2 - n1 ) * ( 240.0E+00 - hue ) / 60.0E+00
  else
    hls_value = n1
  end if

  return
end

function r_modp ( x, y )
!
!*******************************************************************************
!
!! R_MODP returns the nonnegative remainder of real division.
!
!
!  Formula:
!
!    If 
!      REM = R_MODP ( X, Y ) 
!      RMULT = ( X - REM ) / Y
!    then
!      X = Y * RMULT + REM
!    where REM is always nonnegative.
!
!  Comments:
!
!    The MOD function computes a result with the same sign as the
!    quantity being divided.  Thus, suppose you had an angle A,
!    and you wanted to ensure that it was between 0 and 360.
!    Then mod(A,360.0) would do, if A was positive, but if A
!    was negative, your result would be between -360 and 0.
!
!    On the other hand, R_MODP(A,360.0) is between 0 and 360, always.
!
!  Examples:
!
!        I         J     MOD   R_MODP   R_MODP Factorization
! 
!      107        50       7       7    107 =  2 *  50 + 7
!      107       -50       7       7    107 = -2 * -50 + 7
!     -107        50      -7      43   -107 = -3 *  50 + 43
!     -107       -50      -7      43   -107 =  3 * -50 + 43
!
!  Modified:
!
!    29 August 1998
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real X, the number to be divided.
!
!    Input, real Y, the number that divides X.
!
!    Output, real R_MODP, the nonnegative remainder when X is divided by Y.
!
  implicit none
!
  real r_modp
  real x
  real y
!
  if ( y == 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R_MODP - Fatal error!'
    write ( *, '(a,g14.6)' ) '  R_MODP ( X, Y ) called with Y = ', y
    stop
  end if

  r_modp = mod ( x, y )

  if ( r_modp < 0.0E+00 ) then
    r_modp = r_modp + abs ( y )
  end if

  return
end

subroutine ppp_export_to_pov
!-----------------------------------------------------------------------
! exports the 3D scene to a input file for POVRAY
!-----------------------------------------------------------------------
use ppp_type_definitions
use ppp_parameters
use ppp_data
character*7              :: text
real :: xc(3), normal(3)

write(*,*) ' exporting to POV-ray'

!------------------------------------------ set camera position
camera(1) = focal_point(1) - camera_distance * sin(ppp_azimuth * 3.1415926535 / 180.) * cos(ppp_elevation * 3.1415926535 / 180.)
camera(2) = focal_point(2) + camera_distance * cos(ppp_azimuth * 3.1415926535 / 180.) * cos(ppp_elevation * 3.1415926535 / 180.)
camera(3) = focal_point(3) + camera_distance * sin(ppp_elevation * 3.1415926535 / 180.)

!----------------------------------------- write to POV file
ipov = 53
open(ipov,file='plotfile.pov')
write(ipov,*) '#include "colors.inc"'
write(ipov,*) '#include "textures.inc"' 
write(ipov,*) '#include "glass.inc"'
write(ipov,*) '#include "stones.inc"' 
! write(ipov,*) 'plane { <0, 1, 0>, -10. texture { Aluminum } }'
write(ipov,*) 'plane {y, -100   texture {pigment { color red 0. green 0. blue 0.4 }}}'

!write(ipov,*) 'plane { <0, 1, -0.3>, -10. pigment{rgb <0.8, 0.8, 0.9>} finish{ ambient 0.5}}'

write(ipov,*) 'camera {'
write(ipov,'(A,F10.5,A,F10.5,A,F10.5,A)') '  location    <',camera(1),',',camera(3),',',-camera(2),'>'
write(ipov,'(A,F10.5,A,F10.5,A,F10.5,A)') '  look_at     <',focal_point(1),',',focal_point(3),',',-focal_point(2),'>'
write(ipov,*) ' }'

write(ipov,*) 'light_source { '
write(ipov,'(A,f10.5,A,F10.5,A,F10.5,A)') ' <',light(1),',',light(3),',',-light(2),'> rgb <1.0, 1.0, 1.0>*2 shadowless}'

write(ipov,*) 'light_source { '
write(ipov,'(A,f10.5,A,F10.5,A,F10.5,A)') ' <',camera(1),',',camera(3),',',-camera(2),'> rgb <1.0, 1.0, 1.0>*1 }'

write(ipov,*) ' mesh2 {'
write(ipov,*) '  vertex_vectors {'
write(ipov,*) ppp_npoints,','
do i=1,ppp_npoints
  write(ipov,'(A,F10.5,A,F10.5,A,F10.5,A)') '<',ppp_points(i)%x(1),',',ppp_points(i)%x(3),',',-ppp_points(i)%x(2),'>'
enddo
write(ipov,*) ' }'


write(ipov,*) '  normal_vectors {'
write(ipov,*) ppp_npoints,','
do i=1,ppp_npoints
  write(ipov,'(A,F10.5,A,F10.5,A,F10.5,A)') '<',ppp_points(i)%normal(1),',',ppp_points(i)%normal(3),',',-ppp_points(i)%normal(2),'>'
enddo
write(ipov,*) ' }'

write(ipov,*) '  texture_list {'
write(ipov,*) ppp_npoints,','
do i=1,ppp_npoints
  write(ipov,'(A,F10.5,A,F10.5,A,F10.5,A,F10.5,A,A,A,F7.3,A,F7.3,A,F7.3,A,F7.3,A)') &
        'texture{ pigment{color rgbf<',ppp_points(i)%color(1),',',ppp_points(i)%color(2),',',ppp_points(i)%color(3),',', &
	ppp_points(i)%transparency,'>} finish { ',ppp_finish,'  roughness ',1./specular_power,' specular ',specular_fraction, &
        ' diffuse ',diffusive_fraction,' ambient ',ambient_fraction,' }}'

enddo
write(ipov,*) ' }'

write(ipov,*) '  face_indices {'

npatch_total = 0
do i=1,ppp_npatches
  if ( ppp_patches(i)%nv .eq. 4 ) then
    npatch_total = npatch_total + 2
  elseif (ppp_patches(i)%nv .eq. 3 ) then
    npatch_total = npatch_total + 1
  endif    
enddo

write(ipov,*) npatch_total,','
do i=1,ppp_npatches
  if ( ppp_patches(i)%nv .ge. 3 ) then
    write(ipov,'(A,i5,A,i5,A,i5,A,i5,A,i5,A,i5,A)') &
                 '<',ppp_patches(i)%vertex(1)-1,',',ppp_patches(i)%vertex(2)-1,',',ppp_patches(i)%vertex(3)-1,'>,', &
                     ppp_patches(i)%vertex(1)-1,',',ppp_patches(i)%vertex(2)-1,',',ppp_patches(i)%vertex(3)-1,','
    if ( ppp_patches(i)%nv .eq. 4 ) then
    write(ipov,'(A,i5,A,i5,A,i5,A,i5,A,i5,A,i5,A)') &
                 '<',ppp_patches(i)%vertex(1)-1,',',ppp_patches(i)%vertex(3)-1,',',ppp_patches(i)%vertex(4)-1,'>,', &
                     ppp_patches(i)%vertex(1)-1,',',ppp_patches(i)%vertex(3)-1,',',ppp_patches(i)%vertex(4)-1,','
    endif
  endif
enddo

write(ipov,*) '   }'
write(ipov,*) ' }'

do i=1,ppp_npatches
  if ( ppp_patches(i)%nv .eq. 2 ) then

     if (ppp_patches(i)%nchar .eq. 0) then

! draw a line

    else
!text

! rotate text towards camera and along line given by the two vertices!

      iv1     = ppp_patches(i)%vertex(1)
      iv2     = ppp_patches(i)%vertex(2)
      text    = ppp_patches(i)%point_label
      isize   = int(ppp_patches(i)%char_size * ppp_XY_scale)  

      XY_length = sqrt((ppp_points(iv1)%x(1)-ppp_points(iv2)%x(1))**2+(ppp_points(iv1)%x(2)-ppp_points(iv2)%x(2))**2)
!------------------------------------- rotate over X-axis
      angle_Y = 180./3.14159265358979*atan2(ppp_points(iv2)%x(2)-ppp_points(iv1)%x(2),ppp_points(iv2)%x(1)-ppp_points(iv1)%x(1))
!------------------------------------- rotate over Z-axis
      angle_Z = 180./3.14159265358979*atan2(ppp_points(iv2)%x(3)-ppp_points(iv1)%x(3),XY_length)

      xshift = 0.
      if (ppp_patches(i)%nchar .lt. 0 ) xshift = - 5. * float(isize)/500.

      distance = sqrt((camera(2) - ppp_points(iv1)%x(2))**2 + (camera(1) - ppp_points(iv1)%x(1))**2 )
      angle_X = 180./3.14159265358979*atan2(camera(3)-ppp_points(iv1)%x(3),0.5*distance)

!      write(*,*) text
!      write(*,'(6f10.3)') ppp_points(iv1)%x, ppp_points(iv2)%x
!      write(*,'(i6,6f10.3)') ppp_patches(i)%nchar,angle_X,angle_Y, angle_Z

      write(ipov,'(A,A,A)') 'text {ttf "cyrvetic.ttf" "',text,'" .1, 0'
      write(ipov,'(A,3f10.3,A)')  'pigment {rgb <',ppp_text_color,'> }'
      write(ipov,'(A,F10.5,A,F10.5,A,F10.5,A)') 'translate <',xshift,',',0.,',',0.,'>'
      write(ipov,'(A,f10.3)') 'rotate x *',angle_X
      write(ipov,'(A,f10.3)') 'rotate y *',angle_Y 
      write(ipov,'(A,F10.5)') 'scale ',float(isize)/100.
      write(ipov,'(A,f10.3)') 'rotate z *',angle_Z
      write(ipov,'(A,F10.5,A,F10.5,A,F10.5,A)') 'translate <',ppp_points(iv1)%x(1),',',ppp_points(iv1)%x(3), &
                                                ',',-ppp_points(iv1)%x(2),'>'
      write(ipov,*) '}'      
    endif
  endif
enddo


close(ipov)

return
end
