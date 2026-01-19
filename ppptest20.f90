program ppptest
!-------------------------------------------------
! little program to show some features of PPPLIB
! (in Fortran 90)
!
! PPPLIB : Plasma Physics Plotting Library was created 
! at Los Alamos and further developed at FOM Rijnhuizen. 
!
! It writes Postscript files.
!
! PPPLIB is written in fortran77 and runs on every machine 
! with a f77 compiler.
!
! for more information :
!         Guido Huysmans (guido.huysmans@cea.fr)
!-------------------------------------------------
use ppp_parameters
use ppp_data

implicit none
integer  :: nx, ny, nz, nr, ntht, nc, i, j, k, npatch_max, npoint_max
parameter (nx=51,ny=51,nz=51,nr=51,ntht=32, nc=30)

real*8 :: x(nx), y(ny), z(nz), r(nr), tht(ntht)
real*8 :: zx1(nx), zx2(nx), zxy(nx,ny), zrt(nr,ntht), p3d(nx,ny,nz), zc(nc)
real*8 :: vx(nx,ny), vy(nx,ny), vr(nr,ntht), vtht(nr,ntht)

!--------------------------- some arbitrary data
do i=1,nx
  x(i)   = 0.01 + 0.99 * real(i-1)/real(nx-1)
  zx1(i) = 0.001 + 110.*x(i)**2
  zx2(i) = 1000. *(x(i)-0.5)**2
enddo

!
! Before all, initialise PPPLIB with begplt which
! takes a filename as argument, the extension .svg and/or .ps is added to the filename

call begplt('plotfile')

! to add a label at the bottom
! the arguments are a text and its length
call lblbot('bottom label',12)

! the same at the top
call lbltop(' a label at the top',19)

! to make a simple plot : lplot6
! the first two arguments define the area on the page
! then two arrays and the number of points nx 
! followed by a title. 

call lplot6(1,1,x,zx1,nx,'title')     ! 6 stands for six arguments

! Up to 9 different plots can be put on a page
! (1,1)  is the full page
! (2,1)  is split horizontally in 2, full length vertically, on the left
! (3,1)  as (2,1) but on the right, similarly (1,2) and (1,3) are split vertically
! (2,2)  split both vertically and horizontally, similarly (2,3), (3,2) and (3,3)
! if the plot overlaps with a previous plot a new page is made automatically

call lbltop(' 4 plots on a page ',19)
call lblbot('PPPLIB',6)
call lplot6(2,2,x,zx1,nx,'2,2')
call lplot6(2,3,x,zx1,nx,'2,3')
call lplot6(3,2,x,zx1,nx,'3,2')
call lplot6(3,3,x,zx1,nx,'3,3')

call lbltop(' 9 plots on a page ',19)
call lplot6(4,4,x,zx1,nx,'4,4')
call lplot6(4,5,x,zx1,nx,'4,5')
call lplot6(4,6,x,zx1,nx,'4,6')
call lplot6(5,4,x,zx1,nx,'5,4')
call lplot6(5,5,x,zx1,nx,'5,5')
call lplot6(5,6,x,zx1,nx,'5,6')
call lplot6(6,4,x,zx1,nx,'6,4')
call lplot6(6,5,x,zx1,nx,'6,5')
call lplot6(6,6,x,zx1,nx,'6,6')

! lplot6 is the simplest way to make a plot, more control over the options
! is given by the routine lplot with 13 parameters
call lbltop('Log scales',10)
call lplot(2,2,1,x,zx1,nx,1,'the title',9,'label x',7,'y label',7)
call lplot(2,3,2,x,zx1,nx,1,'a Log-plot',10,'label x',7,'y label',7)
call lplot(3,2,3,x,zx1,nx,1,'another Log-plot',16,'label x',7,'y label',7)
call lplot(3,3,4,x,zx1,nx,1,'a Log-Log-plot',14,'label x',7,'y label',7)

call lbltop('more options',12)
call lplot(2,2,11201,x,zx1,nx,1,'adding symbols',14,'label x',7,'y label',7)
call lplot(2,3,-1,x,zx1,nx,1,'only the frame',15,'label x',7,'y label',7)
call lplot(3,2,-1,x,zx1,nx,1,'even color!',15,'label x',7,'y label',7)

call ppp_set_color(1)           ! this changes the color of everything written afterwards
call lplot6(3,2,x,zx1,-nx,' ')  ! the negative number of points implies using a previously drawn frame
call ppp_set_color(0)           ! this restores the color to black

call lplot(3,3,-1,x,zx1,nx,1,'multiple curves',15,'label x',7,'y label',7)
call ppp_set_color(1)           ! this changes the color of everything written afterwards
call lplot6(3,3,x,zx1,-nx,' ')  ! the negative number of points implies using a previously drawn frame

call ppp_set_linewidth(4.)      ! changes the linewidth
call ppp_set_color(2)           ! this changes the color of everything written afterwards

call lplot6(3,3,x,zx2,-nx,' ')    ! the negative number of points implies using a previously drawn frame

call ppp_set_linewidth(1.)      ! changes the linewidth
call ppp_set_color(0)           ! this restores the color to black

call adv(1)                     ! goes to the next page

call dlch(200,200,'One can write text anywhere on the page',39,6)
call dlcv(100,100,'Also vertically and smaller size',32,4)
!call dlch(200,400,'Greek symbols $a$ $b$ $g$ etc.',-24,2)


do i=1,nx
  x(i) = -3.+6.*real(i-1)/real(nx-1)
enddo
do i=1,ny
  y(i) = -3.+6.*real(i-1)/real(ny-1)
enddo

do i=1,nx
  do j=1,ny
    zxy(i,j) = exp( cos(x(i)**2 + y(j)**2) / (1. + x(i)**2))
  enddo
enddo


! contour plots with some options :
call lbltop('Contour plots',13)

! the standard contour plot, negative number of contours (-10) determines its own values
call cplot(2,2,1,x,y,nx,ny,1,1,zxy,nx,zc,-10,'contour plot',12,'x',1,'y',1)

! a color contour plot with colorbar
zc(1) = 0.
zc(2) = 0.5
zc(3) = 1.0
zc(4) = 1.5
zc(5) = 2.0
zc(6) = 2.5

call cplot(3,2,-1,x,y,nx,ny,1,1,zxy,nx,zc,6,'color contour with colorbar',27,'x',1,'y',1)

call ppp_set_color(0)
call cplot(2,3,-2,x,y,nx,ny,1,1,zxy,nx,zc,6,'no colorbar',11,'x',1,'y',1)

call ppp_set_color(0)
call cplot(3,3,0,x,y,nx,ny,1,1,zxy,nx,zc,6,'no labels',9,'x',1,'y',1)


do i=1,nx
  do j=1,ny
    vx(i,j) =  cos(x(i))
    vy(i,j) =  sin(y(j))
  enddo
enddo

!more examples
call hplot6(2,2,x,zx1,nx,'a histogram')

call vplot(3,2,11,x,y,nx,ny,2,2,vx,vy,nx,.5,30,'a vector plot',13,'x',1,'y',1)  


do i=1,nr
  r(i) = float(i-1)/float(nr-1)
enddo
do j=1,ntht
  tht(j) = 6.283 * float(j-1)/float(ntht-1)
enddo
do i=1,nr
  do j=1,ntht
    zrt(i,j)  = exp(cos(r(i)**2) / (1. + (r(i)*cos(tht(j)))**2))
    vr(i,j)   = r(i) * (1.-r(i))
    vtht(i,j) = r(i) * cos(tht(j))
  enddo
enddo

call vplotx(2,3,11,r,tht,nr,ntht,2,2,vr,vtht,nr,1.,30,'vector plot cylindrical coord',33,'x',1,'y',1,1.,4)   

call cplotx(3,3,0,r,tht,nr,ntht,1,1,zrt,nr,zc,6,'contour plot cylindrical coord',33,'x',1,'y',1,1.,4,0)


! surface plot (experimental PPPLIB extension)
do i=1,nx
  x(i) = -3. + 6.*float(i-1)/float(nx-1)
enddo
do j=1,ny
  y(j) = -3. + 6.*float(j-1)/float(ny-1)
enddo 
do k=1,nz
  z(k) = 3.*float(k-1)/float(nz-1)
enddo 

do i=1,nx
  do j=1,ny

    zxy(i,j)   = exp( cos(x(i)**2 + y(j)**2) / (1. + x(i)**2))

    do k=1,nz

      p3D(i,j,k)   = exp( cos(x(i)**2 + y(j)**2) / (1. + x(i)**2)) * sin(z(k))

    enddo
  enddo
enddo


npatch_max  = 100000
npoint_max  = 100000
allocate(ppp_patches(npatch_max))
allocate(ppp_points(npoint_max))

call adv(1)
call ppp_defaults
call tplot3D(x,y,zxy,nx,ny,nx)
!call ppp_scale_Z(.true.)
call ppp_paint_patches

ppp_azimuth       = 120.            ! position of the camera with respect to fixed focal point

!call ppp_export_to_pov

! the last call should always be to finplt
call finplt

end
