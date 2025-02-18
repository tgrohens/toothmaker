!***************************************************************************
!***************  PROGRAMA           ****************************************
!***************************************************************************

program tresdac

!useopengl_gl
!useopengl_glut
!use view_modifier
!use function_plotter
use coreop2d
use esclec
implicit none

integer :: winid, menuid
integer pa,nstep
real*8    step
integer   sstep,nt,pao
character*12 cu,cd,ct,cq
character*12 acu,acd,act,acq
character*1 ccu,ccd,cct,ccq
character*2 dcu,dcd,dct
character*6 dcq
character*31 nff
character*31 nf,nfoff,nfes
character*4 chq
character*6 chs
character*8 chv
character*2 di
character*1 diu
integer paras(19)
integer sis,siss
integer ancels

integer idi,pau,pad,pauu,padd

call getarg(1,cac) !fitxer entrada  !input file
call getarg(2,cau) !fitxer sortida  !output file
call getarg(3,cad) !iteracions      ! number iterations
!iteraciototal=dnum(cad)
!call getarg(4,caq) !parametre a mutar
!call getarg(5,cat) !tan per cert a canviar en cada pas
!call getarg(6,cas) !tan per cert a canviar en cada pas
!sstep=dnum(cas)
call getarg(4,cass) !numero de steps  !repetion of the iterations, it safe a file at this step, so an example input of fileInput fileOutpt 1000 10 will run from 1000x10 iterations, saving an putput file every 1000 iterations
!step=dnum(cat)
!pa=dnum(caq)
!nstep=dnum(cas)
!print *,cac,cau,cad,cass

!nonsense to pass char to int ...
!open(4,file="kk")
!write (4,*) cad,cass
!print *,cad,cass
!close(4)
!open(4,file="kk")
!read (4,*) iteraciototal,sstep
!print *,iteraciototal,sstep
close (4)

READ(cad,*)iteraciototal
READ(cass,*)sstep

if (sstep<0) then ; siss=-1 ; else ;siss=1 ;end if

ncz=4
call ciinicial
call llegirinicial
call dime

!Programa
  ancels=ncels
  call posarparap(1)
  ncels=ancels
  
  call initact  ! Sets initial activation concentration (Ina).
  
  do iti=1,sstep 
    ii=0
    write (ct,*) (idi+ii)*sis
    write (cq,*) iti*iteraciototal
 
    acu=adjustl(cu)
    acd=adjustl(cd)
    act=adjustl(ct)
    acq=adjustl(cq)

    dcu=acu ; dcd=acd ;dct=act ;dcq=acq

    nff=dcq//"_"//cau

    do i=1,len(nff)
      if (nff(i:i)==" ") nff(i:i)="_"
    end do

    nfpro=cau
    do i=1,len(nfpro)
      if (nfpro(i:i)==" ") nfpro(i:i)="_"
    end do


    nf=nff(1:26)//"_"//".dad"
    nfoff=nff(1:26)//"_"//".off"
    nfes=nff(1:26)//"_"//".txt"
    nfpro=nfpro(1:15)//"_progressbar"//".txt"

    temps=0

    nt=iteraciototal !*iti
    call iteracio(nt)

    di=trim(adjustl(dcu))

    !writing
    open(2,file=nf,iostat=nom)
    call agafarparap(1)
    call guardapara
    call guardaveins(vei)
    call guardamarges(vei)
    call guardaknots
    call guardaforma
    call guardacon
    call guardaex
    close(2)

    open(2,file=nfoff,iostat=nom)
    call guardaveinsoff_2(vei)
    close(2)

    open(2,file=nfes,iostat=nom)
    call posarparap(1)
    call escriuparatxt
  end do


777 print *,"fora"
end program tresdac

!***************************************************************************
!***************  FI  PROGRAMA      ****************************************
!***************************************************************************
