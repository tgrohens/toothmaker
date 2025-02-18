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
real*8    parapo(32)
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

paras(1)=3 ; paras(2)=4 ;paras(3)=5 ;paras(4)=7 ;paras(5)=8 ;
paras(6)=9 ; paras(7)=11 ;paras(8)=13 ;paras(9)=14 ;paras(10)=15 ;
paras(11)=17 ; paras(12)=18 ;paras(13)=19 ;paras(14)=20 ;paras(15)=21 ;
paras(16)=23 ; paras(17)=27 ;paras(18)=28 ;paras(19)=29 

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

!parapo=parap(:,1)
!Programa
!parap(:,1)=parapo
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
!end do
!end do

parap(:,1)=parapo
call posarparap(1)

!end do
!end do

777 print *,"fora"
end program tresdac

!***************************************************************************
!***************  FI  PROGRAMA      ****************************************
!***************************************************************************
