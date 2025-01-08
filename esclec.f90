!module esclec is to save and load parameter files and save and output tooth morphology files

module esclec
use coreop2d
public:: guardaforma,guardaveins,guardapara,llegirforma,llegirveins,llegirpara,llegir
character*50, public :: fifr,fivr,fipr,fifw,fivw,fipw 
integer, public :: nom,map,fora,pass,passs,maptotal,is,maxll
integer, parameter :: mamax=5000
real*8,  public :: mallap(1000,3,mamax)  !atencio si ncels>1000 el sistema peta al llegir
real*8,  public :: parap(32,mamax)
character*50, public :: noms(32)
integer, public :: knotsp(1000,mamax)
integer, public, allocatable :: veip(:,:,:)
real*8, public,allocatable :: ma(:)
real*8, public :: vamax,vamin
character*30, public :: cac,cad,caq,cat,cas,cau,cass
contains
subroutine guardapara
  parap(1,map)=iteraciototal*(iti-1)+temps
  write (2,5)  parap(1:5,map)
  write (2,5)  parap(6:10,map)
  write (2,5)  parap(11:15,map)
  write (2,5)  parap(16:20,map)
  write (2,5)  parap(21:25,map)
  write (2,5)  parap(26:30,map)
  write (2,*)  ncz,ncils
5 format(5F15.6)
end subroutine guardapara

subroutine guardaforma
  write (2,*) temps,ncels
  do i=1,ncels ; write (2,*) malla(i,:) ; end do
end subroutine guardaforma

subroutine guardacon
  write (2,*) ng,ncels
  do i=1,ncels ; do j=1,ncz ; write (2,*) q3d(i,j,:) ; end do ; end do
end subroutine guardacon

subroutine guardaex
  write (2,*) ngg,ncels
  do i=1,ncels ; write (2,*) q2d(i,:) ; end do
end subroutine guardaex

subroutine guardaknots
  write (2,*) temps,ncels
  i=sum(knots)
  write (2,*) i
  do i=1,ncels
    if (knots(i)==1) write (2,*) i
  end do
end subroutine guardaknots

subroutine guardaveins(cvei)
  integer cvei(ncals,nvmax),ccvei(nvmax)
  write (2,*) temps,ncels  
  do i=1,ncels
    k=0 ; ccvei=0
    do j=1,nvmax
      if (cvei(i,j)/=0) then ; k=k+1 ; ccvei(k)=cvei(i,j) ; end if
    end do    
    write (2,*) k
    write (2,*) ccvei(1:k)
  end do
end subroutine guardaveins

subroutine guardamarges(cvei)
  integer cvei(ncals,nvmax)
  real*8 ccmarges(nvmax,8)
  write (2,*) temps,ncels  
  do i=1,ncels
    k=0 ; ccmarges=0
    do j=1,nvmax
      if (cvei(i,j)/=0) then ; k=k+1 ; ccmarges(k,:)=marge(i,j,:) ; end if
    end do    
    write (2,*) k,"cell shape"
    do kk=1,k
      write (2,*) ccmarges(kk,1:3)
    end do
  end do
end subroutine guardamarges

subroutine llegirparatxt
  character*50 cara
  do i=3,32
    read (2,*,END=666,ERR=777)  a,cara ; parap(i,map)=a ; noms(i)=cara !; print *,i,a,cara,"we" 
  end do
5 format(5F13.6)
  Return
889 print *,"fi";return
999 print *,"fi";return
888 print *,"error";return

777 print *,"error de lectura para" ; fora=1 ; close(2) ; return
666 print *,"fi de fitxer para"     ; print *,parap(1:5,map); fora=1 ; close(2) ; return
end subroutine llegirparatxt

subroutine escriuparatxt
  character*50 cara
  do i=3,31
    a=parap(i,map)
    cara=noms(i)
    write (2,*)  a,cara 
  end do
  write (2,*) ncels,"number of cells"
  write (2,*) temps,"number of iterations"
end subroutine escriuparatxt

subroutine llegirpara
print *,"si"
read (*,*)
  read (2,*,END=666,ERR=777)  parap(1:5,map)
  read (2,*,END=666,ERR=777)  parap(6:10,map)
  read (2,*,END=666,ERR=777)  parap(11:15,map)
  read (2,*,END=666,ERR=777)  parap(16:20,map)
  read (2,*,END=666,ERR=777)  parap(21:25,map)
print *,parap
read (*,*)
  read (2,*,END=666,ERR=777)  parap(26:30,map)
  read (2,*,END=666,ERR=777)  parap(31,map)
  read (2,*,END=666,ERR=777)  parap(32,map)
  5 format(5F13.6)
  return
777 print *,"error de lectura para" ; fora=1 ; close(2) ; return
666 print *,"fi de fitxer para"     ; print *,parap(1:5,map); read (*,*) ; fora=1 ; close(2) ; return
end subroutine llegirpara

subroutine llegirforma
  read (2,*,END=666,ERR=777) temps,ncels
  do i=1,ncels ; read (2,*,END=666,ERR=666) malla(i,:) ; end do
  return
777 print *,"error de lectura forma" ; fora=1 ; close(2) ; return
666 print *,"fi de fitxer forma"     ; fora=1 ; close(2) ; return
end subroutine llegirforma

subroutine llegirex
  read (2,*,END=666,ERR=777) temps,ncels
  do i=1,ncels ; read (2,*,END=666,ERR=666) q3d(i,1,:) ; end do
  return
777 print *,"error de lectura ex" ; fora=1 ; close(2) ; return
666 print *,"fi de fitxer ex"     ; fora=1 ; close(2) ; return
end subroutine llegirex

subroutine llegirknots
  read (2,*,END=666,ERR=777) temps,ncels
  read (2,*,END=666,ERR=777) j
  do i=1,j
    read (2,*,END=666,ERR=777) k
    knots(k)=1 
  end do
  return
777 print *,"error de lectura knots" ; fora=1 ; close(2) ; return
666 print *,"fi de fitxer knots"     ; fora=1 ; close(2) ; return
end subroutine llegirknots

subroutine llegirveins
  read (2,*,END=666,ERR=777) temps,ncels  
  vei=0
  do i=1,ncels 
    read (2,*,END=666,ERR=777) k          
    read (2,*,END=666,ERR=777) vei(i,1:k)  
  end do
  return
777 print *,"error de lectura v" ; fora=1 ; close(2) ; return
666 print *,"fi de fitxer v"     ; fora=1 ; close(2) ; return
end subroutine llegirveins



subroutine agafarparap(imap)
integer imap
  parap(1,imap)=temps ; parap(2,imap)=ncels        
  parap(3,imap)=tacre ; parap(4,imap)=tahor ; parap(5,imap)=elas   ; parap(6,imap)=tadi ; parap(7,imap)=crema
  parap(8,imap)=acac  ; parap(9,imap)=ihac  ; parap(10,imap)=acec ; parap(11,imap)=ih  ; parap(12,imap)=acaca
  do j=1,ng  ; parap(12+j,imap)=difq3d(j)    ; end do
  do j=1,ngg ; parap(12+ng+j,imap)=difq2d(j) ; end do
  parap(17,imap)=us ; parap(18,imap)=ud;
  parap(13+ng+ngg,imap)=bip ; parap(14+ng+ngg,imap)=bia ; parap(15+ng+ngg,imap)=bib ;  parap(16+ng+ngg,imap)=bil
  parap(17+ng+ngg,imap)=radi 
  parap(18+ng+ngg,imap)=mu     ; parap(19+ng+ngg,imap)=tazmax
  parap(20+ng+ngg,imap)=radibi ; parap(14+ng+1,imap)=tadif  ;
  parap(15+ng+1,imap)=fac ; parap(21+ng+ngg,imap)=radibii ; parap(12,imap)=ncals
  parap(22+ng+ngg,imap) = ina
  parap(23+ng+ngg,imap) = umgr
end subroutine agafarparap



subroutine posarparap(imap)
integer imap
  temps=parap(1,imap) ; ncels=parap(2,imap)
  tacre=parap(3,imap) ; tahor=parap(4,imap) ; elas=parap(5,imap) ; tadi=parap(6,imap)  ; crema=parap(7,imap)
  acac=parap(8,imap)  ; ihac=parap(9,imap) ; acec=parap(10,imap); ih=parap(11,imap)   ; acaca=parap(12,imap)
  do j=1,ng  ; difq3d(j)=parap(12+j,imap)    ; end do
  do j=1,ngg ; difq2d(j)=parap(12+ng+j,imap) ; end do
  us=parap(17,imap) ; ud=parap(18,imap);
  bip=parap(13+ng+ngg,imap) ; bia=parap(14+ng+ngg,imap) ; bib=parap(15+ng+ngg,imap) ; bil=parap(16+ng+ngg,imap)
  radi=parap(17+ng+ngg,imap); mu=parap(18+ng+ngg,imap)  ; tazmax=parap(19+ng+ngg,imap)
  radibi=parap(20+ng+ngg,imap) ; tadif=parap(14+ng+1,imap) 
  fac=parap(15+ng+1,imap) ; radibii=parap(21+ng+ngg,imap) !!!!!!; ncals=parap(12,imap)
  ina = parap(22+ng+ngg,imap)
  umgr = parap(23+ng+ngg,imap)
end subroutine posarparap



subroutine llegir
  character*50 cac
  if (passs==2) then
    print *,"name of the file (scratch/raw/fill//name) "
    read (*,*) cac
!    open(2,file="/scratch/raw/fillv12.913"//cac,status='old',iostat=i)
    open(2,file=cac,status='old',iostat=i)
    fora=0 ; ki=ncels
    pass=1
    passs=0
    allocate(veip(1000,nvmax,mamax))
  else
    if (pass==0) then
      allocate(veip(1000,nvmax,mamax))
      open(1,file="inex/noms.dad",status='old',iostat=nom)
      fora=0
      pass=1
      ki=ncels
      !print *,nom,"fitxer"
      if (nom/=0) then
        fifw="INmodel/dents.dad"
        fifr="INmodel/dents.dad"
      else
        read(1,*) fifw
        read(1,*) fifr 
      end if
      close(1)
!      open(2,file=fifr,status='old',iostat=nom)
      open(2,file=cac,status='old',iostat=i)
    end if
  end if
  mallap=0
  parap=0
  veip=0.
  knotsp=0 
  mallap=0.
  do map=1,mamax
    ki=ncels
    call llegirparatxt
    call posarparap(map)
    if (fora==1) exit
    if (ncels/=ki) then
      ncals=ncels
      deallocate (malla) ; deallocate(vei) ; deallocate(knots)
      allocate(malla(ncels,3)) ; allocate(vei(ncels,nvmax)) ; allocate(knots(ncels))
      vei=0 ; malla=0 ; knots=0
    end if
    call llegirveins
    veip(1:ncels,:,map)=vei
    if (fora==1) exit
    call llegirknots
    knotsp(1:ncels,map)=knots
    if (fora==1) exit
    call llegirforma
    mallap(1:ncels,:,map)=malla
    if (fora==1) exit
  end do
  maxll=map-1
  map=1
  call posarparap(map)
  deallocate (malla) ;  deallocate(q3d)  ; deallocate(px)    ; deallocate(py)     ; deallocate(pz) 
  deallocate (q2d)   ;  deallocate(marge); deallocate(vei)   ; deallocate(nveins) ; deallocate(hmalla)
  deallocate(hvmalla);  deallocate(knots) ; deallocate(mmaa) ; deallocate (mmap)
  ncals=ncels
  call redime
  knots=knotsp(1:ncels,map)           
  malla=mallap(1:ncels,:,map)
  vei=veip(1:ncels,:,map)
  !determinemt ncils
  ncils=0
  do i=1,ncels
    do j=1,nvmax
      if (vei(i,j)>=ncals) then ; ncils=ncils+1 ; exit ; end if
    end do
  end do
  ncils=ncils+1
  arrow_key_func=0 ![PASSAR]
  pass=1
end subroutine llegir

subroutine llegirinicial
open(2,file=cac,status='old',iostat=i)
map=1
call llegirparatxt
call posarparap(map)
close(2)
end subroutine llegirinicial

subroutine guardaveinsoff(cvei)
  integer cvei(ncals,nvmax),ccvei(nvmax)
  real*8 c(4),mic(4)
    !em de passar de veinatge a faces

  integer ja(ncels*6,4)

  integer face(ncels*20,5)
  integer nfa(ncels*20)
  integer nfaces
  integer pasos(10)
  integer npasos,bi,nop
  real*8 mamx

  integer nfacre

  allocate(ma(ncels))
    call mat

  nfaces=0
  nfacre=0

    do i=1,ncels
ale: do j=1,nvmax
       bi=0
       ii=vei(i,j) ; if (ii==0.or.ii>ncels) cycle
ele:   do k=1,nvmax
         iii=vei(ii,k) ; if (iii==0.or.iii>ncels.or.iii==i) cycle
         do kk=1,nvmax
           iiii=vei(iii,kk) ; if (iiii==0.or.iiii>ncels) cycle
           if (iiii==i) then !triangle trobat
             nfaces=nfaces+1
             !write (2,*) nfaces,i,ii,iii
            ! do jj=1,nfacre
            !   if (ja(jj,1)==i.and.ja(jj,2)==ii.and.ja(jj,3)==iii) goto 7891
            !   if (ja(jj,1)==ii.and.ja(jj,2)==iii.and.ja(jj,3)==i) goto 7891
            !   if (ja(jj,1)==iii.and.ja(jj,2)==ii.and.ja(jj,3)==i) goto 7891
            !   if (ja(jj,1)==iii.and.ja(jj,2)==i.and.ja(jj,3)==ii) goto 7891
            !   if (ja(jj,1)==ii.and.ja(jj,2)==i.and.ja(jj,3)==iii) goto 7891
            !   if (ja(jj,1)==i.and.ja(jj,2)==iii.and.ja(jj,3)==ii) goto 7891
            ! end do
		! nfacre=nfacre+1
            ! ja(nfacre,1)=i
            ! ja(nfacre,2)=ii
            ! ja(nfacre,3)=iii
7891         bi=bi+1
             nop=iii     
             if (bi==1) cycle ele
             cycle ale
           end if
         end do
       end do ele
      
       do k=1,nvmax
         iii=vei(ii,k) ; if (iii==0.or.iii>ncels.or.iii==i.or.iii==nop) cycle
         if (bi==0) cycle
         ! a per els quadrats
         do kk=1,nvmax
           iiii=vei(iii,kk) ; 
           if (iiii==0.or.iiii>ncels.or.iiii==ii.or.iiii==nop) cycle
           do kkk=1,nvmax
             jj=vei(iiii,kkk)
             if (jj==i) then !triangle trobat
               nfaces=nfaces+1
               !write (2,*) nfaces,i,ii,iii,iiii
               cycle ale
             end if
           end do
         end do
       end do

    end do ale
  end do
  
  write (2,*) "COFF"  
  write (2,*) ncels,nfaces,ncels
  write (2,*) " "
  do i=1,ncels ; call get_rainbow(ma(i),vamin,vamax,c) ; write (2,*) malla(i,:),c ; end do

nfaces=0
ja=0
nfacre=0

  write (2,*) " "  
  !write (2,*) "faces"  
    do i=1,ncels
aale: do j=1,nvmax
       bi=0
       ii=vei(i,j) ; if (ii==0.or.ii>ncels) cycle
aele:   do k=1,nvmax
         iii=vei(ii,k) ; if (iii==0.or.iii>ncels.or.iii==i) cycle
         do kk=1,nvmax
           iiii=vei(iii,kk) ; if (iiii==0.or.iiii>ncels) cycle
           if (iiii==i) then !triangle trobat
               nfaces=nfaces+1
           !  do jj=1,nfacre
           !    if (ja(jj,1)==i.and.ja(jj,2)==ii.and.ja(jj,3)==iii) goto 789
           !    if (ja(jj,1)==ii.and.ja(jj,2)==iii.and.ja(jj,3)==i) goto 789
           !    if (ja(jj,1)==iii.and.ja(jj,2)==ii.and.ja(jj,3)==i) goto 789
           !    if (ja(jj,1)==iii.and.ja(jj,2)==i.and.ja(jj,3)==ii) goto 789
           !    if (ja(jj,1)==ii.and.ja(jj,2)==i.and.ja(jj,3)==iii) goto 789
           !    if (ja(jj,1)==i.and.ja(jj,2)==iii.and.ja(jj,3)==ii) goto 789
           !  end do
		! nfacre=nfacre+1
            ! ja(nfacre,1)=i
            ! ja(nfacre,2)=ii
            ! ja(nfacre,3)=iii
               call get_rainbow(ma(i),vamin,vamax,c)
               mic=c ; mamx=ma(i)
               call get_rainbow(ma(ii),vamin,vamax,c)
               mic=mic+c
               !if (ma(ii)>mamx) then ; mic=c ; mamx=ma(ii) ; end if
               call get_rainbow(ma(iii),vamin,vamax,c)
               mic=mic+c
               !if (ma(iii)>mamx) then ; mic=c; end if 
               mic=mic/3.
             write (2,67) 3,i-1,ii-1,iii-1 !,mic
67 format (4I4,4F10.6)
789          bi=bi+1
             nop=iii          
             if (bi==1) cycle aele
             cycle aale
           end if
         end do
       end do aele
      
       do k=1,nvmax
         iii=vei(ii,k) ; if (iii==0.or.iii>ncels.or.iii==i.or.iii==nop) cycle
         if (bi==0) cycle
         ! a per els quadrats
         do kk=1,nvmax
           iiii=vei(iii,kk) ; 
           if (iiii==0.or.iiii>ncels.or.iiii==ii.or.iiii==nop) cycle
           do kkk=1,nvmax
             jj=vei(iiii,kkk)
             if (jj==i) then !triangle trobat
               nfaces=nfaces+1
               call get_rainbow(ma(i),vamin,vamax,c)
               mic=c ; mamx=ma(i)
               call get_rainbow(ma(ii),vamin,vamax,c)
               mic=mic+c 
               !if (ma(ii)>mamx) then ; mic=c ; mamx=ma(ii) ; end if 
               call get_rainbow(ma(iii),vamin,vamax,c)
               mic=mic+c ; 
               !if (ma(iii)>mamx) then ; mic=c ; mamx=ma(iii) ; end if 
               call get_rainbow(ma(iiii),vamin,vamax,c)
               mic=mic+c ; 
               !if (ma(iiii)>mamx) then ; mic=c ; mamx=ma(iiii) ; end if 
               mic=c/4.
               write (2,68) 4,i-1,ii-1,iii-1,iiii-1 !,mic
68 format (5I4,4F10.6)
               cycle aale
             end if
           end do
         end do
       end do

    end do aale
  end do
deallocate(ma)
end subroutine guardaveinsoff


subroutine guardaveinsoff_2(cvei)
  integer cvei(ncals,nvmax),ccvei(nvmax)
  real*8 c(4),mic(4)
    !em de passar de veinatge a faces

  integer ja(ncels*6,4)

  integer face(ncels*20,5)
  integer nfa(ncels*20)
  integer nfaces
  integer pasos(10)
  integer npasos,bi,nop
  real*8 mamx

  integer nfacre

  allocate(ma(ncels))
    call mat

  nfaces=0
  nfacre=0

    do i=1,ncels
ale: do j=1,nvmax
       bi=0
       ii=vei(i,j) ; if (ii==0.or.ii>ncels) cycle
ele:   do k=1,nvmax
         iii=vei(ii,k) ; if (iii==0.or.iii>ncels.or.iii==i) cycle
         do kk=1,nvmax
           iiii=vei(iii,kk) ; if (iiii==0.or.iiii>ncels) cycle
           if (iiii==i) then !triangle trobat
             nfaces=nfaces+1          
7891         bi=bi+1
             nop=iii     
             if (bi==1) cycle ele
             cycle ale
           end if
         end do
       end do ele
      
       do k=1,nvmax
         iii=vei(ii,k) ; if (iii==0.or.iii>ncels.or.iii==i.or.iii==nop) cycle
         if (bi==0) cycle
         ! a per els quadrats
         do kk=1,nvmax
           iiii=vei(iii,kk) ; 
           if (iiii==0.or.iiii>ncels.or.iiii==ii.or.iiii==nop) cycle
           do kkk=1,nvmax
             jj=vei(iiii,kkk)
             if (jj==i) then !triangle trobat
               nfaces=nfaces+1
               !write (2,*) nfaces,i,ii,iii,iiii
               cycle ale
             end if
           end do
         end do
       end do

    end do ale
  end do
  
  write (2,*) "COFF"  
  write (2,*) ncels,nfaces,0
!  write (2,*) " "
!  do i=1,ncels ; call get_rainbow(ma(i),vamin,vamax,c) ; write (2,*) malla(i,:),c ; end do
!  do i=1,ncels ; write (2,*) malla(i,1:3) ; end do
do i=1,ncels ; call get_rainbow_knot(ma(i),vamin,vamax,c,i) ; write (2,*) malla(i,:),c ; end do

nfaces=0
ja=0
nfacre=0

!  write (2,*) " "  
  !write (2,*) "faces"  
    do i=1,ncels
aale: do j=1,nvmax
       bi=0
       ii=vei(i,j) ; if (ii==0.or.ii>ncels) cycle
aele:   do k=1,nvmax
         iii=vei(ii,k) ; if (iii==0.or.iii>ncels.or.iii==i) cycle
         do kk=1,nvmax
           iiii=vei(iii,kk) ; if (iiii==0.or.iiii>ncels) cycle
           if (iiii==i) then !triangle trobat
               nfaces=nfaces+1
           !  do jj=1,nfacre
           !    if (ja(jj,1)==i.and.ja(jj,2)==ii.and.ja(jj,3)==iii) goto 789
           !    if (ja(jj,1)==ii.and.ja(jj,2)==iii.and.ja(jj,3)==i) goto 789
           !    if (ja(jj,1)==iii.and.ja(jj,2)==ii.and.ja(jj,3)==i) goto 789
           !    if (ja(jj,1)==iii.and.ja(jj,2)==i.and.ja(jj,3)==ii) goto 789
           !    if (ja(jj,1)==ii.and.ja(jj,2)==i.and.ja(jj,3)==iii) goto 789
           !    if (ja(jj,1)==i.and.ja(jj,2)==iii.and.ja(jj,3)==ii) goto 789
           !  end do
		! nfacre=nfacre+1
            ! ja(nfacre,1)=i
            ! ja(nfacre,2)=ii
            ! ja(nfacre,3)=iii
               call get_rainbow(ma(i),vamin,vamax,c)
               mic=c ; mamx=ma(i)
               call get_rainbow(ma(ii),vamin,vamax,c)
               mic=mic+c
               !if (ma(ii)>mamx) then ; mic=c ; mamx=ma(ii) ; end if
               call get_rainbow(ma(iii),vamin,vamax,c)
               mic=mic+c
               !if (ma(iii)>mamx) then ; mic=c; end if 
               mic=mic/3.
             write (2,67) 3,i-1,ii-1,iii-1 !,mic
67 format (4I10,4F10.6)
789          bi=bi+1
             nop=iii          
             if (bi==1) cycle aele
             cycle aale
           end if
         end do
       end do aele
      
       do k=1,nvmax
         iii=vei(ii,k) ; if (iii==0.or.iii>ncels.or.iii==i.or.iii==nop) cycle
         if (bi==0) cycle
         ! a per els quadrats
         do kk=1,nvmax
           iiii=vei(iii,kk) ; 
           if (iiii==0.or.iiii>ncels.or.iiii==ii.or.iiii==nop) cycle
           do kkk=1,nvmax
             jj=vei(iiii,kkk)
             if (jj==i) then !triangle trobat
               nfaces=nfaces+1
               call get_rainbow(ma(i),vamin,vamax,c)
               mic=c ; mamx=ma(i)
               call get_rainbow(ma(ii),vamin,vamax,c)
               mic=mic+c 
               !if (ma(ii)>mamx) then ; mic=c ; mamx=ma(ii) ; end if 
               call get_rainbow(ma(iii),vamin,vamax,c)
               mic=mic+c ; 
               !if (ma(iii)>mamx) then ; mic=c ; mamx=ma(iii) ; end if 
               call get_rainbow(ma(iiii),vamin,vamax,c)
               mic=mic+c ; 
               !if (ma(iiii)>mamx) then ; mic=c ; mamx=ma(iiii) ; end if 
               mic=c/4.
               write (2,68) 4,i-1,ii-1,iii-1,iiii-1 !,mic
68 format (5I10,4F10.6)
               cycle aale
             end if
           end do
         end do
       end do

    end do aale
  end do
deallocate(ma)
end subroutine guardaveinsoff_2

subroutine mat
  ma=0
      do i=1,ncels
        if (knots(i)==1) then ; ma(i)=1.0
        else
          if (q2d(i,1)>us) ma(i)=0.1
          if (q2d(i,1)>ud) ma(i)=1.0
        end if
      end do
  vamax=maxval(ma)
  vamin=minval(ma)
end subroutine

subroutine get_rainbow(val,minval,maxval,c)
real*8, intent(in) :: val,maxval,minval
real*8, intent(out) :: c(4)

real*8 :: f

if (maxval > minval) then
   f = (val-minval)/(maxval-minval)
else ! probably maxval==minval
   f = 0.5
endif

if (f < .07) then
   c(1) = 0.6
   c(2) = 0.6
   c(3) = 0.6
   c(4) = 0.8
elseif (f < .2) then
   c(1) = 1.0
   c(2) = f
   c(3) = 0.0
   c(4) = 0.5
elseif (f < 1.0) then
   c(1) = 1.0
   c(2) = f*3
   c(3) = 0.0
   c(4) = 1.0
else
   c(1) = 1.0
   c(2) = 1.0
   c(3) = 0.0
   c(4) = 1.0
endif

end subroutine get_rainbow

subroutine get_rainbow_knot(val,minval,maxval,c,i)
real*8, intent(in) :: val,maxval,minval
real*8, intent(out) :: c(4)

real*8 :: f

if (maxval > minval) then
   f = (val-minval)/(maxval-minval)
else ! probably maxval==minval
   f = 0.5
endif

IF (knots(i) == 1) THEN
   c(1) = 0
   c(2) = 1
   c(3) = 1
   c(4) = 0
ELSE
  if (f < .07) then
   c(1) = 0.6
   c(2) = 0.6
   c(3) = 0.6
   c(4) = 0.8
  elseif (f < .2) then
   c(1) = 1.0
   c(2) = f
   c(3) = 0.0
   c(4) = 0.5
  elseif (f < 1.0) then
   c(1) = 1.0
   c(2) = f*3
   c(3) = 0.0
   c(4) = 1.0
  else
   c(1) = 1.0
   c(2) = 1.0
   c(3) = 0.0
   c(4) = 1.0
  endif
ENDIF

end subroutine get_rainbow_knot

end module esclec
