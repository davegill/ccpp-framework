module wm_example_mod

   use ccpp_kinds, only: kind_phys

   implicit none
   public

   integer            :: ntimes_loop
   !> \section arg_table_wm_example_mod  Argument Table
   !! \htmlinclude arg_table_wm_example_host.html
   !!

   integer , parameter :: ids=1,ide=74,jds=1,jde=61,kds=1,kde=33, & 
                          ims=-4,ime=79,jms=-4,jme=66,kms=1,kme=33, &
                          ips=1,ipe=74,jps=1,jpe=61,kps=1,kpe=33 


   real(kind_phys),dimension(ims:ime,kms:kme,jms:jme)   :: th_phy, qv, qc, qr, rho, pi_phy, z, dz8w
   real(kind_phys),dimension(ims:ime,        jms:jme)   :: RAINNC, RAINNCV
   real(kind_phys)                                      :: dtm ,xlv, cp,EP_2,SVP1,SVP2,SVP3,SVPT0,rhowater


   public :: init_fields
   public :: compare_fields

contains

   subroutine init_fields()
      implicit none   
      integer :: idso,ideo, jdso,jdeo, kdso,kdeo,               & ! domain dims
                 imso,imeo, jmso,jmeo, kmso,kmeo,               & ! memory dims
                 itso,iteo, jtso,jteo, ktso,kteo                 ! tile   dims

   real(kind_phys),dimension(ims:ime,kms:kme,jms:jme)   :: th_phy0, qvo, qco, qro, rhoo, pi_phyo, zo, dz8wo
   real(kind_phys),dimension(ims:ime,        jms:jme)   :: RAINNCo, RAINNCVo
   real(kind_phys)                                      :: dtm0 ,xlvo, cpo,EP_2o,SVP1o,SVP2o,SVP3o,SVPT0o,rhowatero

read (10,*) &
    idso,ideo, jdso,jdeo, kdso,kdeo,               & ! domain dims
    imso,imeo, jmso,jmeo, kmso,kmeo,               & ! memory dims
    itso,iteo, jtso,jteo, ktso,kteo                 ! tile   dims

read (10,*) &
    th_phyo, qvo, qco, qro, rhoo, pi_phyo                  &
   ,dtmo, zo, xlvo, cpo                        &
   ,EP_2o,SVP1o,SVP2o,SVP3o,SVPT0o,rhowatero        &
   ,dz8wo                                     &
   ,RAINNCo, RAINNCVo

th_phy=th_phyo
qv=qvo
qc=qco
qr=qro
rho=rhoo
pi_phy=pi_phyo
dtm=dtmo
z=zo
xlv=xlvo
cp=cpo
EP_2=EP_2o
SVP1=SVP1o
SVP2=SVP2o
SVP3=SVP3o
SVPT0=SVPT0o
rhowater=rhowatero
dz8w=dz8wo
RAINNC=RAINNCo
RAINNCV=RAINNCVo

print *,'itimestep = ',kdso
print *,'t, qv, qc, qr = ',th_phy(1,1,1),qv(1,1,1),qc(1,1,1),qr(1,1,1)

   end subroutine init_fields

   logical function compare_fields()

      integer         :: i,j,k
      integer :: idso,ideo, jdso,jdeo, kdso,kdeo,               & ! domain dims
                 imso,imeo, jmso,jmeo, kmso,kmeo,               & ! memory dims
                 itso,iteo, jtso,jteo, ktso,kteo                 ! tile   dims
      real(kind_phys) :: sum

   real(kind_phys),dimension(ims:ime,kms:kme,jms:jme)   :: th_phy0, qvo, qco, qro, rhoo, pi_phyo, zo, dz8wo
   real(kind_phys),dimension(ims:ime,        jms:jme)   :: RAINNCo, RAINNCVo
   real(kind_phys)                                      :: dtm0 ,xlvo, cpo,EP_2o,SVP1o,SVP2o,SVP3o,SVPT0o,rhowatero

      compare_fields = .true.

read (11,*) &
    idso,ideo, jdso,jdeo, kdso,kdeo,               & ! domain dims
    imso,imeo, jmso,jmeo, kmso,kmeo,               & ! memory dims
    itso,iteo, jtso,jteo, ktso,kteo                 ! tile   dims
read (11,*) &
    th_phyo, qvo, qco, qro, rhoo, pi_phyo                  &
   ,dtmo, zo, xlvo, cpo                        &
   ,EP_2o,SVP1o,SVP2o,SVP3o,SVPT0o,rhowatero        &
   ,dz8wo                                     &
   ,RAINNCo, RAINNCVo

      sum = 0.0
      do j = jps,jpe-1
      do k = kps,kpe-1
      do i = ips,ipe-1
         sum = sum + th_phy(i,k,j)-th_phyo(i,k,j)
         sum = sum + qv(i,k,j)-qvo(i,k,j)
         sum = sum + qc(i,k,j)-qco(i,k,j)
         sum = sum + qr(i,k,j)-qro(i,k,j)
         sum = sum + rho(i,k,j)-rhoo(i,k,j)
         sum = sum + pi_phy(i,k,j)-pi_phyo(i,k,j)
         sum = sum + z(i,k,j)-zo(i,k,j)
         sum = sum + dz8w(i,k,j)-dz8wo(i,k,j)
      end do
      end do
      end do

      do j = jps,jpe-1
      do i = ips,ipe-1
         sum = sum + RAINNC(i,j)-RAINNCo(i,j)
         sum = sum + RAINNCV(i,j)-RAINNCVo(i,j)
      end do
      end do

      sum = sum + dtm-dtmo
      sum = sum + xlv-xlvo
      sum = sum + cp-cpo
      sum = sum + EP_2-EP_2o
      sum = sum + SVP1-SVP1o
      sum = sum + SVP2-SVP2o
      sum = sum + SVP3-SVP3o
      sum = sum + SVPT0-SVPT0o
      sum = sum + rhowater-rhowatero

      if ( sum.ne.0.0) then
        compare_fields = .false.
      endif

   end function compare_fields

end module wm_example_mod
