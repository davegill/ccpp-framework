!=================================================================================================================
!bl_ysu.F was modified to accomodate both the WRF and MPAS models / 2018-12-7 

!modifications to sourcecode for future implementation using the CPF:
! * in subroutine bl_ysu_run, split the array qx containing the water vapor, cloud water, and cloud ice (if available)
!   mixing ratios into 3 separate arrays (qvx, qcx, and qix if available). similarly, split the array qntp into 3
!   separate arrays for the tendency of water vapor, cloud water, and cloud ice (if available) due to vertical
!   mixing. now do the vertical mixing for water vapor, cloud water, and ice separately.
!   Laura D. Fowler (laura@ucar.edu) / 2019-07-25.
! * in subroutine ysu, added and initialize the local arrays qmix and rqmixblten to include the vertical mixing
!   of number concentrations, aerosols, chemical species and/or passive tracers for use in subroutine bl_ysu_run. for
!   now these arrays are simple place holders.
!   Laura D. Fowler (laura@ucar.edu) / 2019-07-25.
! * in subroutine bl_ysu_run, cleaned up the calculation of the k-coefficients for water vapor,cloud water,cloud ice,
!   and chemical species/passive tracers. Because all water species,and chemical tracers/passive tracers share
!   the same k-coefficients,many of the input arrays (with the exception of r1 and f1) do not need to be computed
!   over and over. this required to clean up the subroutine tridin_ysu so that some of those arrays (like au) are
!   not overwritten.
!   Laura D. Fowler (laura@ucar.edu) / 2019-08-02.
! * in subroutine bl_ysu_run, added the initialization of local and output k-coefficients.
!   Laura D. Fowler (laura@ucar.edu) / 2019-08-02.
! * in subroutine bl_ysu_run, modified the call to subroutine get_pblh. on cheyenne with the -g option, intel compilers
!   create temporary arrays that lead MPAS to run extremely slow. This is remedied by first copying a few of the
!   targeted arrays to dummy kts:kte arrays prior to the call to get_pblh.
!   Laura D. Fowler (laura@ucar.edu) / 2019-08-03.

!=================================================================================================================
!WRF:model_layer:physics
!
!
!
!
!
!
!
module bl_ysu
use mpas_log
contains
!
!
!-------------------------------------------------------------------------------
!
   subroutine ysu(u3d,v3d,th3d,t3d,qv3d,qc3d,qi3d,p3d,p3di,pi3d,               &
                  rublten,rvblten,rthblten,                                    &
                  rqvblten,rqcblten,rqiblten,flag_qc,flag_qi,                  &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w,psfc,                                                   &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
                  dt,kpbl2d,                                                   &
                  exch_h,exch_m,                                               &
                  wstar,delta,                                                 &
                  u10,v10,                                                     &
                  uoce,voce,                                                   &
                  rthraten,ysu_topdown_pblmix,                                 &
                  ctopo,ctopo2,                                                &
                  ids,ide, jds,jde, kds,kde,                                   &
                  ims,ime, jms,jme, kms,kme,                                   &
                  its,ite, jts,jte, kts,kte,                                   &
                  errmsg,errflg                                                &
                 )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!-- u3d         3d u-velocity interpolated to theta points (m/s)
!-- v3d         3d v-velocity interpolated to theta points (m/s)
!-- th3d        3d potential temperature (k)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- qc3d        3d cloud mixing ratio (kg/kg)
!-- qi3d        3d ice mixing ratio (kg/kg)
!               (note: if P_QI<PARAM_FIRST_SCALAR this should be zero filled)
!-- p3d         3d pressure (pa)
!-- p3di        3d pressure (pa) at interface level
!-- pi3d        3d exner function (dimensionless)
!-- rr3d        3d dry air density (kg/m^3)
!-- rublten     u tendency due to
!               pbl parameterization (m/s/s)
!-- rvblten     v tendency due to
!               pbl parameterization (m/s/s)
!-- rthblten    theta tendency due to
!               pbl parameterization (K/s)
!-- rqvblten    qv tendency due to
!               pbl parameterization (kg/kg/s)
!-- rqcblten    qc tendency due to
!               pbl parameterization (kg/kg/s)
!-- rqiblten    qi tendency due to
!               pbl parameterization (kg/kg/s)
!-- cp          heat capacity at constant pressure for dry air (j/kg/k)
!-- g           acceleration due to gravity (m/s^2)
!-- rovcp       r/cp
!-- rd          gas constant for dry air (j/kg/k)
!-- rovg        r/g
!-- dz8w        dz between full levels (m)
!-- xlv         latent heat of vaporization (j/kg)
!-- rv          gas constant for water vapor (j/kg/k)
!-- psfc        pressure at the surface (pa)
!-- znt         roughness length (m)
!-- ust         u* in similarity theory (m/s)
!-- hpbl        pbl height (m)
!-- psim        similarity stability function for momentum
!-- psih        similarity stability function for heat
!-- xland       land mask (1 for land, 2 for water)
!-- hfx         upward heat flux at the surface (w/m^2)
!-- qfx         upward moisture flux at the surface (kg/m^2/s)
!-- wspd        wind speed at lowest model level (m/s)
!-- u10         u-wind speed at 10 m (m/s)
!-- v10         v-wind speed at 10 m (m/s)
!-- uoce        sea surface zonal currents (m s-1)
!-- voce        sea surface meridional currents (m s-1)
!-- br          bulk richardson number in surface layer
!-- dt          time step (s)
!-- rvovrd      r_v divided by r_d (dimensionless)
!-- ep1         constant for virtual temperature (r_v/r_d - 1)
!-- ep2         constant for specific humidity calculation
!-- karman      von karman constant
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!-------------------------------------------------------------------------------
!
!
   integer,  intent(in   )   ::      ids,ide, jds,jde, kds,kde,                &
                                     ims,ime, jms,jme, kms,kme,                &
                                     its,ite, jts,jte, kts,kte

   integer,  intent(in)      ::      ysu_topdown_pblmix
!
   real,     intent(in   )   ::      dt,cp,g,rovcp,rovg,rd,xlv,rv
!
   real,     intent(in )     ::      ep1,ep2,karman
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          qv3d, &
                                                                         qc3d, &
                                                                         qi3d, &
                                                                          p3d, &
                                                                         pi3d, &
                                                                         th3d, &
                                                                          t3d, &
                                                                         dz8w, &
                                                                     rthraten
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                          p3di
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(out  )   ::                                       rublten, &
                                                                      rvblten, &
                                                                     rthblten, &
                                                                     rqvblten, &
                                                                     rqcblten, &
                                                                     rqiblten
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(out  )   ::                                        exch_h, &
                                                                       exch_m
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(out  )   ::                                         wstar
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(out  )   ::                                         delta
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(inout)   ::                                           u10, &
                                                                          v10
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                          uoce, &
                                                                         voce
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx, &
                                                                           br, &
                                                                         psfc
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                                &
                                                                         psim, &
                                                                         psih
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(in   )   ::                                           znt, &
                                                                          ust, &
                                                                          wspd
   real,     dimension( ims:ime, jms:jme )                                   , &
             intent(out  )   ::                                          hpbl
!
   real,     dimension( ims:ime, kms:kme, jms:jme )                          , &
             intent(in   )   ::                                           u3d, &
                                                                          v3d
!
   integer,  dimension( ims:ime, jms:jme )                                   , &
             intent(out  )   ::                                        kpbl2d
!
   logical,  intent(in)      ::                                       flag_qc, &
                                                                      flag_qi
!
   real,     dimension( ims:ime, jms:jme )                                   , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2
!
   character(len=*), intent(out)   ::                                  errmsg
   integer,          intent(out)   ::                                  errflg
!local
   integer ::  i,j,k

!temporary allocation of local chemical species and/or passive tracers that are vertically-
!mixed in subroutine bl_ysu_run:
   integer,  parameter :: nmix = 5
   integer :: n
   real,     dimension ( ims:ime, kms:kme, jms:jme, nmix) ::             qmix
   real,     dimension ( ims:ime, kms:kme, jms:jme, nmix) ::       rqmixblten

   !  Local tile-sized arrays for contiguous data for bl_ysu_run call.
      
   real,                   dimension(its:ite,kts:kte,nmix) ::              &
                                                             qmix_hv     , &
                                                             rqmixblten_hv
      
   real,                   dimension(its:ite,kts:kte)      ::              &
                                                             u3d_hv      , &
                                                             v3d_hv      , &
                                                             t3d_hv      , &
                                                             qv3d_hv     , &
                                                             qc3d_hv     , &
                                                             qi3d_hv     , &
                                                             p3d_hv      , &
                                                             pi3d_hv     , &
                                                             rublten_hv  , &
                                                             rvblten_hv  , &
                                                             rthblten_hv , &
                                                             rqvblten_hv , &
                                                             rqcblten_hv , &
                                                             rqiblten_hv , &
                                                             dz8w_hv     , &
                                                             exch_h_hv   , &
                                                             exch_m_hv   , &
                                                             rthraten_hv

   real,                   dimension(its:ite,kts:kte+1)    ::              &
                                                             p3di_hv
      
   real,                   dimension(its:ite)              ::              &
                                                             psfc_hv     , &
                                                             znt_hv      , &
                                                             ust_hv      , &
                                                             hpbl_hv     , &
                                                             psim_hv     , &
                                                             psih_hv     , &
                                                             xland_hv    , &
                                                             hfx_hv      , &
                                                             qfx_hv      , &
                                                             wspd_hv     , &
                                                             br_hv       , &
                                                             wstar_hv    , &
                                                             delta_hv    , &
                                                             u10_hv      , &
                                                             v10_hv      , &
                                                             uoce_hv     , &
                                                             voce_hv     , &
                                                             ctopo_hv    , &
                                                             ctopo2_hv
      
   integer,                dimension(its:ite)              ::              &
                                                             kpbl2d_hv

!temporary initialization of chemical species or passive tracers:
 do j = jts, jte
    do k = kts, kte
       do i = its, ite
          n = 1
          qmix(i,k,j,n) = 0.
          n = 2
          qmix(i,k,j,n) = 0.25
          n = 3
          qmix(i,k,j,n) = 0.50
          n = 4
          qmix(i,k,j,n) = 0.75
          n = 5
          qmix(i,k,j,n) = 1.
       enddo
   enddo
 enddo
!
   do j = jts,jte
!
      !  Assign input data to local tile-sized arrays.
      
      do n = 1, nmix
         do k = kts, kte
            do i = its, ite
               qmix_hv(i,k,n) = qmix(i,k,j,n)
            end do
         end do
      end do
      
      do k = kts, kte+1
         do i = its, ite
            p3di_hv(i,k) = p3di(i,k,j)
         end do
      end do
      
      do k = kts, kte
         do i = its, ite
            u3d_hv(i,k) = u3d(i,k,j)
            v3d_hv(i,k) = v3d(i,k,j)
            t3d_hv(i,k) = t3d(i,k,j)
            qv3d_hv(i,k) = qv3d(i,k,j)
            qc3d_hv(i,k) = qc3d(i,k,j)
            qi3d_hv(i,k) = qi3d(i,k,j)
            p3d_hv(i,k) = p3d(i,k,j)
            pi3d_hv(i,k) = pi3d(i,k,j)
            dz8w_hv(i,k) = dz8w(i,k,j)
            rthraten_hv(i,k) = rthraten(i,k,j)
         end do
      end do
      
      do i = its, ite
         psfc_hv(i) = psfc(i,j)
         znt_hv(i) = znt(i,j)
         ust_hv(i) = ust(i,j)
         wspd_hv(i) = wspd(i,j)
         psim_hv(i) = psim(i,j)
         psih_hv(i) = psih(i,j)
         xland_hv(i) = xland(i,j)
         hfx_hv(i) = hfx(i,j)
         qfx_hv(i) = qfx(i,j)
         br_hv(i) = br(i,j)
         u10_hv(i) = u10(i,j)
         v10_hv(i) = v10(i,j)
         uoce_hv(i) = uoce(i,j)
         voce_hv(i) = voce(i,j)
         ctopo_hv(i) = ctopo(i,j)
         ctopo2_hv(i) = ctopo2(i,j)
      end do
!
      call bl_ysu_run(J=j,ux=u3d_hv,vx=v3d_hv                                  &
              ,tx=t3d_hv                                                       &
              ,qvx=qv3d_hv,qcx=qc3d_hv,qix=qi3d_hv                             &
              ,f_qc=flag_qc,f_qi=flag_qi                                       &
              ,nmix=nmix,qmix=qmix_hv                                          &
              ,p2d=p3d_hv,p2di=p3di_hv                                         &
              ,pi2d=pi3d_hv                                                    &
              ,utnp=rublten_hv,vtnp=rvblten_hv                                 &
              ,ttnp=rthblten_hv,qvtnp=rqvblten_hv                              &
              ,qctnp=rqcblten_hv,qitnp=rqiblten_hv                             &
              ,qmixtnp=rqmixblten_hv                                           &
              ,cp=cp,g=g,rovcp=rovcp,rd=rd,rovg=rovg                           &    
              ,xlv=xlv,rv=rv                                                   &
              ,ep1=ep1,ep2=ep2,karman=karman                                   &
              ,dz8w2d=dz8w_hv                                                  &
              ,psfcpa=psfc_hv,znt=znt_hv,ust=ust_hv                            &
              ,hpbl=hpbl_hv                                                    &
              ,psim=psim_hv                                                    &
              ,psih=psih_hv,xland=xland_hv                                     &
              ,hfx=hfx_hv,qfx=qfx_hv                                           &
              ,wspd=wspd_hv,br=br_hv                                           &
              ,dt=dt,kpbl1d=kpbl2d_hv                                          &
              ,exch_hx=exch_h_hv                                               &
              ,exch_mx=exch_m_hv                                               &
              ,wstar=wstar_hv                                                  &
              ,delta=delta_hv                                                  &
              ,u10=u10_hv,v10=v10_hv                                           &
              ,uox=uoce_hv,vox=voce_hv                                         &
              ,rthraten=rthraten_hv                                            &
              ,ysu_topdown_pblmix=ysu_topdown_pblmix                           &
              ,ctopo=ctopo_hv,ctopo2=ctopo2_hv                                 &
              ,its=its,ite=ite, kts=kts,kte=kte                                &
              ,errmsg=errmsg,errflg=errflg                                     )
!
      !  Assign local data back to full-sized arrays.
      !  Only required for the INTENT(OUT) or INTENT(INOUT) arrays.
      
      do n = 1, nmix
         do k = kts, kte
            do i = its, ite
               rqmixblten(i,k,j,n) = rqmixblten_hv(i,k,n)
            end do
         end do
      end do
      
      do k = kts, kte
         do i = its, ite
            rublten(i,k,j) = rublten_hv(i,k)
            rvblten(i,k,j) = rvblten_hv(i,k)
            rthblten(i,k,j) = rthblten_hv(i,k)
            rqvblten(i,k,j) = rqvblten_hv(i,k)
            rqcblten(i,k,j) = rqcblten_hv(i,k)
            rqiblten(i,k,j) = rqiblten_hv(i,k)
            exch_h(i,k,j) = exch_h_hv(i,k)
            exch_m(i,k,j) = exch_m_hv(i,k)
         end do
      end do
      
      do i = its, ite
         u10(i,j) = u10_hv(i)
         v10(i,j) = v10_hv(i)
         hpbl(i,j) = hpbl_hv(i)
         kpbl2d(i,j) = kpbl2d_hv(i)
         wstar(i,j) = wstar_hv(i)
         delta(i,j) = delta_hv(i)
      end do
!
     do k = kts,kte
       do i = its,ite
         rthblten(i,k,j) = rthblten(i,k,j)/pi3d(i,k,j)
       enddo
     enddo
!
!  call mpas_log_write(' ')
!  call mpas_log_write('--- end subroutine bl_ysu_run:')
!  do i = its,ite
!     do k = kts, kte
!        call mpas_log_write('$i $i $r $r $r $r $r $r',intArgs=(/i,k/),realArgs=(/rthblten(i,k,j), &
!                  rqvblten(i,k,j),rqcblten(i,k,j),rqiblten(i,k,j),rublten(i,k,j),rvblten(i,k,j)/))
!     enddo
!     call mpas_log_write(' ')
!  enddo
!  do i = its,ite
!     do k = kts,kte
!        call mpas_log_write('$i $i $r $r $r $r $r $r $r $r $r $r',intArgs=(/i,k/), &
!                  realArgs=(/(qmix(i,k,j,n),n=1,nmix),(rqmixblten(i,k,j,n),n=1,nmix)/))
!     enddo
!     call mpas_log_write(' ')
!  enddo
   enddo
!
   end subroutine ysu
!
!-------------------------------------------------------------------------------
!
   subroutine bl_ysu_run(j,ux,vx,tx,qvx,qcx,qix,nmix,qmix,p2d,p2di,pi2d,       &
                  f_qc,f_qi,                                                   &
                  utnp,vtnp,ttnp,qvtnp,qctnp,qitnp,qmixtnp,                    &
                  cp,g,rovcp,rd,rovg,ep1,ep2,karman,xlv,rv,                    &
                  dz8w2d,psfcpa,                                               &
                  znt,ust,hpbl,psim,psih,                                      &
                  xland,hfx,qfx,wspd,br,                                       &
                  dt,kpbl1d,                                                   &
                  exch_hx,exch_mx,                                             &
                  wstar,delta,                                                 &
                  u10,v10,                                                     &
                  uox,vox,                                                     &
                  rthraten,                                                    &
                  ysu_topdown_pblmix,                                          &
                  ctopo,ctopo2,                                                &
                  its,ite, kts,kte,                                            &
                  errmsg,errflg                                                &
                   )
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
!     this code is a revised vertical diffusion package ("ysupbl")
!     with a nonlocal turbulent mixing in the pbl after "mrfpbl".
!     the ysupbl (hong et al. 2006) is based on the study of noh
!     et al.(2003) and accumulated realism of the behavior of the
!     troen and mahrt (1986) concept implemented by hong and pan(1996).
!     the major ingredient of the ysupbl is the inclusion of an explicit
!     treatment of the entrainment processes at the entrainment layer.
!     this routine uses an implicit approach for vertical flux
!     divergence and does not require "miter" timesteps.
!     it includes vertical diffusion in the stable atmosphere
!     and moist vertical diffusion in clouds.
!
!     mrfpbl:
!     coded by song-you hong (ncep), implemented by jimy dudhia (ncar)
!              fall 1996
!
!     ysupbl:
!     coded by song-you hong (yonsei university) and implemented by
!              song-you hong (yonsei university) and jimy dudhia (ncar)
!              summer 2002
!
!     further modifications :
!              an enhanced stable layer mixing, april 2008
!              ==> increase pbl height when sfc is stable (hong 2010)
!              pressure-level diffusion, april 2009
!              ==> negligible differences
!              implicit forcing for momentum with clean up, july 2009
!              ==> prevents model blowup when sfc layer is too low
!              incresea of lamda, maximum (30, 0.1 x del z) feb 2010
!              ==> prevents model blowup when delz is extremely large
!              revised prandtl number at surface, peggy lemone, feb 2010
!              ==> increase kh, decrease mixing due to counter-gradient term
!              revised thermal, shin et al. mon. wea. rev. , songyou hong, aug 2011
!              ==> reduce the thermal strength when z1 < 0.1 h
!              revised prandtl number for free convection, dudhia, mar 2012
!              ==> pr0 = 1 + bke (=0.272) when neutral, kh is reduced
!              minimum kzo = 0.01, lo = min (30m,delz), hong, mar 2012
!              ==> weaker mixing when stable, and les resolution in vertical
!              gz1oz0 is removed, and psim psih are ln(z1/z0)-psim,h, hong, mar 2012
!              ==> consider thermal z0 when differs from mechanical z0
!              a bug fix in wscale computation in stable bl, sukanta basu, jun 2012
!              ==> wscale becomes small with height, and less mixing in stable bl
!              revision in background diffusion (kzo), jan 2016
!              ==> kzo = 0.1 for momentum and = 0.01 for mass to account for
!                  internal wave mixing of large et al. (1994), songyou hong, feb 2016
!              ==> alleviate superious excessive mixing when delz is large
!
!     references:
!
!        hong (2010) quart. j. roy. met. soc
!        hong, noh, and dudhia (2006), mon. wea. rev.
!        hong and pan (1996), mon. wea. rev.
!        noh, chun, hong, and raasch (2003), boundary layer met.
!        troen and mahrt (1986), boundary layer met.
!
!-------------------------------------------------------------------------------
!
   real,parameter    ::  xkzminm = 0.1,xkzminh = 0.01
   real,parameter    ::  xkzmin = 0.01,xkzmax = 1000.,rimin = -100.
   real,parameter    ::  rlam = 30.,prmin = 0.25,prmax = 4.
   real,parameter    ::  brcr_ub = 0.0,brcr_sb = 0.25,cori = 1.e-4
   real,parameter    ::  afac = 6.8,bfac = 6.8,pfac = 2.0,pfac_q = 2.0
   real,parameter    ::  phifac = 8.,sfcfrac = 0.1
   real,parameter    ::  d1 = 0.02, d2 = 0.05, d3 = 0.001
   real,parameter    ::  h1 = 0.33333335, h2 = 0.6666667
   real,parameter    ::  zfmin = 1.e-8,aphi5 = 5.,aphi16 = 16.
   real,parameter    ::  tmin=1.e-2
   real,parameter    ::  gamcrt = 3.,gamcrq = 2.e-3
   real,parameter    ::  xka = 2.4e-5
   integer,parameter ::  imvdif = 1
   real,parameter    ::  rcl = 1.0
!
   integer,  intent(in   )   ::     its,ite, kts,kte,                          &
                                    j

   integer,  intent(in)      ::     ysu_topdown_pblmix 
!
   integer,  intent(in)      ::     nmix
!
   real,     intent(in   )   ::     dt,cp,g,rovcp,rovg,rd,xlv,rv
!
   real,     intent(in )     ::     ep1,ep2,karman
!
   logical,  intent(in )     ::     f_qc, f_qi
!
   real,     dimension( its:ite, kts:kte ),                                    &
             intent(in)      ::                                        dz8w2d, &
                                                                         pi2d
!
   real,     dimension( its:ite, kts:kte )                                   , &
             intent(in   )   ::                                            tx, &
                                                                          qvx, &
                                                                          qcx, &
                                                                          qix
!
   real,     dimension( its:ite, kts:kte, nmix )                             , &
             intent(in   )   ::                                          qmix
!
   real,     dimension( its:ite, kts:kte )                                   , &
             intent(out  )   ::                                          utnp, &
                                                                         vtnp, &
                                                                         ttnp, &
                                                                        qvtnp, &
                                                                        qctnp, &
                                                                        qitnp
!
   real,     dimension( its:ite, kts:kte, nmix )                             , &
             intent(out  )   ::                                       qmixtnp
!
   real,     dimension( its:ite, kts:kte+1 )                                 , &
             intent(in   )   ::                                          p2di
!
   real,     dimension( its:ite, kts:kte )                                   , &
             intent(in   )   ::                                           p2d
!
   real,     dimension( its:ite )                                            , &
             intent(out  )   ::                                          hpbl
!
   real,     dimension( its:ite )                                            , &
             intent(in   )   ::                                           ust, &
                                                                          znt
   real,     dimension( its:ite )                                            , &
             intent(in   )   ::                                         xland, &
                                                                          hfx, &
                                                                          qfx
!
   real,     dimension( its:ite ), intent(in   )   ::                    wspd
   real,     dimension( its:ite ), intent(in  )    ::                      br
!
   real,     dimension( its:ite ), intent(in   )   ::                    psim, &
                                                                         psih
!
   real,     dimension( its:ite ), intent(in   )   ::                  psfcpa
   integer,  dimension( its:ite ), intent(out  )   ::                  kpbl1d
!
   real,     dimension( its:ite, kts:kte )                                   , &
             intent(in   )   ::                                            ux, &
                                                                           vx, &
                                                                      rthraten
   real,     dimension( its:ite )                                            , &
             optional                                                        , &
             intent(in   )   ::                                         ctopo, &
                                                                       ctopo2
!
   character(len=*), intent(out)   ::                                  errmsg
   integer,          intent(out)   ::                                  errflg

!
! local vars
!
   real,     dimension( its:ite )            ::                           hol
   real,     dimension( its:ite, kts:kte+1 ) ::                            zq
!
   real,     dimension( its:ite, kts:kte )   ::                                &
                                                               thx,thvx,thlix, &
                                                                          del, &
                                                                          dza, &
                                                                          dzq, &
                                                                        xkzom, &
                                                                        xkzoh, &
                                                                           za
!
   real,    dimension( its:ite )             ::                                &
                                                                         rhox, &
                                                                       govrth, &
                                                                  zl1,thermal, &
                                                                       wscale, &
                                                                  hgamt,hgamq, &
                                                                    brdn,brup, &
                                                                    phim,phih, &
                                                                  dusfc,dvsfc, &
                                                                  dtsfc,dqsfc, &
                                                                        prpbl, &
                                                              wspd1,thermalli
!
   real,    dimension( its:ite, kts:kte )    ::                xkzh,xkzm,xkzq, &
                                                                        f1,f2, &
                                                                        r1,r2, &
                                                                        ad,au, &
                                                                           cu, &
                                                                           al, &
                                                                         zfac, &
                                                                        rhox2, &
                                                                       hgamt2
!
!jdf added exch_hx
!
   real,    dimension( its:ite, kts:kte )                                    , &
            intent(out  )   ::                                        exch_hx, &
                                                                      exch_mx
!
   real,    dimension( its:ite )                                             , &
            intent(inout)    ::                                           u10, &
                                                                          v10
   real,    dimension( its:ite )                                             , &
            intent(in  )    ::                                            uox, &
                                                                          vox
   real,    dimension( its:ite )    ::                                         &
                                                                         brcr, &
                                                                        sflux, &
                                                                         zol1, &
                                                                    brcr_sbro
!
   real,    dimension( its:ite, kts:kte)     ::                         r3,f3
   integer, dimension( its:ite )             ::                  kpbl,kpblold
!
   logical, dimension( its:ite )             ::                        pblflg, &
                                                                       sfcflg, &
                                                                       stable, &
                                                                     cloudflg

   logical                                   ::                     definebrup
!
   integer ::  n,i,k,l,ic,is,kk
   integer ::  klpbl
!
!
   real    ::  dt2,rdt,spdk2,fm,fh,hol1,gamfac,vpert,prnum,prnum0
   real    ::  ss,ri,qmean,tmean,alph,chi,zk,rl2,dk,sri
   real    ::  brint,dtodsd,dtodsu,rdz,dsdzt,dsdzq,dsdz2,rlamdz
   real    ::  utend,vtend,ttend,qtend
   real    ::  dtstep,govrthv
   real    ::  cont, conq, conw, conwrc
!

   real, dimension( its:ite, kts:kte )     ::                wscalek,wscalek2
   real, dimension( its:ite ), intent(out) ::                           wstar, &
                                                                        delta
   real, dimension( its:ite, kts:kte )     ::                     xkzml,xkzhl, &
                                                               zfacent,entfac
   real, dimension( its:ite, kts:kte )     ::                            qcxl, &
                                                                         qixl
   real, dimension( its:ite, kts:kte, nmix ) ::                         qmixl
   real, dimension( its:ite )              ::                            ust3, &
                                                                       wstar3, &
                                                                     wstar3_2, &
                                                                  hgamu,hgamv, &
                                                                      wm2, we, &
                                                                       bfxpbl, &
                                                                hfxpbl,qfxpbl, &
                                                                ufxpbl,vfxpbl, &
                                                                        dthvx
   real    ::  prnumfac,bfx0,hfx0,qfx0,delb,dux,dvx,                           &
               dsdzu,dsdzv,wm3,dthx,dqx,wspd10,ross,tem1,dsig,tvcon,conpr,     &
               prfac,prfac2,phim8z,radsum,tmp1,templ,rvls,temps,ent_eff,    &
               rcldb,bruptmp,radflux,vconvlim,vconvnew,fluxc,vconvc,vconv
!topo-corr
   real,    dimension( its:ite, kts:kte )    ::                          fric, &
                                                                       tke_ysu,&
                                                                        el_ysu,&
                                                                     shear_ysu,&
                                                                     buoy_ysu
   real,    dimension( its:ite )             ::                       pblh_ysu,&
                                                                      vconvfx

   real,     dimension( kts:kte )   :: dummy1,dummy2,dummy4
   real,     dimension( kts:kte+1 ) :: dummy3
!
!-------------------------------------------------------------------------------
!
   klpbl = kte
!
   cont=cp/g
   conq=xlv/g
   conw=1./g
   conwrc = conw*sqrt(rcl)
   conpr = bfac*karman*sfcfrac
!
!  k-start index for tracer diffusion
!
   if(f_qc) then
      do k = kts,kte
        do i = its,ite
           qcxl(i,k)  = qcx(i,k)
        enddo
      enddo
   else
      do k = kts,kte
        do i = its,ite
           qcxl(i,k) = 0.
        enddo
      enddo
   endif
!
   if(f_qi) then
      do k = kts,kte
        do i = its,ite
           qixl(i,k)  = qix(i,k)
        enddo
      enddo
   else
      do k = kts,kte
        do i = its,ite
           qixl(i,k) = 0.
        enddo
      enddo
   endif
!
   do k = kts,kte
     do i = its,ite
       thx(i,k) = tx(i,k)/pi2d(i,k)
       thlix(i,k) = (tx(i,k)-xlv*qcxl(i,k)/cp-2.834E6*qixl(i,k)/cp)/pi2d(i,k)
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       tvcon = (1.+ep1*qvx(i,k))
       thvx(i,k) = thx(i,k)*tvcon
     enddo
   enddo
!
   do i = its,ite
     tvcon = (1.+ep1*qvx(i,1))
     rhox(i) = psfcpa(i)/(rd*tx(i,1)*tvcon)
     govrth(i) = g/thx(i,1)
   enddo
!
!-----compute the height of full- and half-sigma levels above ground
!     level, and the layer thicknesses.
!
   do i = its,ite
     zq(i,1) = 0.
   enddo
!
   do k = kts,kte
     do i = its,ite
       zq(i,k+1) = dz8w2d(i,k)+zq(i,k)
       tvcon = (1.+ep1*qvx(i,k))
       rhox2(i,k) = p2d(i,k)/(rd*tx(i,k)*tvcon)
     enddo
   enddo
!
   do k = kts,kte
     do i = its,ite
       za(i,k) = 0.5*(zq(i,k)+zq(i,k+1))
       dzq(i,k) = zq(i,k+1)-zq(i,k)
       del(i,k) = p2di(i,k)-p2di(i,k+1)
     enddo
   enddo
!
   do i = its,ite
     dza(i,1) = za(i,1)
   enddo
!
   do k = kts+1,kte
     do i = its,ite
       dza(i,k) = za(i,k)-za(i,k-1)
     enddo
   enddo
!
!-----initialize output and local exchange coefficents:
   do k = kts,kte
     do i = its,ite
       exch_hx(i,k) = 0.
       exch_mx(i,k) = 0.
       xkzh(i,k)    = 0.
       xkzhl(i,k)   = 0.
       xkzm(i,k)    = 0.
       xkzml(i,k)   = 0.
       xkzq(i,k)    = 0.
     enddo
   enddo
!
   do i = its,ite
     wspd1(i) = sqrt( (ux(i,1)-uox(i))*(ux(i,1)-uox(i)) + (vx(i,1)-vox(i))*(vx(i,1)-vox(i)) )+1.e-9
   enddo
!
!---- compute vertical diffusion
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!     compute preliminary variables
!
   dtstep = dt
   dt2 = 2.*dtstep
   rdt = 1./dt2
!
   do i = its,ite
     bfxpbl(i) = 0.0
     hfxpbl(i) = 0.0
     qfxpbl(i) = 0.0
     ufxpbl(i) = 0.0
     vfxpbl(i) = 0.0
     hgamu(i)  = 0.0
     hgamv(i)  = 0.0
     wstar3_2(i) =  0.0
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       wscalek(i,k) = 0.0
       wscalek2(i,k) = 0.0
     enddo
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       zfac(i,k) = 0.0
     enddo
   enddo
   do k = kts,klpbl-1
     do i = its,ite
       xkzom(i,k) = xkzminm
       xkzoh(i,k) = xkzminh
     enddo
   enddo
!
   do i = its,ite
     dusfc(i) = 0.
     dvsfc(i) = 0.
     dtsfc(i) = 0.
     dqsfc(i) = 0.
   enddo
!
   do i = its,ite
     hgamt(i)  = 0.
     hgamq(i)  = 0.
     wscale(i) = 0.
     kpbl(i)   = 1
     hpbl(i)   = zq(i,1)
     zl1(i)    = za(i,1)
     thermal(i)= thvx(i,1)
     thermalli(i) = thlix(i,1)
     pblflg(i) = .true.
     sfcflg(i) = .true.
     sflux(i) = hfx(i)/rhox(i)/cp + qfx(i)/rhox(i)*ep1*thx(i,1)
     if(br(i).gt.0.0) sfcflg(i) = .false.
   enddo
!
!     compute the first guess of pbl height
!
   do i = its,ite
     stable(i) = .false.
     brup(i) = br(i)
     brcr(i) = brcr_ub
   enddo
!
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo
!
   do i = its,ite
     k = kpbl(i)
     if(brdn(i).ge.brcr(i))then
       brint = 0.
     elseif(brup(i).le.brcr(i))then
       brint = 1.
     else
       brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
     endif
     hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
     if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
     if(kpbl(i).le.1) pblflg(i) = .false.
   enddo
!
   do i = its,ite
     fm = psim(i)
     fh = psih(i)
     zol1(i) = max(br(i)*fm*fm/fh,rimin)
     if(sfcflg(i))then
       zol1(i) = min(zol1(i),-zfmin)
     else
       zol1(i) = max(zol1(i),zfmin)
     endif
     hol1 = zol1(i)*hpbl(i)/zl1(i)*sfcfrac
     if(sfcflg(i))then
       phim(i) = (1.-aphi16*hol1)**(-1./4.)
       phih(i) = (1.-aphi16*hol1)**(-1./2.)
       bfx0 = max(sflux(i),0.)
       hfx0 = max(hfx(i)/rhox(i)/cp,0.)
       qfx0 = max(ep1*thx(i,1)*qfx(i)/rhox(i),0.)
       wstar3(i) = (govrth(i)*bfx0*hpbl(i))
       wstar(i) = (wstar3(i))**h1
     else
       phim(i) = (1.+aphi5*hol1)
       phih(i) = phim(i)
       wstar(i)  = 0.
       wstar3(i) = 0.
     endif
     ust3(i)   = ust(i)**3.
     wscale(i) = (ust3(i)+phifac*karman*wstar3(i)*0.5)**h1
     wscale(i) = min(wscale(i),ust(i)*aphi16)
     wscale(i) = max(wscale(i),ust(i)/aphi5)
   enddo
!
!     compute the surface variables for pbl height estimation
!     under unstable conditions
!
   do i = its,ite
     if(sfcflg(i).and.sflux(i).gt.0.0)then
       gamfac   = bfac/rhox(i)/wscale(i)
       hgamt(i) = min(gamfac*hfx(i)/cp,gamcrt)
       hgamq(i) = min(gamfac*qfx(i),gamcrq)
       vpert = (hgamt(i)+ep1*thx(i,1)*hgamq(i))/bfac*afac
       thermal(i) = thermal(i)+max(vpert,0.)*min(za(i,1)/(sfcfrac*hpbl(i)),1.0)
       thermalli(i)= thermalli(i)+max(vpert,0.)*min(za(i,1)/(sfcfrac*hpbl(i)),1.0)
       hgamt(i) = max(hgamt(i),0.0)
       hgamq(i) = max(hgamq(i),0.0)
       brint    = -15.9*ust(i)*ust(i)/wspd(i)*wstar3(i)/(wscale(i)**4.)
       hgamu(i) = brint*ux(i,1)
       hgamv(i) = brint*vx(i,1)
     else
       pblflg(i) = .false.
     endif
   enddo
!
!     enhance the pbl height by considering the thermal
!
   do i = its,ite
     if(pblflg(i))then
       kpbl(i) = 1
       hpbl(i) = zq(i,1)
     endif
   enddo
!
   do i = its,ite
     if(pblflg(i))then
       stable(i) = .false.
       brup(i) = br(i)
       brcr(i) = brcr_ub
     endif
   enddo
!
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i).and.pblflg(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo
!
!     enhance pbl by theta-li
!
   if (ysu_topdown_pblmix.eq.1)then
     do i = its,ite
        kpblold(i) = kpbl(i)
        definebrup=.false.
        do k = kpblold(i), kte-1
           spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
           bruptmp = (thlix(i,k)-thermalli(i))*(g*za(i,k)/thlix(i,1))/spdk2
           stable(i) = bruptmp.ge.brcr(i)
           if (definebrup) then
           kpbl(i) = k
           brup(i) = bruptmp
           definebrup=.false.
           endif
           if (.not.stable(i)) then !overwrite brup brdn values
           brdn(i)=bruptmp
           definebrup=.true.
           pblflg(i)=.true.
           endif
        enddo
     enddo
   endif

   do i = its,ite
     if(pblflg(i)) then
       k = kpbl(i)
       if(brdn(i).ge.brcr(i))then
         brint = 0.
       elseif(brup(i).le.brcr(i))then
         brint = 1.
       else
         brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
       endif
       hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
       if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
       if(kpbl(i).le.1) pblflg(i) = .false.
     endif
   enddo
!
!     stable boundary layer
!
   do i = its,ite
     if((.not.sfcflg(i)).and.hpbl(i).lt.zq(i,2)) then
       brup(i) = br(i)
       stable(i) = .false.
     else
       stable(i) = .true.
     endif
   enddo
!
   do i = its,ite
     if((.not.stable(i)).and.((xland(i)-1.5).ge.0))then
       wspd10 = u10(i)*u10(i) + v10(i)*v10(i)
       wspd10 = sqrt(wspd10)
       ross = wspd10 / (cori*znt(i))
       brcr_sbro(i) = min(0.16*(1.e-7*ross)**(-0.18),.3)
     endif
   enddo
!
   do i = its,ite
     if(.not.stable(i))then
       if((xland(i)-1.5).ge.0)then
         brcr(i) = brcr_sbro(i)
       else
         brcr(i) = brcr_sb
       endif
     endif
   enddo
!
   do k = 2,klpbl
     do i = its,ite
       if(.not.stable(i))then
         brdn(i) = brup(i)
         spdk2   = max(ux(i,k)**2+vx(i,k)**2,1.)
         brup(i) = (thvx(i,k)-thermal(i))*(g*za(i,k)/thvx(i,1))/spdk2
         kpbl(i) = k
         stable(i) = brup(i).gt.brcr(i)
       endif
     enddo
   enddo
!
   do i = its,ite
     if((.not.sfcflg(i)).and.hpbl(i).lt.zq(i,2)) then
       k = kpbl(i)
       if(brdn(i).ge.brcr(i))then
         brint = 0.
       elseif(brup(i).le.brcr(i))then
         brint = 1.
       else
         brint = (brcr(i)-brdn(i))/(brup(i)-brdn(i))
       endif
       hpbl(i) = za(i,k-1)+brint*(za(i,k)-za(i,k-1))
       if(hpbl(i).lt.zq(i,2)) kpbl(i) = 1
       if(kpbl(i).le.1) pblflg(i) = .false.
     endif
   enddo
!
!     estimate the entrainment parameters
!
   do i = its,ite
     cloudflg(i)=.false. 
     if(pblflg(i)) then
       k = kpbl(i) - 1
       wm3       = wstar3(i) + 5. * ust3(i)
       wm2(i)    = wm3**h2
       bfxpbl(i) = -0.15*thvx(i,1)/g*wm3/hpbl(i)
       dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),tmin)
       we(i) = max(bfxpbl(i)/dthvx(i),-sqrt(wm2(i)))
       if((qcxl(i,k)+qixl(i,k)).gt.0.01e-3.and.ysu_topdown_pblmix.eq.1)then
           if ( kpbl(i) .ge. 2) then
                cloudflg(i)=.true. 
                templ=thlix(i,k)*(p2di(i,k+1)/100000)**rovcp
                !rvls is ws at full level
                rvls=100.*6.112*EXP(17.67*(templ-273.16)/(templ-29.65))*(ep2/p2di(i,k+1))
                temps=templ + ((qvx(i,k)+qcxl(i,k))-rvls)/(cp/xlv  + &
                ep2*xlv*rvls/(rd*templ**2))
                rvls=100.*6.112*EXP(17.67*(temps-273.15)/(temps-29.65))*(ep2/p2di(i,k+1))
                rcldb=max((qvx(i,k)+qcxl(i,k))-rvls,0.)
                !entrainment efficiency
                dthvx(i)  = (thlix(i,k+2)+thx(i,k+2)*ep1*(qvx(i,k+2)+qcxl(i,k+2))) &
                          - (thlix(i,k) + thx(i,k)  *ep1*(qvx(i,k)  +qcxl(i,k)))
                dthvx(i)  = max(dthvx(i),0.1)
                tmp1      = xlv/cp * rcldb/(pi2d(i,k)*dthvx(i))
                ent_eff   = 0.2 * 8. * tmp1 +0.2

                radsum=0.
                do kk = 1,kpbl(i)-1
                   radflux=rthraten(i,kk)*pi2d(i,kk) !converts theta/s to temp/s
                   radflux=radflux*cp/g*(p2di(i,kk)-p2di(i,kk+1)) ! converts temp/s to W/m^2
                   if (radflux < 0.0 ) radsum=abs(radflux)+radsum
                enddo
                radsum=max(radsum,0.0)

                !recompute entrainment from sfc thermals
                bfx0 = max(max(sflux(i),0.0)-radsum/rhox2(i,k)/cp,0.)
                bfx0 = max(sflux(i),0.0)
                wm3 = (govrth(i)*bfx0*hpbl(i))+5. * ust3(i)
                wm2(i)    = wm3**h2
                bfxpbl(i) = -0.15*thvx(i,1)/g*wm3/hpbl(i)
                dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),tmin)
                we(i) = max(bfxpbl(i)/dthvx(i),-sqrt(wm2(i)))

                !entrainment from PBL top thermals
                bfx0 = max(radsum/rhox2(i,k)/cp-max(sflux(i),0.0),0.)
                bfx0 = max(radsum/rhox2(i,k)/cp,0.)
                wm3       = (g/thvx(i,k)*bfx0*hpbl(i)) ! this is wstar3(i)
                wm2(i)    = wm2(i)+wm3**h2
                bfxpbl(i) = - ent_eff * bfx0
                dthvx(i)  = max(thvx(i,k+1)-thvx(i,k),0.1)
                we(i) = we(i) + max(bfxpbl(i)/dthvx(i),-sqrt(wm3**h2))

                !wstar3_2
                bfx0 = max(radsum/rhox2(i,k)/cp,0.)
                wstar3_2(i) =  (g/thvx(i,k)*bfx0*hpbl(i))
                !recompute hgamt 
                wscale(i) = (ust3(i)+phifac*karman*(wstar3(i)+wstar3_2(i))*0.5)**h1
                wscale(i) = min(wscale(i),ust(i)*aphi16)
                wscale(i) = max(wscale(i),ust(i)/aphi5)
                gamfac   = bfac/rhox(i)/wscale(i)
                hgamt(i) = min(gamfac*hfx(i)/cp,gamcrt)
                hgamq(i) = min(gamfac*qfx(i),gamcrq)
                gamfac   = bfac/rhox2(i,k)/wscale(i)
                hgamt2(i,k) = min(gamfac*radsum/cp,gamcrt)
                hgamt(i) = max(hgamt(i),0.0) + max(hgamt2(i,k),0.0)
                brint    = -15.9*ust(i)*ust(i)/wspd(i)*(wstar3(i)+wstar3_2(i))/(wscale(i)**4.)
                hgamu(i) = brint*ux(i,1)
                hgamv(i) = brint*vx(i,1)
           endif
       endif
       prpbl(i) = 1.0
       dthx  = max(thx(i,k+1)-thx(i,k),tmin)
       dqx   = min(qvx(i,k+1)-qvx(i,k),0.0)
       hfxpbl(i) = we(i)*dthx
       qfxpbl(i) = we(i)*dqx
!
       dux = ux(i,k+1)-ux(i,k)
       dvx = vx(i,k+1)-vx(i,k)
       if(dux.gt.tmin) then
         ufxpbl(i) = max(prpbl(i)*we(i)*dux,-ust(i)*ust(i))
       elseif(dux.lt.-tmin) then
         ufxpbl(i) = min(prpbl(i)*we(i)*dux,ust(i)*ust(i))
       else
         ufxpbl(i) = 0.0
       endif
       if(dvx.gt.tmin) then
         vfxpbl(i) = max(prpbl(i)*we(i)*dvx,-ust(i)*ust(i))
       elseif(dvx.lt.-tmin) then
         vfxpbl(i) = min(prpbl(i)*we(i)*dvx,ust(i)*ust(i))
       else
         vfxpbl(i) = 0.0
       endif
       delb  = govrth(i)*d3*hpbl(i)
       delta(i) = min(d1*hpbl(i) + d2*wm2(i)/delb,100.)
     endif
   enddo
!
   do k = kts,klpbl
     do i = its,ite
       if(pblflg(i).and.k.ge.kpbl(i))then
         entfac(i,k) = ((zq(i,k+1)-hpbl(i))/delta(i))**2.
       else
         entfac(i,k) = 1.e30
       endif
     enddo
   enddo
!
!     compute diffusion coefficients below pbl
!
   do k = kts,klpbl
     do i = its,ite
       if(k.lt.kpbl(i)) then
         zfac(i,k) = min(max((1.-(zq(i,k+1)-zl1(i))/(hpbl(i)-zl1(i))),zfmin),1.)
         zfacent(i,k) = (1.-zfac(i,k))**3.
         wscalek(i,k) = (ust3(i)+phifac*karman*wstar3(i)*(1.-zfac(i,k)))**h1
         wscalek2(i,k) = (phifac*karman*wstar3_2(i)*(zfac(i,k)))**h1
         if(sfcflg(i)) then
           prfac = conpr
           prfac2 = 15.9*(wstar3(i)+wstar3_2(i))/ust3(i)/(1.+4.*karman*(wstar3(i)+wstar3_2(i))/ust3(i))
           prnumfac = -3.*(max(zq(i,k+1)-sfcfrac*hpbl(i),0.))**2./hpbl(i)**2.
         else
           prfac = 0.
           prfac2 = 0.
           prnumfac = 0.
           phim8z = 1.+aphi5*zol1(i)*zq(i,k+1)/zl1(i)
           wscalek(i,k) = ust(i)/phim8z
           wscalek(i,k) = max(wscalek(i,k),0.001)
         endif
         prnum0 = (phih(i)/phim(i)+prfac)
         prnum0 = max(min(prnum0,prmax),prmin)
           xkzm(i,k) = wscalek(i,k) *karman*    zq(i,k+1)      *    zfac(i,k)**pfac+ &
                       wscalek2(i,k)*karman*(hpbl(i)-zq(i,k+1))*(1-zfac(i,k))**pfac
         !Do not include xkzm at kpbl-1 since it changes entrainment
         if (k.eq.kpbl(i)-1.and.cloudflg(i).and.we(i).lt.0.0) then
           xkzm(i,k) = 0.0
         endif
         prnum =  1. + (prnum0-1.)*exp(prnumfac)
         xkzq(i,k) = xkzm(i,k)/prnum*zfac(i,k)**(pfac_q-pfac)
         prnum0 = prnum0/(1.+prfac2*karman*sfcfrac)
         prnum =  1. + (prnum0-1.)*exp(prnumfac)
         xkzh(i,k) = xkzm(i,k)/prnum
         xkzm(i,k) = xkzm(i,k)+xkzom(i,k)
         xkzh(i,k) = xkzh(i,k)+xkzoh(i,k)
         xkzq(i,k) = xkzq(i,k)+xkzoh(i,k)
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         xkzq(i,k) = min(xkzq(i,k),xkzmax)
       endif
     enddo
   enddo
!
!     compute diffusion coefficients over pbl (free atmosphere)
!
   do k = kts,kte-1
     do i = its,ite
       if(k.ge.kpbl(i)) then
         ss = ((ux(i,k+1)-ux(i,k))*(ux(i,k+1)-ux(i,k))                         &
              +(vx(i,k+1)-vx(i,k))*(vx(i,k+1)-vx(i,k)))                        &
              /(dza(i,k+1)*dza(i,k+1))+1.e-9
         govrthv = g/(0.5*(thvx(i,k+1)+thvx(i,k)))
         ri = govrthv*(thvx(i,k+1)-thvx(i,k))/(ss*dza(i,k+1))
         if(imvdif.eq.1)then
           if((qcxl(i,k)+qixl(i,k)).gt.0.01e-3.and. &
              (qcxl(i,k+1)+qixl(i,k+1)).gt.0.01e-3)then
!      in cloud
             qmean = 0.5*(qvx(i,k)+qvx(i,k+1))
             tmean = 0.5*(tx(i,k)+tx(i,k+1))
             alph  = xlv*qmean/rd/tmean
             chi   = xlv*xlv*qmean/cp/rv/tmean/tmean
             ri    = (1.+alph)*(ri-g*g/ss/tmean/cp*((chi-alph)/(1.+chi)))
           endif
         endif
         zk = karman*zq(i,k+1)
         rlamdz = min(max(0.1*dza(i,k+1),rlam),300.)
         rlamdz = min(dza(i,k+1),rlamdz)
         rl2 = (zk*rlamdz/(rlamdz+zk))**2
         dk = rl2*sqrt(ss)
         if(ri.lt.0.)then
! unstable regime
           ri = max(ri, rimin)
           sri = sqrt(-ri)
           xkzm(i,k) = dk*(1+8.*(-ri)/(1+1.746*sri))
           xkzh(i,k) = dk*(1+8.*(-ri)/(1+1.286*sri))
         else
! stable regime
           xkzh(i,k) = dk/(1+5.*ri)**2
           prnum = 1.0+2.1*ri
           prnum = min(prnum,prmax)
           xkzm(i,k) = xkzh(i,k)*prnum
         endif
!
         xkzm(i,k) = xkzm(i,k)+xkzom(i,k)
         xkzh(i,k) = xkzh(i,k)+xkzoh(i,k)
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         xkzml(i,k) = xkzm(i,k)
         xkzhl(i,k) = xkzh(i,k)
       endif
     enddo
   enddo
!
!     compute tridiagonal matrix elements for heat
!
   do k = kts,kte
     do i = its,ite
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
       f1(i,k) = 0.
     enddo
   enddo
!
   do i = its,ite
     ad(i,1) = 1.
     f1(i,1) = thx(i,1)-300.+hfx(i)/cont/del(i,1)*dt2
   enddo
!
   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzh(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i)) then
         dsdzt = tem1*(-hgamt(i)/hpbl(i)-hfxpbl(i)*zfacent(i,k)/xkzh(i,k))
         f1(i,k)   = f1(i,k)+dtodsd*dsdzt
         f1(i,k+1) = thx(i,k+1)-300.-dtodsu*dsdzt
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzh(i,k) = -we(i)*dza(i,kpbl(i))*exp(-entfac(i,k))
         xkzh(i,k) = sqrt(xkzh(i,k)*xkzhl(i,k))
         xkzh(i,k) = max(xkzh(i,k),xkzoh(i,k))
         xkzh(i,k) = min(xkzh(i,k),xkzmax)
         f1(i,k+1) = thx(i,k+1)-300.
       else
         f1(i,k+1) = thx(i,k+1)-300.
       endif
       tem1   = dsig*xkzh(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
       exch_hx(i,k+1) = xkzh(i,k)
     enddo
   enddo
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
       r1(i,k) = f1(i,k)
     enddo
   enddo
!
   call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)
!
!     recover tendencies of heat
!
   do k = kte,kts,-1
     do i = its,ite
       ttend = (f1(i,k)-thx(i,k)+300.)*rdt*pi2d(i,k)
       ttnp(i,k) = ttend
       dtsfc(i) = dtsfc(i)+ttend*cont*del(i,k)/pi2d(i,k)
     enddo
   enddo
!

!--- compute tridiagonal matrix elements for water vapor, cloud water, and cloud ice:
   !--- initialization of k-coefficient above the PBL.
   do i = its,ite
      do k = kts,kte-1
         if(k .ge. kpbl(i)) xkzq(i,k) = xkzh(i,k)
      enddo
   enddo

   !--- water vapor:
   do i = its,ite
      do k = kts,kte
         au(i,k) = 0.
         al(i,k) = 0.
         ad(i,k) = 0.
         f1(i,k) = 0.
         r1(i,k) = 0.
      enddo

      k = 1
      ad(i,1) = 1.
      f1(i,1) = qvx(i,1)+qfx(i)*g/del(i,1)*dt2

      do k = kts,kte-1
         dtodsd = dt2/del(i,k)
         dtodsu = dt2/del(i,k+1)
         dsig   = p2d(i,k)-p2d(i,k+1)
         rdz    = 1./dza(i,k+1)
         tem1   = dsig*xkzq(i,k)*rdz
         if(pblflg(i).and.k.lt.kpbl(i)) then
            dsdzq = tem1*(-qfxpbl(i)*zfacent(i,k)/xkzq(i,k))
            f1(i,k) = f1(i,k)+dtodsd*dsdzq
            f1(i,k+1) = qvx(i,k+1)-dtodsu*dsdzq
         elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
            xkzq(i,k) = -we(i)*dza(i,kpbl(i))*exp(-entfac(i,k))
            xkzq(i,k) = sqrt(xkzq(i,k)*xkzhl(i,k))
            xkzq(i,k) = max(xkzq(i,k),xkzoh(i,k))
            xkzq(i,k) = min(xkzq(i,k),xkzmax)
            f1(i,k+1) = qvx(i,k+1)
         else
            f1(i,k+1) = qvx(i,k+1)
         endif
         tem1      = dsig*xkzq(i,k)*rdz
         dsdz2     = tem1*rdz
         au(i,k)   = -dtodsd*dsdz2
         al(i,k)   = -dtodsu*dsdz2
         ad(i,k)   = ad(i,k)-au(i,k)
         ad(i,k+1) = 1.-al(i,k)
      enddo

      do k = kts,kte
         cu(i,k) = au(i,k)
         r1(i,k) = f1(i,k)
      enddo
   enddo
   call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)

   do i = its,ite
      do k = kte,kts,-1
         qtend = (f1(i,k)-qvx(i,k))*rdt
         qvtnp(i,k) = qtend
         dqsfc(i)   = dqsfc(i)+qtend*conq*del(i,k)
      enddo
   enddo

   !--- cloud water:
   if(f_qc) then
      do i = its,ite
         do k = kts,kte
            f1(i,k) = qcxl(i,k)
            r1(i,k) = f1(i,k)
         enddo
      enddo
      call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)
   
      do i = its,ite
         do k = kte,kts,-1
            qtend = (f1(i,k)-qcxl(i,k))*rdt
            qctnp(i,k) = qtend
         enddo
      enddo
   endif

   !--- cloud ice:
   if(f_qi) then
      do i = its,ite
         do k = kts,kte
            f1(i,k) = qixl(i,k)
            r1(i,k) = f1(i,k)
         enddo
      enddo
      call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)

      do i = its,ite
         do k = kte,kts,-1
            qtend = (f1(i,k)-qixl(i,k))*rdt
            qitnp(i,k) = qtend
         enddo
      enddo
   endif

   !--- chemical species and/or passive tracers, meaning all variables that we want to
   !    be vertically-mixed:
   if(nmix > 0) then
      do n = 1, nmix
         do i = its,ite
            do k = kts,kte
               f1(i,k) = qmix(i,k,n)
               r1(i,k) = f1(i,k)
            enddo
         enddo
         call tridin_ysu(al,ad,cu,r1,au,f1,its,ite,kts,kte,1)

         do i = its,ite
            do k = kte,kts,-1
               qtend = (f1(i,k)-qmix(i,k,n))*rdt
               qmixtnp(i,k,n) = qtend
            enddo
         enddo
      enddo
   endif

!
!     compute tridiagonal matrix elements for momentum
!
   do i = its,ite
     do k = kts,kte
       au(i,k) = 0.
       al(i,k) = 0.
       ad(i,k) = 0.
       f1(i,k) = 0.
       f2(i,k) = 0.
     enddo
   enddo
!
! paj: ctopo=1 if topo_wind=0 (default)
!raquel---paj tke code (could be replaced with shin-hong tke in future
   do i = its,ite
      do k= kts, kte-1
        shear_ysu(i,k)=xkzm(i,k)*((-hgamu(i)/hpbl(i)+(ux(i,k+1)-ux(i,k))/dza(i,k+1))*(ux(i,k+1)-ux(i,k))/dza(i,k+1) &
        + (-hgamv(i)/hpbl(i)+(vx(i,k+1)-vx(i,k))/dza(i,k+1))*(vx(i,k+1)-vx(i,k))/dza(i,k+1))
         buoy_ysu(i,k)=xkzh(i,k)*g*(1.0/thx(i,k))*(-hgamt(i)/hpbl(i)+(thx(i,k+1)-thx(i,k))/dza(i,k+1))

       zk = karman*zq(i,k+1)
 !over pbl
       if (k.ge.kpbl(i)) then
        rlamdz = min(max(0.1*dza(i,k+1),rlam),300.)
        rlamdz = min(dza(i,k+1),rlamdz)
       else
 !in pbl
        rlamdz = 150.0
       endif
       el_ysu(i,k) = zk*rlamdz/(rlamdz+zk)
       tke_ysu(i,k)=16.6*el_ysu(i,k)*(shear_ysu(i,k)-buoy_ysu(i,k))
 !q2 when q3 positive
       if(tke_ysu(i,k).le.0) then
        tke_ysu(i,k)=0.0
       else
        tke_ysu(i,k)=(tke_ysu(i,k))**0.66
       endif
      enddo
 !Hybrid pblh of MYNN
 !tke is q2
!     CALL GET_PBLH(KTS,KTE,pblh_ysu(i),thvx(i,kts:kte),&
!     &    tke_ysu(i,kts:kte),zq(i,kts:kte+1),dzq(i,kts:kte),xland(i))
      do k = kts,kte
         dummy1(k) = thvx(i,k)
         dummy2(k) = tke_ysu(i,k)
         dummy3(k) = zq(i,k)
         dummy4(k) = dzq(i,k)
      enddo
      dummy3(kte+1) = zq(i,kte+1)
      call get_pblh(kts,kte,pblh_ysu(i),dummy1,dummy2,dummy3,dummy4,xland(i))

!--- end of paj tke
! compute vconv
!      Use Beljaars over land
        if (xland(i).lt.1.5) then
        fluxc = max(sflux(i),0.0)
        vconvc=1.
        VCONV = vconvc*(g/thvx(i,1)*pblh_ysu(i)*fluxc)**.33
        else
! for water there is no topo effect so vconv not needed
        VCONV = 0.
        endif
        vconvfx(i) = vconv
!raquel
!ctopo stability correction
      fric(i,1)=ust(i)**2/wspd1(i)*rhox(i)*g/del(i,1)*dt2         &
        *(wspd1(i)/wspd(i))**2
      if(present(ctopo)) then
        vconvnew=0.9*vconvfx(i)+1.5*(max((pblh_ysu(i)-500)/1000.0,0.0))
        vconvlim = min(vconvnew,1.0)
        ad(i,1) = 1.+fric(i,1)*vconvlim+ctopo(i)*fric(i,1)*(1-vconvlim)
      else
       ad(i,1) = 1.+fric(i,1)
      endif
     f1(i,1) = ux(i,1)+uox(i)*ust(i)**2*rhox(i)*g/del(i,1)*dt2/wspd1(i)*(wspd1(i)/wspd(i))**2
     f2(i,1) = vx(i,1)+vox(i)*ust(i)**2*rhox(i)*g/del(i,1)*dt2/wspd1(i)*(wspd1(i)/wspd(i))**2
   enddo
!
   do k = kts,kte-1
     do i = its,ite
       dtodsd = dt2/del(i,k)
       dtodsu = dt2/del(i,k+1)
       dsig   = p2d(i,k)-p2d(i,k+1)
       rdz    = 1./dza(i,k+1)
       tem1   = dsig*xkzm(i,k)*rdz
       if(pblflg(i).and.k.lt.kpbl(i))then
         dsdzu     = tem1*(-hgamu(i)/hpbl(i)-ufxpbl(i)*zfacent(i,k)/xkzm(i,k))
         dsdzv     = tem1*(-hgamv(i)/hpbl(i)-vfxpbl(i)*zfacent(i,k)/xkzm(i,k))
         f1(i,k)   = f1(i,k)+dtodsd*dsdzu
         f1(i,k+1) = ux(i,k+1)-dtodsu*dsdzu
         f2(i,k)   = f2(i,k)+dtodsd*dsdzv
         f2(i,k+1) = vx(i,k+1)-dtodsu*dsdzv
       elseif(pblflg(i).and.k.ge.kpbl(i).and.entfac(i,k).lt.4.6) then
         xkzm(i,k) = prpbl(i)*xkzh(i,k)
         xkzm(i,k) = sqrt(xkzm(i,k)*xkzml(i,k))
         xkzm(i,k) = max(xkzm(i,k),xkzom(i,k))
         xkzm(i,k) = min(xkzm(i,k),xkzmax)
         f1(i,k+1) = ux(i,k+1)
         f2(i,k+1) = vx(i,k+1)
       else
         f1(i,k+1) = ux(i,k+1)
         f2(i,k+1) = vx(i,k+1)
       endif
       tem1   = dsig*xkzm(i,k)*rdz
       dsdz2     = tem1*rdz
       au(i,k)   = -dtodsd*dsdz2
       al(i,k)   = -dtodsu*dsdz2
       ad(i,k)   = ad(i,k)-au(i,k)
       ad(i,k+1) = 1.-al(i,k)
       exch_mx(i,k+1) = xkzm(i,k)
     enddo
   enddo
!
! copies here to avoid duplicate input args for tridin
!
   do k = kts,kte
     do i = its,ite
       cu(i,k) = au(i,k)
       r1(i,k) = f1(i,k)
       r2(i,k) = f2(i,k)
     enddo
   enddo
!
!     solve tridiagonal problem for momentum
!
   call tridi1n(al,ad,cu,r1,r2,au,f1,f2,its,ite,kts,kte,1)
!
!     recover tendencies of momentum
!
   do k = kte,kts,-1
     do i = its,ite
       utend = (f1(i,k)-ux(i,k))*rdt
       vtend = (f2(i,k)-vx(i,k))*rdt
       utnp(i,k) = utend
       vtnp(i,k) = vtend
       dusfc(i) = dusfc(i) + utend*conwrc*del(i,k)
       dvsfc(i) = dvsfc(i) + vtend*conwrc*del(i,k)
     enddo
   enddo
!
! paj: ctopo2=1 if topo_wind=0 (default)
!
   do i = its,ite
     if(present(ctopo).and.present(ctopo2)) then ! mchen for NMM
       u10(i) = ctopo2(i)*u10(i)+(1-ctopo2(i))*ux(i,1)
       v10(i) = ctopo2(i)*v10(i)+(1-ctopo2(i))*vx(i,1)
     endif !mchen
   enddo
!
!---- end of vertical diffusion
!
   do i = its,ite
     kpbl1d(i) = kpbl(i)
   enddo
!
   errmsg = 'bl_ysu_run OK'
   errflg = 0
!
   end subroutine bl_ysu_run
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine tridi1n(cl,cm,cu,r1,r2,au,f1,f2,its,ite,kts,kte,nt)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer, intent(in )      ::     its,ite, kts,kte, nt
!
   real, dimension( its:ite, kts+1:kte+1 )                                   , &
         intent(in   )  ::                                                 cl
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(in   )  ::                                                 cm, &
                                                                           r1
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(in   )  ::                                                 r2
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(inout)  ::                                                 au, &
                                                                           cu, &
                                                                           f1
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(inout)  ::                                                 f2
!
   real    :: fk
   integer :: i,k,l,n,it
!
!-------------------------------------------------------------------------------
!
   l = ite
   n = kte
!
   do i = its,l
     fk = 1./cm(i,1)
     au(i,1) = fk*cu(i,1)
     f1(i,1) = fk*r1(i,1)
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1)
       f2(i,1,it) = fk*r2(i,1,it)
     enddo
   enddo
!
   do k = kts+1,n-1
     do i = its,l
       fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
       au(i,k) = fk*cu(i,k)
       f1(i,k) = fk*(r1(i,k)-cl(i,k)*f1(i,k-1))
     enddo
   enddo
!
   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k)-cl(i,k)*au(i,k-1))
         f2(i,k,it) = fk*(r2(i,k,it)-cl(i,k)*f2(i,k-1,it))
       enddo
     enddo
   enddo
!
   do i = its,l
     fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
     f1(i,n) = fk*(r1(i,n)-cl(i,n)*f1(i,n-1))
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n)-cl(i,n)*au(i,n-1))
       f2(i,n,it) = fk*(r2(i,n,it)-cl(i,n)*f2(i,n-1,it))
     enddo
   enddo
!
   do k = n-1,kts,-1
     do i = its,l
       f1(i,k) = f1(i,k)-au(i,k)*f1(i,k+1)
     enddo
   enddo
!
   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it) = f2(i,k,it)-au(i,k)*f2(i,k+1,it)
       enddo
     enddo
   enddo
!
   end subroutine tridi1n
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
   subroutine tridin_ysu(cl,cm,cu,r2,au,f2,its,ite,kts,kte,nt)
!-------------------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------------------
!
   integer, intent(in )      ::     its,ite, kts,kte, nt
!
   real, dimension( its:ite, kts+1:kte+1 )                                   , &
         intent(in   )  ::                                                 cl
!
   real, dimension( its:ite, kts:kte )                                       , &
         intent(in   )  ::                                                 au, &
                                                                           cm, &
                                                                           cu
   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(in   )  ::                                                 r2

   real, dimension( its:ite, kts:kte,nt )                                    , &
         intent(inout)  ::                                                 f2
!
   real    :: fk
   real, dimension( its:ite, kts:kte ) ::                                 aul
   integer :: i,k,l,n,it
!
!-------------------------------------------------------------------------------
!
   l = ite
   n = kte
!
   do  i = its,ite
     do k = kts,kte
       aul(i,k) = 0.
     enddo
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./cm(i,1)
       aul(i,1) = fk*cu(i,1)
       f2(i,1,it) = fk*r2(i,1,it)
     enddo
   enddo
!
   do it = 1,nt
     do k = kts+1,n-1
       do i = its,l
         fk = 1./(cm(i,k)-cl(i,k)*aul(i,k-1))
         aul(i,k) = fk*cu(i,k)
         f2(i,k,it) = fk*(r2(i,k,it)-cl(i,k)*f2(i,k-1,it))
       enddo
     enddo
   enddo
!
   do it = 1,nt
     do i = its,l
       fk = 1./(cm(i,n)-cl(i,n)*aul(i,n-1))
       f2(i,n,it) = fk*(r2(i,n,it)-cl(i,n)*f2(i,n-1,it))
     enddo
   enddo
!
   do it = 1,nt
     do k = n-1,kts,-1
       do i = its,l
         f2(i,k,it) = f2(i,k,it)-aul(i,k)*f2(i,k+1,it)
       enddo
     enddo
   enddo
!
   end subroutine tridin_ysu
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
! ==================================================================

      SUBROUTINE GET_PBLH(KTS,KTE,zi,thetav1D,qke1D,zw1D,dz1D,landsea)
! Copied from MYNN PBL

      !---------------------------------------------------------------
      !             NOTES ON THE PBLH FORMULATION
      !
      !The 1.5-theta-increase method defines PBL heights as the level at
      !which the potential temperature first exceeds the minimum potential
      !temperature within the boundary layer by 1.5 K. When applied to
      !observed temperatures, this method has been shown to produce PBL-
      !height estimates that are unbiased relative to profiler-based
      !estimates (Nielsen-Gammon et al. 2008). However, their study did not
      !include LLJs. Banta and Pichugina (2008) show that a TKE-based
      !threshold is a good estimate of the PBL height in LLJs. Therefore,
      !a hybrid definition is implemented that uses both methods, weighting
      !the TKE-method more during stable conditions (PBLH < 400 m).
      !A variable tke threshold (TKEeps) is used since no hard-wired
      !value could be found to work best in all conditions.
      !---------------------------------------------------------------

      INTEGER,INTENT(IN) :: KTS,KTE
      REAL, INTENT(OUT) :: zi
      REAL, INTENT(IN) :: landsea
      REAL, DIMENSION(KTS:KTE), INTENT(IN) :: thetav1D, qke1D, dz1D
      REAL, DIMENSION(KTS:KTE+1), INTENT(IN) :: zw1D
      !LOCAL VARS
      REAL ::  PBLH_TKE,qtke,qtkem1,wt,maxqke,TKEeps,minthv
      REAL :: delt_thv   !delta theta-v; dependent on land/sea point
      REAL, PARAMETER :: sbl_lim  = 200. !Theta-v PBL lower limit of trust (m).
      REAL, PARAMETER :: sbl_damp = 400. !Damping range for averaging with TKE-based PBLH (m).
      INTEGER :: I,J,K,kthv,ktke

      !FIND MAX TKE AND MIN THETAV IN THE LOWEST 500 M
      k = kts+1
      kthv = 1
      ktke = 1
      maxqke = 0.
      minthv = 9.E9

      DO WHILE (zw1D(k) .LE. 500.)
        qtke  =MAX(Qke1D(k),0.)   ! maximum QKE
         IF (maxqke < qtke) then
            maxqke = qtke
            ktke = k
         ENDIF
         IF (minthv > thetav1D(k)) then
             minthv = thetav1D(k)
             kthv = k
         ENDIF
         k = k+1
      ENDDO
      !TKEeps = maxtke/20. = maxqke/40.
      TKEeps = maxqke/40.
      TKEeps = MAX(TKEeps,0.025)
      TKEeps = MIN(TKEeps,0.25)

      !FIND THETAV-BASED PBLH (BEST FOR DAYTIME).
      zi=0.
      k = kthv+1
      IF((landsea-1.5).GE.0)THEN
      ! WATER
          delt_thv = 0.75
      ELSE
      ! LAND
          delt_thv = 1.5
      ENDIF

      zi=0.
      k = kthv+1
      DO WHILE (zi .EQ. 0.)
         IF (thetav1D(k) .GE. (minthv + delt_thv))THEN
            zi = zw1D(k) - dz1D(k-1)* &
              & MIN((thetav1D(k)-(minthv + delt_thv))/MAX(thetav1D(k)-thetav1D(k-1),1E-6),1.0)
        ENDIF
        k = k+1
         IF (k .EQ. kte-1) zi = zw1D(kts+1) !EXIT SAFEGUARD
      ENDDO

      !print*,"IN GET_PBLH:",thsfc,zi
      !FOR STABLE BOUNDARY LAYERS, USE TKE METHOD TO COMPLEMENT THE
      !THETAV-BASED DEFINITION (WHEN THE THETA-V BASED PBLH IS BELOW ~0.5 KM).
      !THE TANH WEIGHTING FUNCTION WILL MAKE THE TKE-BASED DEFINITION NEGLIGIBLE
      !WHEN THE THETA-V-BASED DEFINITION IS ABOVE ~1 KM.
      !FIND TKE-BASED PBLH (BEST FOR NOCTURNAL/STABLE CONDITIONS).

      PBLH_TKE=0.
      k = ktke+1
     DO WHILE (PBLH_TKE .EQ. 0.)
        !QKE CAN BE NEGATIVE (IF CKmod == 0)... MAKE TKE NON-NEGATIVE.
         qtke  =MAX(Qke1D(k)/2.,0.)      ! maximum TKE
         qtkem1=MAX(Qke1D(k-1)/2.,0.)
         IF (qtke .LE. TKEeps) THEN
               PBLH_TKE = zw1D(k) - dz1D(k-1)* &
               & MIN((TKEeps-qtke)/MAX(qtkem1-qtke, 1E-6), 1.0)
             !IN CASE OF NEAR ZERO TKE, SET PBLH = LOWEST LEVEL.
             PBLH_TKE = MAX(PBLH_TKE,zw1D(kts+1))
             !print *,"PBLH_TKE:",i,j,PBLH_TKE, Qke1D(k)/2., zw1D(kts+1)
         ENDIF
         k = k+1
         IF (k .EQ. kte-1) PBLH_TKE = zw1D(kts+1) !EXIT SAFEGUARD
      ENDDO

    !BLEND THE TWO PBLH TYPES HERE:

      wt=.5*TANH((zi - sbl_lim)/sbl_damp) + .5
      zi=PBLH_TKE*(1.-wt) + zi*wt

   END SUBROUTINE GET_PBLH
! ==================================================================

end module bl_ysu
!-------------------------------------------------------------------------------
