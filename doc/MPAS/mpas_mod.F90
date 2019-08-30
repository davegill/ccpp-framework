module mpas_mod

   use ccpp_kinds, only: kind_phys

   implicit none
   public

   !> \section arg_table_mpas_mod  Argument Table
   !! \htmlinclude arg_table_mpas_mod.html
   !!
  
   !  dimensions, as if from MPAS somehow
   integer, parameter :: nVertLevels = 15
   integer, parameter :: nVertLevelsP1 = 16
   integer, parameter :: nCells = 20

   !  some derived dims
   integer, parameter :: nCellSolveStart = 1
   integer, parameter :: nCellSolveEnd = nCells
   integer, parameter :: nVertStart = 1
   integer, parameter :: nVertEnd = nVertLevels
   integer, parameter :: nVertStartP1 = 1
   integer, parameter :: nVertEndP1 = nVertLevelsP1
   integer, parameter :: nConstituents = 5

   ! fields hvn
   real(kind_phys),dimension(nCellSolveStart:nCellSolveEnd,nVertStart:nVertEnd,nConstituents) :: &
                                        qmix, &
                                        qmixtnp

   ! fields hv interface
   real(kind_phys),dimension(nCellSolveStart:nCellSolveEnd,nVertStartP1:nVertEndP1) :: &
                                        p2di

   ! fields hv interface
   real(kind_phys),dimension(nCellSolveStart:nCellSolveEnd,nVertStart:nVertEnd) :: &
                                        ux, & 
                                        vx, & 
                                        tx, & 
                                        qvx, & 
                                        qcx, & 
                                        qix, & 
                                        p2d, & 
                                        pi2d, & 
                                        utnp, & 
                                        vtnp, & 
                                        ttnp, & 
                                        qvtnp, & 
                                        qctnp, & 
                                        qitnp, & 
                                        dz8w2d, & 
                                        exch_hx, & 
                                        exch_mx, & 
                                        rthraten

   ! fields h
   real(kind_phys),dimension(nCellSolveStart:nCellSolveEnd)         :: &
                                        psfcpa, &
                                        znt, &
                                        ust, &
                                        hpbl, &
                                        psim, &
                                        psih, &
                                        xland, &
                                        hfx, &
                                        qfx, &
                                        wspd, &
                                        br, &
                                        wstar, &
                                        delta, &
                                        u10, &
                                        v10, &
                                        uox, &
                                        vox, &
                                        ctopo, &
                                        ctopo2

   ! fields
   real(kind_phys)                            :: &
                                        cp, &
                                        g, &
                                        rovcp, &
                                        rd, &
                                        rovg, &
                                        ep1, &
                                        ep2, &
                                        karman, &
                                        xlv, &
                                        rv, &
                                        dt

   ! fields h
   integer,dimension(nCellSolveStart:nCellSolveEnd)                 :: &
                                        kpbl1d

   ! fields
   integer                                    :: &
                                        ysu_topdown_pblmix

   ! fields
   logical                                    :: &
                                        f_qc, &
                                        f_qi

contains

end module mpas_mod


