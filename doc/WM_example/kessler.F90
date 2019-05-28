!WRF:MODEL_LAYER:PHYSICS
!

MODULE kessler

  USE ccpp_kinds, ONLY: kind_phys

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: kessler_init
  PUBLIC :: kessler_run
  PUBLIC :: kessler_finalize

CONTAINS
!> \section arg_table_kessler_run  Argument Table
!! \htmlinclude arg_table_kessler_run.html
!!
   SUBROUTINE kessler_run( ncol, ilev, t              & ! memory dims
                      ,errmsg, errflg                           )
!----------------------------------------------------------------
   IMPLICIT NONE
!----------------------------------------------------------------
   !  taken from the COMMAS code - WCS 10 May 1999.
   !  converted from FORTRAN 77 to 90, tiled, WCS 10 May 1999.
!----------------------------------------------------------------
   REAL    , PARAMETER ::  c1 = .001 
   REAL    , PARAMETER ::  c2 = .001 
   REAL    , PARAMETER ::  c3 = 2.2 
   REAL    , PARAMETER ::  c4 = .875 
   REAL    , PARAMETER ::  fudge = 1.0 
   REAL    , PARAMETER ::  mxfall = 10.0 
!----------------------------------------------------------------
   INTEGER,      INTENT(IN   )    :: ncol, ilev

   REAL(kind=kind_phys), DIMENSION( ncol, ilev ),             &
         INTENT(INOUT) ::                                       &
                                                            t

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg
  END SUBROUTINE kessler_run

!> \section arg_table_kessler_init  Argument Table
!! \htmlinclude arg_table_kessler_init.html
!!
  subroutine kessler_init (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine kessler_init

!> \section arg_table_kessler_finalize  Argument Table
!! \htmlinclude arg_table_kessler_finalize.html
!!
  subroutine kessler_finalize (errmsg, errflg)

    character(len=512),      intent(out)   :: errmsg
    integer,                 intent(out)   :: errflg

    ! This routine currently does nothing

    errmsg = ''
    errflg = 0

  end subroutine kessler_finalize



END MODULE kessler
