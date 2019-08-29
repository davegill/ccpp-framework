module mpas_host

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public mpas_sub

CONTAINS

  !> \section arg_table_mpas_sub  Argument Table
  !! \htmlinclude arg_table_mpas_sub.html
  !!
  subroutine mpas_sub()

    use mpas_ccpp_cap, only: mpas_ccpp_physics_initialize
    use mpas_ccpp_cap, only: mpas_ccpp_physics_timestep_initial
    use mpas_ccpp_cap, only: mpas_ccpp_physics_run
    use mpas_ccpp_cap, only: mpas_ccpp_physics_timestep_final
    use mpas_ccpp_cap, only: mpas_ccpp_physics_finalize
    use mpas_ccpp_cap, only: ccpp_physics_suite_list
    use mpas_ccpp_cap, only: ccpp_physics_suite_part_list

    ! dimensions
    use mpas_mod,     only: nCellSolveStart,nCellSolveEnd,nVertStart,nVertEnd,nConstituents

    integer                         :: its, ite, kts, kte, j
    character(len=128), allocatable :: part_names(:)
    character(len=512)              :: errmsg
    integer                         :: errflg

    its = nCellSolveStart
    ite = nCellSolveEnd
    kts = nVertStart
    kte = nVertEnd
    nmix = nConstituents

    call mpas_ccpp_physics_initialize('mpas_suite', errmsg, errflg)

    call mpas_ccpp_physics_timestep_initial('mpas_suite', errmsg, errflg)

    j = 1
    call mpas_ccpp_physics_run('mpas_suite', 'physics', j, its, ite, kts, kte, nmix, errmsg, errflg)
    call ccpp_physics_suite_part_list('mpas_suite', part_names, errmsg, errflg)

    call mpas_ccpp_physics_timestep_final('mpas_suite', errmsg, errflg)

    call mpas_ccpp_physics_finalize('mpas_suite', errmsg, errflg)

  end subroutine mpas_sub

end module mpas_host

program mpas
  use mpas_host, only: mpas_sub
  call mpas_sub()
end program mpas
