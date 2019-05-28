module wm_example_host

  use ccpp_kinds, only: kind_phys

  implicit none
  private

  public wm_example_sub

CONTAINS

  !> \section arg_table_wm_example_sub  Argument Table
  !! \htmlinclude arg_table_wm_example_sub.html
  !!
  subroutine wm_example_sub()

    use kess_test_ccpp_cap, only: kess_test_ccpp_physics_initialize
    use kess_test_ccpp_cap, only: kess_test_ccpp_physics_timestep_initial
    use kess_test_ccpp_cap, only: kess_test_ccpp_physics_run
    use kess_test_ccpp_cap, only: kess_test_ccpp_physics_timestep_final
    use kess_test_ccpp_cap, only: kess_test_ccpp_physics_finalize
    use kess_test_ccpp_cap, only: ccpp_physics_suite_list
    use kess_test_ccpp_cap, only: ccpp_physics_suite_part_list
    use wm_example_mod,      only: init_fields, compare_fields


    integer                         :: index
    character(len=128), allocatable :: part_names(:)
    character(len=512)              :: errmsg
    integer                         :: errflg

    ! Initialize our 'data'
    call init_fields()

    ! Use the suite information to setup the run
    call kess_test_ccpp_physics_initialize('wm_example_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    ! Initialize the timestep
    call kess_test_ccpp_physics_timestep_initial('wm_example_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      stop
    end if

    call kess_test_ccpp_physics_run('wm_example_suite', 'physics2', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      call ccpp_physics_suite_part_list('wm_example_suite', part_names, errmsg, errflg)
      write(6, *) 'Available suite parts are:'
      do index = 1, size(part_names)
        write(6, *) trim(part_names(index))
      end do
      stop
    end if

    call kess_test_ccpp_physics_timestep_final('wm_example_suite', errmsg, errflg)

    call kess_test_ccpp_physics_finalize('wm_example_suite', errmsg, errflg)
    if (errflg /= 0) then
      write(6, *) trim(errmsg)
      write(6,'(a)') 'An error occurred in ccpp_timestep_final, Exiting...'
      stop
    end if

    if (compare_fields()) then
      write(6, *) 'Answers are correct!'
    else
      write(6, *) 'Answers are not correct!'
    end if

  end subroutine wm_example_sub

end module wm_example_host

program wm_example
  use wm_example_host, only: wm_example_sub
  call wm_example_sub()
end program wm_example
