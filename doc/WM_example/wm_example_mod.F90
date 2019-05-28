module wm_example_mod

   use ccpp_kinds, only: kind_phys

   implicit none
   public

   integer            :: ntimes_loop
   !> \section arg_table_wm_example_mod  Argument Table
   !! \htmlinclude arg_table_wm_example_host.html
   !!
   integer,parameter ::          ime=5, kme=5

   real(kind_phys),dimension(ime,kme)   :: th_phy

   public :: init_fields
   public :: compare_fields

contains

   subroutine init_fields()
      implicit none   

   end subroutine init_fields

   logical function compare_fields()

      integer         :: i,j,k
      real(kind_phys) :: sum

      compare_fields = .true.

   end function compare_fields

end module wm_example_mod
