module integrand_interface
  use kinds, only: sp, dp
  implicit none

  abstract interface 
     function integrand_sp(x) result(y)
       import :: sp
       real(sp), intent(in) :: x
       real(sp) :: y
     end function integrand_sp
  end interface

  abstract interface 
     function integrand_dp(x) result(y)
       import :: dp
       real(dp), intent(in) :: x
       real(dp) :: y
     end function integrand_dp
  end interface

end module integrand_interface
