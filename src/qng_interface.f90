module qng_interface
  use kinds, only: sp, dp
  use integrand_interface, only: integrand_sp, integrand_dp
  implicit none

  interface
    subroutine qng(f, a, b, epsabs, epsrel, result, abserr, neval, ier)
       import :: sp, integrand_sp
       procedure(integrand_sp) :: f
       real(sp), intent(in) :: a, b, epsabs, epsrel
       real(sp), intent(out) :: result, abserr
       integer, intent(out) :: neval, ier
     end subroutine qng
 
     subroutine dqng(f, a, b, epsabs, epsrel, result, abserr, neval, ier)
       import :: dp, integrand_dp
       procedure(integrand_dp) :: f
       real(dp), intent(in) :: a, b, epsabs, epsrel
       real(dp), intent(out) :: result, abserr
       integer, intent(out) :: neval, ier
     end subroutine dqng
  end interface
 
  integer, parameter :: QNG_IER_OK = 0
  integer, parameter :: QNG_IER_INACCURATE = 1
  integer, parameter :: QNG_IER_INVALID = 6

end module qng_interface
