module qawo_interface
  use kinds, only: sp, dp
  use integrand_interface, only: integrand_sp, integrand_dp
  implicit none

  interface
     subroutine qawo(f, a, b, omega, integr, epsabs, epsrel, result, abserr, &
          neval, ier, leniw, maxp1, lenw, last, iwork, work)
       import :: sp, integrand_sp
       procedure(integrand_sp) :: f
       real(sp), intent(in) :: a
       real(sp), intent(in) :: b
       real(sp), intent(in) :: omega
       integer, intent(in) :: integr
       real(sp), intent(in) :: epsabs
       real(sp), intent(in) :: epsrel
       real(sp), intent(out) :: result
       real(sp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: leniw
       integer, intent(in) :: maxp1
       integer, intent(in) :: lenw
       integer, intent(out) :: last
       integer, dimension(leniw), intent(inout) :: iwork
       real(sp), dimension(lenw), intent(inout) :: work
     end subroutine qawo

     subroutine dqawo(f, a, b, omega, integr, epsabs, epsrel, result, abserr, &
          neval, ier, leniw, maxp1, lenw, last, iwork, work)
       import :: dp, integrand_dp
       procedure(integrand_dp) :: f
       real(dp), intent(in) :: a
       real(dp), intent(in) :: b
       real(dp), intent(in) :: omega
       integer, intent(in) :: integr
       real(dp), intent(in) :: epsabs
       real(dp), intent(in) :: epsrel
       real(dp), intent(out) :: result
       real(dp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: leniw
       integer, intent(in) :: maxp1
       integer, intent(in) :: lenw
       integer, intent(out) :: last
       integer, dimension(leniw), intent(inout) :: iwork
       real(dp), dimension(lenw), intent(inout) :: work
     end subroutine dqawo
  end interface

  integer, parameter :: QAWO_COS = 1
  integer, parameter :: QAWO_SIN = 2
 
  integer, parameter :: QAWO_IER_OK = 0
  integer, parameter :: QAWO_IER_SUBDIVISIONS = 1
  integer, parameter :: QAWO_IER_ROUNDOFF = 2
  integer, parameter :: QAWO_IER_BADFUNC = 3
  integer, parameter :: QAWO_IER_CONVERGENCE = 4
  integer, parameter :: QAWO_IER_SINGULAR = 5
  integer, parameter :: QAWO_IER_INVALID = 6

end module qawo_interface
