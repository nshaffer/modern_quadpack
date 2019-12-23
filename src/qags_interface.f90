module qags_interface
  use kinds, only: sp, dp
  use integrand_interface, only: integrand_sp, integrand_dp
  implicit none

  interface
     subroutine qags(f, a, b, epsabs, epsrel, result, abserr, neval, ier, &
          limit, lenw, last, iwork, work)
       import :: sp, integrand_sp
       procedure(integrand_sp) :: f
       real(sp), intent(in) :: a
       real(sp), intent(in) :: b
       real(sp), intent(in) :: epsabs
       real(sp), intent(in) :: epsrel
       real(sp), intent(out) :: result
       real(sp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: limit
       integer, intent(in) :: lenw
       integer, intent(out) :: last
       integer, dimension(limit), intent(out) :: iwork
       real(sp), dimension(lenw), intent(out) :: work
     end subroutine qags

     subroutine dqags(f, a, b, epsabs, epsrel, result, abserr, neval, ier, &
          limit, lenw, last, iwork, work)
       import :: dp, integrand_dp
       procedure(integrand_dp) :: f
       real(dp), intent(in) :: a
       real(dp), intent(in) :: b
       real(dp), intent(in) :: epsabs
       real(dp), intent(in) :: epsrel
       real(dp), intent(out) :: result
       real(dp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: limit
       integer, intent(in) :: lenw
       integer, intent(out) :: last
       integer, dimension(limit), intent(out) :: iwork
       real(dp), dimension(lenw), intent(out) :: work
     end subroutine dqags
  end interface
 
  integer, parameter :: QAGS_IER_OK = 0
  integer, parameter :: QAGS_IER_SUBDIVISIONS = 1
  integer, parameter :: QAGS_IER_ROUNDOFF = 2
  integer, parameter :: QAGS_IER_BADFUNC = 3
  integer, parameter :: QAGS_IER_CONVERGENCE = 4
  integer, parameter :: QAGS_IER_SINGULAR = 5
  integer, parameter :: QAGS_IER_INVALID = 6

end module qags_interface
