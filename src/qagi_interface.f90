module qagi_interface
  use kinds, only: sp, dp
  use integrand_interface, only: integrand_sp, integrand_dp
  implicit none

  interface
     subroutine qagi(f, bound, inf, epsabs, epsrel, result, abserr, neval, &
          ier, limit, lenw, last, iwork, work)
       import :: sp, integrand_sp
       procedure(integrand_sp) :: f
       real(sp), intent(in) :: bound
       integer, intent(in) :: inf
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
     end subroutine qagi

     subroutine dqagi(f, bound, inf, epsabs, epsrel, result, abserr, neval, &
          ier, limit, lenw, last, iwork, work)
       import :: dp, integrand_dp
       procedure(integrand_dp) :: f
       real(dp), intent(in) :: bound
       integer, intent(in) :: inf
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
     end subroutine dqagi
  end interface

  integer, parameter :: QAGI_INF_UPPER = 1
  integer, parameter :: QAGI_INF_LOWER = -1
  integer, parameter :: QAGI_INF_BOTH = 2
 
  integer, parameter :: QAGI_IER_OK = 0
  integer, parameter :: QAGI_IER_SUBDIVISIONS = 1
  integer, parameter :: QAGI_IER_ROUNDOFF = 2
  integer, parameter :: QAGI_IER_BADFUNC = 3
  integer, parameter :: QAGI_IER_CONVERGENCE = 4
  integer, parameter :: QAGI_IER_SINGULAR = 5
  integer, parameter :: QAGI_IER_INVALID = 6

end module qagi_interface
