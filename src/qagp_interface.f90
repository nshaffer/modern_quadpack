module qagp_interface
  use kinds, only: sp, dp
  use integrand_interface, only: integrand_sp, integrand_dp
  implicit none

  interface
     subroutine qagp(f, a, b, npts2, points, epsabs, epsrel, result, abserr, &
          neval, ier, leniw, lenw, last, iwork, work)
       import :: sp, integrand_sp
       procedure(integrand_sp) :: f
       real(sp), intent(in) :: a
       real(sp), intent(in) :: b
       integer, intent(in) :: npts2
       real(sp), dimension(npts2), intent(in) :: points
       real(sp), intent(in) :: epsabs
       real(sp), intent(in) :: epsrel
       real(sp), intent(out) :: result
       real(sp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: lenw
       integer, intent(out) :: last
       integer, dimension(leniw), intent(inout) :: iwork
       real(sp), dimension(lenw), intent(inout) :: work
     end subroutine qagp

     subroutine dqagp(f, a, b, npts2, points, epsabs, epsrel, result, abserr, &
          neval, ier, leniw, lenw, last, iwork, work)
       import :: dp, integrand_dp
       procedure(integrand_dp) :: f
       real(dp), intent(in) :: a
       real(dp), intent(in) :: b
       integer, intent(in) :: npts2
       real(dp), dimension(npts2), intent(in) :: points
       real(dp), intent(in) :: epsabs
       real(dp), intent(in) :: epsrel
       real(dp), intent(out) :: result
       real(dp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: lenw
       integer, intent(out) :: last
       integer, dimension(leniw), intent(inout) :: iwork
       real(dp), dimension(lenw), intent(inout) :: work
     end subroutine dqagp
  end interface
 
  integer, parameter :: QAGP_IER_OK = 0
  integer, parameter :: QAGP_IER_SUBDIVISIONS = 1
  integer, parameter :: QAGP_IER_ROUNDOFF = 2
  integer, parameter :: QAGP_IER_BADFUNC = 3
  integer, parameter :: QAGP_IER_CONVERGENCE = 4
  integer, parameter :: QAGP_IER_SINGULAR = 5
  integer, parameter :: QAGP_IER_INVALID = 6

end module qagp_interface
