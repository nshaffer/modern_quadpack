module qaws_interface
  use kinds, only: sp, dp
  use integrand_interface, only: integrand_sp, integrand_dp
  implicit none

  interface
     subroutine qaws(f, a, b, alfa, beta, integr, epsabs, epsrel, result, &
          abserr, neval, ier, limit, lenw, last, iwork, work)
       import :: sp, integrand_sp
       procedure(integrand_sp) :: f
       real(sp), intent(in) :: a
       real(sp), intent(in) :: b
       real(sp), intent(in) :: alfa
       real(sp), intent(in) :: beta
       integer, intent(in) :: integr
       real(sp), intent(in) :: epsabs
       real(sp), intent(in) :: epsrel
       real(sp), intent(out) :: result
       real(sp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: lenw
       integer, intent(out) :: last
       integer, dimension(limit), intent(inout) :: iwork
       real(sp), dimension(lenw), intent(inout) :: work
     end subroutine qaws

     subroutine dqaws(f, a, b, alfa, beta, integr, epsabs, epsrel, result, &
          abserr, neval, ier, limit, lenw, last, iwork, work)
       import :: dp, integrand_dp
       procedure(integrand_dp) :: f
       real(dp), intent(in) :: a
       real(dp), intent(in) :: b
       real(dp), intent(in) :: alfa
       real(dp), intent(in) :: beta
       integer, intent(in) :: integr
       real(dp), intent(in) :: epsabs
       real(dp), intent(in) :: epsrel
       real(dp), intent(out) :: result
       real(dp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: lenw
       integer, intent(out) :: last
       integer, dimension(limit), intent(inout) :: iwork
       real(dp), dimension(lenw), intent(inout) :: work
     end subroutine dqaws
  end interface

  integer, parameter :: QAWS_ALG_ALG = 1
  integer, parameter :: QAWS_ALGLOG_ALG = 2
  integer, parameter :: QAWS_ALG_ALGLOG = 3
  integer, parameter :: QAWS_ALGLOG_ALGLOG = 4
 
  integer, parameter :: QAWS_IER_OK = 0
  integer, parameter :: QAWS_IER_SUBDIVISIONS = 1
  integer, parameter :: QAWS_IER_ROUNDOFF = 2
  integer, parameter :: QAWS_IER_BADFUNC = 3
  integer, parameter :: QAWS_IER_INVALID = 6

end module qaws_interface
