module qag_interface
  use kinds, only: sp, dp
  use integrand_interface, only: integrand_sp, integrand_dp
  implicit none

  interface
     subroutine qag(f, a, b, epsabs, epsrel, key, result, abserr, neval, ier, &
          limit, lenw, last, iwork, work)
       import :: sp, integrand_sp
       procedure(integrand_sp) :: f
       real(sp), intent(in) :: a
       real(sp), intent(in) :: b
       real(sp), intent(in) :: epsabs
       real(sp), intent(in) :: epsrel
       integer, intent(in) :: key
       real(sp), intent(out) :: result
       real(sp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: limit
       integer, intent(in) :: lenw
       integer, intent(out) :: last
       integer, dimension(limit), intent(inout) :: iwork
       real(sp), dimension(lenw), intent(inout) :: work
     end subroutine qag

     subroutine dqag(f, a, b, epsabs, epsrel, key, result, abserr, neval, ier, &
          limit, lenw, last, iwork, work)
       import :: dp, integrand_dp
       procedure(integrand_dp) :: f
       real(dp), intent(in) :: a
       real(dp), intent(in) :: b
       real(dp), intent(in) :: epsabs
       real(dp), intent(in) :: epsrel
       integer, intent(in) :: key
       real(dp), intent(out) :: result
       real(dp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: limit
       integer, intent(in) :: lenw
       integer, intent(out) :: last
       integer, dimension(limit), intent(inout) :: iwork
       real(dp), dimension(lenw), intent(inout) :: work
     end subroutine dqag
  end interface

  integer, parameter :: QAG_KEY_7_15 = 1
  integer, parameter :: QAG_KEY_10_21 = 2
  integer, parameter :: QAG_KEY_15_31 = 3
  integer, parameter :: QAG_KEY_20_41 = 4
  integer, parameter :: QAG_KEY_25_51 = 5
  integer, parameter :: QAG_KEY_30_61 = 6
 
  integer, parameter :: QAG_IER_OK = 0
  integer, parameter :: QAG_IER_SUBDIVISIONS = 1
  integer, parameter :: QAG_IER_ROUNDOFF = 2
  integer, parameter :: QAG_IER_BADFUNC = 3
  integer, parameter :: QAG_IER_INVALID = 6

end module qag_interface
