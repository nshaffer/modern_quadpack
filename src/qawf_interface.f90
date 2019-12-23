module qawf_interface
  use kinds, only: sp, dp
  use integrand_interface, only: integrand_sp, integrand_dp
  implicit none

  interface
     subroutine qawf(f, a, omega, integr, epsabs, result, abserr, neval, ier, &
          limlst, lst, leniw, maxp1, lenw, iwork, work)
       import :: sp, integrand_sp
       procedure(integrand_sp) :: f
       real(sp), intent(in) :: a
       real(sp), intent(in) :: omega
       integer, intent(in) :: integr
       real(sp), intent(in) :: epsabs
       real(sp), intent(out) :: result
       real(sp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: limlst
       integer, intent(out) :: lst
       integer, intent(in) :: leniw
       integer, intent(in) :: maxp1
       integer, intent(in) :: lenw
       integer, dimension(leniw), intent(out) :: iwork
       real(sp), dimension(lenw), intent(out) :: work
     end subroutine qawf

     subroutine dqawf(f, a, omega, integr, epsabs, result, abserr, neval, ier, &
          limlst, lst, leniw, maxp1, lenw, iwork, work)
       import :: dp, integrand_dp
       procedure(integrand_dp) :: f
       real(dp), intent(in) :: a
       real(dp), intent(in) :: omega
       integer, intent(in) :: integr
       real(dp), intent(in) :: epsabs
       real(dp), intent(out) :: result
       real(dp), intent(out) :: abserr
       integer, intent(out) :: neval
       integer, intent(out) :: ier
       integer, intent(in) :: limlst
       integer, intent(out) :: lst
       integer, intent(in) :: leniw
       integer, intent(in) :: maxp1
       integer, intent(in) :: lenw
       integer, dimension(leniw), intent(out) :: iwork
       real(dp), dimension(lenw), intent(out) :: work
     end subroutine dqawf
  end interface

  integer, parameter :: QAWF_COS = 1
  integer, parameter :: QAWF_SIN = 2
 
  integer, parameter :: QAWF_IER_OK = 0
  integer, parameter :: QAWF_IER_CYCLES = 1
  integer, parameter :: QAWF_IER_CONVERGENCE = 4
  integer, parameter :: QAWF_IER_SINGULAR = 5
  integer, parameter :: QAWF_IER_INVALID = 6
  integer, parameter :: QAWF_IER_BADFUNC = 7

end module qawf_interface
