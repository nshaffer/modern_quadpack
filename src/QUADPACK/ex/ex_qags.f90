program ex_qags
  implicit none
  
  real, parameter :: a = 0.0
  real, parameter :: b = 1.0
  real, parameter :: epsabs = 0.0
  real, parameter :: epsrel = 1.0e-3

  real :: result, abserr
  integer :: neval, ier

  integer, parameter :: limit = 100
  integer, parameter :: lenw = limit*4

  integer :: last
  integer, dimension(limit) :: iwork
  integer, dimension(lenw) :: work

  real, parameter :: ans = 2.0

  call qags(f, a, b, epsabs, epsrel, result, abserr, neval, ier, & 
       limit, lenw, last, iwork, work)
  
  print *, 'QAGS integration of 1/sqrt(x) from 0 to 1'
  print *, 'Obtained result: ', result
  print *, 'Expected result: ', ans
  print *, 'Absolute error estimate: ', abserr
  print *, 'Actual error: ', abs(result-ans)
  print *, 'Required number of integrand evaluations: ', neval
  print *, 'Exit status: ', ier

contains

  function f(x) 
    real :: f
    real, intent(in) :: x
    f = 1.0/sqrt(x)
  end function f

end program ex_qags
