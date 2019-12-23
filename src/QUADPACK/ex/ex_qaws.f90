program ex_qaws
  implicit none
  
  real, parameter :: a = 0.0
  real, parameter :: b = 1.0
  real, parameter :: alfa = -0.5
  real, parameter :: beta = -0.5
  integer, parameter :: integr = 1
  real, parameter :: epsabs = 0.0
  real, parameter :: epsrel = 1.0e-3

  real :: result, abserr
  integer :: neval, ier

  integer, parameter :: limit = 100
  integer, parameter :: lenw = limit*4

  integer :: last
  integer, dimension(limit) :: iwork
  integer, dimension(lenw) :: work

  real, parameter :: ans = 0.535019056922365344126359

  call qaws(f, a, b, alfa, beta, integr, epsabs, epsrel, result, &
       abserr, neval, ier, limit, lenw, last, iwork, work)
  
  print *, 'QAWS integration of sin(10.x)/sqrt(x.(1-x)) from 0 to 1'
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
    f = sin(10.0*x)
  end function f

end program ex_qaws
