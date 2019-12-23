program ex_qawo
  implicit none
  
  real, parameter :: a = 0.0
  real, parameter :: b = 1.0
  real, parameter :: omega = 10.0
  integer, parameter :: integr = 1 ! cos(omega*x)
  real, parameter :: epsabs = 0.0
  real, parameter :: epsrel = 1.0e-3

  real :: result, abserr
  integer :: neval, ier

  integer, parameter :: limit = 100
  integer, parameter :: leniw = limit*2
  integer, parameter :: maxp1 = 21
  integer, parameter :: lenw = limit*4 + maxp1*25

  integer :: last
  integer, dimension(leniw) :: iwork
  integer, dimension(lenw) :: work

  real, parameter :: ans = -0.17763920651138898591

  call qawo(f, a, b, omega, integr, epsabs, epsrel, result, abserr, neval, ier, & 
       leniw, maxp1, lenw, last, iwork, work)
  
  print *, 'QAWO integration of exp(-x)log(x)cos(10.x) from 0 to 1'
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
    f = 0.0
    if (x > 0.0) f = exp(-x)*log(x)
  end function f

end program ex_qawo
