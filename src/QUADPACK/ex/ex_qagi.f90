program ex_qagi
  implicit none
  
  real, parameter :: boun = 0.0
  integer, parameter :: inf = 1 
  real, parameter :: epsabs = 0.0
  real, parameter :: epsrel = 1.0e-3

  real :: result, abserr
  integer :: neval, ier

  integer, parameter :: limit = 100
  integer, parameter :: lenw = limit*4

  integer :: last
  integer, dimension(limit) :: iwork
  integer, dimension(lenw) :: work

  real, parameter :: ans = 3.07957178

  call qagi(f, boun, inf, epsabs, epsrel, result, abserr, neval, ier, & 
       limit, lenw, last, iwork, work)
  
  print *, 'QAGI integration of sqrt(x)log(x)/((x+1)(x+2)) from 0 to +inf'
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
    f = sqrt(x)*log(x) / ((x + 1.0)*(x + 2.0))
  end function f

end program ex_qagi
