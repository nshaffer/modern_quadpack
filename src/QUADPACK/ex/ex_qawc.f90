program ex_qawc
  implicit none
  
  real, parameter :: a = -1.0
  real, parameter :: b = 1.0
  real, parameter :: c = 0.5
  real, parameter :: epsabs = 0.0
  real, parameter :: epsrel = 1.0e-3

  real :: result, abserr
  integer :: neval, ier

  integer, parameter :: limit = 100
  integer, parameter :: lenw = limit*4

  integer :: last
  integer, dimension(limit) :: iwork
  integer, dimension(lenw) :: work

  real, parameter :: ans = -628.461728506562366229

  call qawc(f, a, b, c, epsabs, epsrel, result, abserr, neval, ier, & 
       limit, lenw, last, iwork, work)
  
  print *, 'QAWC integration of 1/(x^2 + 10^-4) 1/(x-0.5) from -1 to 1'
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
    f = 1.0/(x*x + 1.0e-4)
  end function f

end program ex_qawc
