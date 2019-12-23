program ex_qng
  implicit none
  
  real, parameter :: a = 0.0
  real, parameter :: b = 1.0
  real, parameter :: epsabs = 0.0
  real, parameter :: epsrel = 1.0e-3

  real :: result, abserr
  integer :: neval, ier

  real, parameter :: ans = 1.270724139833620

  call qng(f, a, b, epsabs, epsrel, result, abserr, neval, ier)
  
  print *, 'QNG integration of exp(x)/(x^2+1) from 0 to 1'
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
    f = exp(x)/(x**2 + 1.0)
  end function f

end program ex_qng
