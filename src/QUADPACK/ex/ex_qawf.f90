program ex_qawf
  implicit none
  
  real, parameter :: a = 0.0
  real, parameter :: omega = 8.0
  integer, parameter :: integr = 2 ! sin(omega*x)
  real, parameter :: epsabs = 1.0e-3

  real :: result, abserr
  integer :: neval, ier
  
  integer, parameter :: limlst = 50
  integer, parameter :: limit = 100
  integer, parameter :: leniw = limit*2 + limlst
  integer, parameter :: maxp1 = 21
  integer, parameter :: lenw = leniw*2 + maxp1*25

  integer :: lst
  integer, dimension(leniw) :: iwork
  integer, dimension(lenw) :: work

  real, parameter :: ans = 1.422552162575912242782619650
  
  call qawf(f, a, omega, integr, epsabs, result, abserr, neval, ier, & 
       limlst, lst, leniw, maxp1, lenw, iwork, work)
  
  print *, 'QAWF integration of sin(50.x)sin(8.x)/(x.sqrt(x)) from 0 to oo'
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
    if (x > 0.0) f = sin(50.0*x)/(x*sqrt(x))
  end function f

end program ex_qawf
