program ex_qagp
  implicit none
  
  real, parameter :: a = 0.0
  real, parameter :: b = 1.0
  real, parameter :: epsabs = 0.0
  real, parameter :: epsrel = 1.0e-3
  integer, parameter :: npts2 = 4
  real, dimension(npts2-2), parameter :: points = [1.0/7.0, 2.0/3.0]

  real :: result, abserr
  integer :: neval, ier

  integer, parameter :: limit = 100
  integer, parameter :: leniw = limit*2 + npts2
  integer, parameter :: lenw = limit*4 + npts2

  integer :: last
  integer, dimension(leniw) :: iwork
  integer, dimension(lenw) :: work

  real, parameter :: ans = 4.25368769

  call qagp(f, a, b, npts2, points, epsabs, epsrel, result, abserr, neval, ier, & 
       leniw, lenw, last, iwork, work)
  
  print *, 'QAGP integration of |x-1/7|^(-1/4) |x-2/3|^(-11/20) from 0 to 1'
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
    f = abs(x-points(1))**(-0.25) * abs(x-points(2))**(-0.55)
  end function f

end program ex_qagp
