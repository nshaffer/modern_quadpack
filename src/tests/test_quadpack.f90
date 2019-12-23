program test_quadpack
  use quadpack_test_cases
  implicit none

  call test_qng
  call test_qag
  call test_qags
  call test_qagp
  call test_qagi
  call test_qawo
  call test_qawf
  call test_qaws
  call test_qawc

end program test_quadpack


