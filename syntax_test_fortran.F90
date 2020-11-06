! SYNTAX TEST "modern-fortran.sublime-syntax"
!
  a == b .and. c
!   ^^ keyword.operator.logical
!        ^^^^^ keyword.operator.word
!
   a => b
!    ^^ keyword.operator
!
    integer(kind=8), dimension(:,:), allocatable :: myInt
!   ^^^^^^^ storage.type
!                    ^^^^^^^^^ storage.modifier
!                                    ^^^^^^^^^^^ storage.modifier

    real(dp), intent(in) :: myReal
!   ^^^^ storage.type
!             ^^^^^^ storage.modifier
!                    ^^ keyword.other