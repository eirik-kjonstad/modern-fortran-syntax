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
   if (a == b) then
!  ^^ keyword.control.conditional
!              ^^^^ keyword.control.conditional
   endif
!  ^^^^ keyword.control.conditional
!
   do while (a .ne. c)
!  ^^ keyword.control
!     ^^^^^ keyword.control
!
   enddo

    real(dp), intent(in) :: myReal
!   ^^^^ storage.type
!             ^^^^^^ storage.modifier
!                    ^^ keyword.other

    class(myClass), allocatable :: myClassInstance
!   ^^^^^ storage.type.class
!         ^^^^^^^ entity.name.class
!                   ^^^^^^^^^^^ storage.modifier
!
    type :: myClass
!   ^^^^ storage.type.class
!        ^^ keyword.separator
!           ^^^^^^^ entity.name.class
    end type myClass

!
   "This is a simple string"
!
   "This is a simple string,  & 
   & but 'with' continuation, &
   & and more continuation!" 
!
   'This is a simple string'
!  ^^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.single
   &
!  ^ punctuation.separator.continuation
!
   'This is a simple string,  & 
   & but "with" continuation, &
   & and more continuation!' 
!  ^ punctuation.separator.continuation
!
   'string' // ' contatenation!'
!           ^^ keyword.operator.arithmetic 

