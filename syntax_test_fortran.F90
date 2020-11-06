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
!
   function theFunction()
!  ^^^^^^^^ keyword.declaration.function
!           ^^^^^^^^^^^ entity.name.function
   pure function theFunction()
!  ^^^^ storage.modifier
!       ^^^^^^^^ keyword.declaration.function
!                ^^^^^^^^^^^ entity.name.function 
   function theFunction(a)
!                       ^ variable.other
   function theFunction(a, bee, cesium) 
!                       ^ variable.other
!                          ^^^ variable.other
!                               ^^^^^^ variable.other
!                        ^ punctuation.separator
!
   pure function getStuff(a) result(theStuff)
!                            ^^^^^^ keyword.control
!
   end function getStuff
!  ^^^ keyword.control
!      ^^^^^^^^ keyword.declaration.function
!               ^^^^^^^^ entity.name.function
!
   module subroutine doStuff(a, b, c)
!  ^^^^^^ storage.modifier
!         ^^^^^^^^^^ keyword.declaration.function
!                    ^^^^^^^ entity.name.function 
!
      implicit none
!     ^^^^^^^^^^^^^ keyword.control
!
   end subroutine doStuff
!
   module subroutine doStuff(ace, bees, cees, & ! a comment
!                                             ^ punctuation.separator.continuation
                             dees, ees, fsss)
!                            ^^^^ variable.other
!
   module myModule
!  ^^^^^^ keyword.declaration.interface
!         ^^^^^^^^ entity.name.interface
!
   submodule (moduleName) submoduleName
!  ^^^^^^^^^ keyword.declaration.interface
!             ^^^^^^^^^^ entity.name.interface
!                         ^^^^^^^^^^^^^ entity.name.interface
!
   end submodule submoduleName
!      ^^^^^^^^^ keyword.declaration.interface
!                ^^^^^^^^^^^^^ entity.name.interface
!