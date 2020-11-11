! SYNTAX TEST "modern-fortran.sublime-syntax"
!
   a == b .and. c
!    ^^ keyword.operator.logical
!         ^^^^^ keyword.operator.word
!
   a => b
!    ^^ keyword.operator
!
   integer(kind=8), dimension(:,:), allocatable :: myInt
!  ^^^^^^^ storage.type
!                   ^^^^^^^^^ storage.modifier
!                                   ^^^^^^^^^^^ storage.modifier
   if (a == b) then
!  ^^ keyword.control.conditional
!              ^^^^ keyword.control.conditional
   endif
!  ^^^^ keyword.control.conditional
!
   do while (a .ne. b) ! a side-comment
!  ^^ keyword.control
!     ^^^^^ keyword.control
!                        ^^^^^^^^^^^^^^ comment.line
!
   enddo

   real(dp), intent(in) :: myReal ! a side-comment
!  ^^^^ storage.type
!       ^^ variable.other
!            ^^^^^^ storage.modifier
!                   ^^ keyword.other
!                          ^^^^^^ variable.other
!                                   ^^^^^^^^^^^^^^ comment.line

   class(myClass), allocatable :: myClassInstance
!  ^^^^^ storage.type.class
!        ^^^^^^^ entity.name.class
!                  ^^^^^^^^^^^ storage.modifier
!
   type :: myClass1
!  ^^^^ storage.type.class
!       ^^ keyword.separator
!          ^^^^^^^^ entity.name.class
!
!     ...
!
   contains
!
   end type myClass1
!
!
   type, abstract :: myClass2
!  ^^^^ storage.type.class
!        ^^^^^^^^ storage.modifier
!                 ^^ keyword.separator
!                    ^^^^^^^ entity.name.class

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
!                                   ^^^^^^^^ variable.other
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
   8
!  ^ constant.numeric
!
   123
!  ^^^ constant.numeric
!
   1.0d-12
!  ^^^^^^^ constant.numeric
!
   a = minval(b)
!      ^^^^^^ variable.function
!
!  type casting versus variable declaration
   real(8) :: aRealNumber
!  ^^^^ storage.type
!
   interface myInterface
!  ^^^^^^^^^ keyword.declaration.interface 
!            ^^^^^^^^^^^ entity.name.interface
!
   end interface myInterface 
!  ^^^ keyword.control
!      ^^^^^^^^^ keyword.declaration.interface
!                ^^^^^^^^^^^ entity.name.interface
!
   abstract interface
!  ^^^^^^^^ storage.modifier
!           ^^^^^^^^^ keyword.declaration.interface
!
   end interface 
!  ^^^ keyword.control
!      ^^^^^^^^^ keyword.declaration.interface
!
   if (thing == object%getThing('string', .true.)) call object%doStuff()
!               ^^^^^^ storage.type.class 
!                                       ^ punctuation.separator
!                      ^^^^^^^^ variable.function
!                                ^^^^^^ string.quoted.single
!                                         ^^^^^^ constant.language
!
   use moduleName, only: moduleRoutine, moduleObject, &
!                        ^^^^^^^^^^^^^ variable.other
!                                                     ^ punctuation.separator.continuation
                           otherModuleRoutine, anotherModuleObject
!                          ^^^^^^^^^^^^^^^^^^ variable.other
!                                            ^ punctuation.separator
   evenNumber = 0
!
   do I = 1, 10
!  ^^ keyword.control
!     ^ variable.other
!         ^ constant.numeric
!          ^ punctuation.separator
!            ^^ constant.numeric
!
      evenNumber = evenNumber + 2*I
!     ^^^^^^^^^^ variable.other
!                  ^^^^^^^^^^ variable.other
!                                 ^ variable.other
!                               ^ constant.numeric
   enddo
!  ^^^^^ keyword.control
!

   thing = object%inObject%inInObject(I)%get_thing(J)
!  ^^^^^ variable.other
!          ^^^^^^ storage.type.class
!                 ^^^^^^^^ storage.type.class
!                          ^^^^^^^^^^ storage.type.class
!                                        ^^^^^^^^^ variable.function
!
   aRealNumber = real(anInteger) ! ruined this one; have to think to fix it
!                ^^^^ variable.function
!                     ^^^^^^^^^ variable.other
!  simple function call
   call mySubroutine(a, b, c)
!  ^^^^ keyword.control
!       ^^^^^^^^^^^^ variable.function
!
   myVar = object%objectVariable
!  ^^^^^ variable.other
!          ^^^^^^ storage.type.class 
!                ^ punctuation.accessor
!                 ^^^^^^^^^^^^^^ variable.other
!
   myVar = object%objectVariable%getStuff(a, b)
!          ^^^^^^ storage.type.class 
!                 ^^^^^^^^^^^^^^ storage.type.class 
!                                ^^^^^^^^ variable.function
!
   myVar = object%objectFunction(a, b, c, otherObject%variable)
!          ^^^^^^ storage.type.class 
!                 ^^^^^^^^^^^^^^ variable.function
!                                         ^^^^^^^^^^^ storage.type.class 
!                                                     ^^^^^^^^ variable.other
!
   call object%calculateStuff()
!       ^^^^^^ storage.type.class 
!              ^^^^^^^^^^^^^^ variable.function
!
   if (a == b) call mySubroutine(a, b, c)
!              ^^^^ keyword.control
!                   ^^^^^^^^^^^^ variable.function
!
   call object%objectFunction(anotherObject%variable)
!                             ^^^^^^^^^^^^^ storage.type.class 
!                                           ^^^^^^^^ variable.other
!
   call object%objectFunction(anotherObject%myFunction())
!                                           ^^^^^^^^^^ variable.function
!
   if (present(myArgument)) call doThing(myArgument)
!  ^^ keyword.control
!      ^^^^^^^ variable.function
!                                ^^^^^^^ variable.function
!
   real(dp), dimension(wf%n_ao**2, wf%n_densities), intent(in), optional :: prev_ao_density
!  ^^^^ storage.type
!            ^^^^^^^^^ storage.modifier
!                                                   ^^^^^^ storage.modifier
!                      ^^ storage.type.class
!                        ^ punctuation.accessor
!                         ^^^ variable.other
!
#ifdef myVar
!      ^^^^^ variable.other
   integer, parameter :: p = 1
#else
   integer, parameter :: p = 2
#endif
!<- support.function
!^^^^^ support.function

#include "someFile.F08"
!         ^^^^^^^^^^^^ string.quoted.single

!$omp parallel do private(I)
!^ support.function
! ^^^ support.function
   do I = 1, 10
!
      f(I) = someThing(I)
!
   enddo
!$omp end parallel do 

   type1 = "hello!" ! should understand that 'type1' is a variable 
!  ^^^^^ variable.other
!           ^^^^^^ string.quoted.single
!                     ^^^^^^ comment.line
!
   used_diag(j) = 5 ! should not recognize "use" as keyword
!  ^^^^^^^^^ variable.function
!
   real(dp) function myFunction(a, b, c) result(someResult)
!  ^^^^ storage.type
!       ^^ variable.other
!                                               ^^^^^^^^^^ variable.other
!           ^^^^^^^^ keyword.declaration.function
!                    ^^^^^^^^^^ entity.name.function
!
program myProgram
!<-^^^^ keyword.other
!       ^^^^^^^^^ entity.name
!
!  Program contents
!
end program myProgram
!   ^^^^^^^ keyword.other
!           ^^^^^^^^^ entity.name
