! SYNTAX TEST "modern-fortran.sublime-syntax"
!
   a == b .and. c
!    ^^ keyword.operator.logical
!         ^^^^^ keyword.operator.word
!
   a => b
!    ^^ keyword.operator.points-to.fortran
!
   a = b 
!    ^ keyword.operator.assignment.fortran
!
   integer(kind=8), dimension(:,:), allocatable :: myInt
!  ^^^^^^^ storage.type
!                   ^^^^^^^^^ storage.modifier.fortran
!                                   ^^^^^^^^^^^ storage.modifier.fortran
   if (a == b) then
!  ^^ keyword.control.fortran
!              ^^^^ keyword.control.fortran
   elseif (a == c) then 
!  ^^^^^^ keyword.control.fortran
!
   else if (a == d) then 
!  ^^^^ keyword.control.fortran
!       ^^ keyword.control.fortran
!
   else 
!  ^^^^ keyword.control.fortran
!  
   endif
!  ^^^^ keyword.control.fortran
!
   do while (a .ne. b) ! a side-comment
!  ^^ keyword.control.fortran
!     ^^^^^ keyword.control.fortran
!                        ^^^^^^^^^^^^^^ comment.line
!
   enddo
!  ^^^^^ keyword.control.fortran

   real(dp), intent(in) :: myReal ! a side-comment
!  ^^^^ storage.type
!       ^^ variable.other.fortran
!            ^^^^^^ storage.modifier.fortran
!                   ^^ keyword.other.intent.fortran 
!                          ^^^^^^ variable.other.fortran
!                                   ^^^^^^^^^^^^^^ comment.line

   class(myClass), allocatable :: myClassInstance
!  ^^^^^ storage.type.class
!        ^^^^^^^ entity.name.class
!                  ^^^^^^^^^^^ storage.modifier.fortran
!
   type :: myClass1
!  ^^^^ keyword.declaration.class
!       ^^ punctuation.separator
!          ^^^^^^^^ entity.name.class
!
!     ...
!
   contains
!
      procedure :: doStuff => myDoStuffRoutine
!     ^^^^^^^^^ keyword.declaration.function
!               ^^ punctuation.separator.double-colon.fortran
!                  ^^^^^^^ entity.name.function
!                          ^^ keyword.operator.points-to.fortran
!                             ^^^^^^^^^^^^^^^^ entity.name.function
!
      procedure, private :: doStuffMyWay => doStuffMyWayRoutine
!     ^^^^^^^^^ keyword.declaration.function
!                                        ^^ keyword.operator.points-to.fortran
!              ^ punctuation.separator
!                ^^^^^^^ storage.modifier.fortran  
!                           ^^^^^^^^^^^^ entity.name.function
!                                            ^^^^^^^^^^^^^^^^^^ entity.name.function
!
      procedure, private :: aRatherLongFunctionNameIndeed &
!                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ entity.name.function
!                                                         ^ punctuation.separator.continuation.fortran                       
                         => theImplementationNameOfTheRatherLongFunction
!
   end type myClass1
!  ^^^ keyword.control.fortran
!      ^^^^ keyword.declaration.class
!           ^^^^^^^^ entity.name.class
!
!
   type, abstract :: myClass2
!  ^^^^ keyword.declaration.class
!      ^ punctuation.separator
!        ^^^^^^^^ storage.modifier.fortran
!                 ^^ punctuation.separator
!                    ^^^^^^^ entity.name.class

   "This is a simple string"
!
   "This is a simple string,  &
   & but 'with' continuation, &
   & and more continuation!" 
!
   'This is a simple string'
!  ^^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.single.fortran
   &
!  ^ punctuation.separator.continuation.fortran
!
   'This is a simple string,  & 
   & but "with" continuation, &
   & and more continuation!' 
!  ^ punctuation.separator.continuation.fortran
!
   'string' // ' contatenation!'
!           ^^ keyword.operator.arithmetic.string-concatenation.fortran
!
   function theFunction()
!  ^^^^^^^^ keyword.declaration.function
!           ^^^^^^^^^^^ entity.name.function
   pure function theFunction()
!  ^^^^ storage.modifier.fortran
!       ^^^^^^^^ keyword.declaration.function
!                ^^^^^^^^^^^ entity.name.function 
   function theFunction(a)
!                       ^ variable.other.fortran
   function theFunction(a, bee, cesium) 
!                       ^ variable.other.fortran
!                          ^^^ variable.other.fortran
!                               ^^^^^^ variable.other.fortran
!                        ^ punctuation.separator
!
   pure function getStuff(a) result(theStuff)
!                            ^^^^^^ keyword.control.fortran
!                                   ^^^^^^^^ variable.other.fortran
!
   end function getStuff
!  ^^^ keyword.control.fortran
!      ^^^^^^^^ keyword.declaration.function
!               ^^^^^^^^ entity.name.function
!
   module subroutine doStuff(a, b, c)
!  ^^^^^^ storage.modifier.fortran
!         ^^^^^^^^^^ keyword.declaration.function
!                    ^^^^^^^ entity.name.function 
!
      implicit none
!     ^^^^^^^^^^^^^ keyword.control.fortran
!
   end subroutine doStuff
!
   module subroutine doStuff(ace, bees, cees, & ! a comment
!                                             ^ punctuation.separator.continuation.fortran
                             dees, ees, fsss)
!                            ^^^^ variable.other.fortran
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
   1.2345E-10 
!  ^^^^^^^^^^ constant.numeric
!
   1.23_dp
!       ^^ variable.other.fortran
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
!  ^^^ keyword.control.fortran
!      ^^^^^^^^^ keyword.declaration.interface
!                ^^^^^^^^^^^ entity.name.interface
!
   abstract interface
!  ^^^^^^^^ storage.modifier.fortran
!           ^^^^^^^^^ keyword.declaration.interface
!
   end interface 
!  ^^^ keyword.control.fortran
!      ^^^^^^^^^ keyword.declaration.interface
!
   if (thing == object%getThing('string', .true.)) call object%doStuff()
!               ^^^^^^ storage.type.class 
!                                       ^ punctuation.separator
!                      ^^^^^^^^ variable.function
!                                ^^^^^^ string.quoted.single.fortran
!                                         ^^^^^^ constant.language
!
   use moduleName, only: moduleRoutine, moduleObject, &
!                        ^^^^^^^^^^^^^ variable.other.fortran
!                                                     ^ punctuation.separator.continuation.fortran
                           otherModuleRoutine, anotherModuleObject
!                          ^^^^^^^^^^^^^^^^^^ variable.other.fortran
!                                            ^ punctuation.separator
   evenNumber = 0
!
   do I = 1, 10
!  ^^ keyword.control.fortran
!     ^ variable.other.fortran
!         ^ constant.numeric
!          ^ punctuation.separator
!            ^^ constant.numeric
!
      evenNumber = evenNumber + 2*I
!     ^^^^^^^^^^ variable.other.fortran
!                  ^^^^^^^^^^ variable.other.fortran
!                                 ^ variable.other.fortran
!                               ^ constant.numeric
   enddo
!  ^^^^^ keyword.control.fortran
!

   thing = object%inObject%inInObject(I)%get_thing(J)
!  ^^^^^ variable.other.fortran
!          ^^^^^^ storage.type.class
!                 ^^^^^^^^ storage.type.class
!                          ^^^^^^^^^^ storage.type.class
!                                        ^^^^^^^^^ variable.function
!
   aRealNumber = real(anInteger) 
!                ^^^^ variable.function
!                     ^^^^^^^^^ variable.other.fortran
!  simple function call
   call mySubroutine(a, b, c)
!  ^^^^ keyword.control.fortran
!       ^^^^^^^^^^^^ variable.function
!
   myVar = object%objectVariable
!  ^^^^^ variable.other.fortran
!          ^^^^^^ storage.type.class 
!                ^ punctuation.accessor
!                 ^^^^^^^^^^^^^^ variable.other.fortran
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
!                                                     ^^^^^^^^ variable.other.fortran
!
   call object%calculateStuff()
!       ^^^^^^ storage.type.class 
!              ^^^^^^^^^^^^^^ variable.function
!
   if (a == b) call mySubroutine(a, b, c)
!              ^^^^ keyword.control.fortran
!                   ^^^^^^^^^^^^ variable.function
!
   call object%objectFunction(anotherObject%variable)
!                             ^^^^^^^^^^^^^ storage.type.class 
!                                           ^^^^^^^^ variable.other.fortran
!
   call object%objectFunction(anotherObject%myFunction())
!                                           ^^^^^^^^^^ variable.function
!
   if (present(myArgument)) call doThing(myArgument)
!  ^^ keyword.control.fortran
!      ^^^^^^^ variable.function
!                                ^^^^^^^ variable.function
!
   real(dp), dimension(wf%n_ao**2, wf%n_densities), intent(in), optional :: prev_ao_density
!  ^^^^ storage.type
!            ^^^^^^^^^ storage.modifier.fortran
!                                                          ^^ keyword.other.intent.fortran
!                                                   ^^^^^^ storage.modifier.fortran
!                      ^^ storage.type.class
!                        ^ punctuation.accessor
!                         ^^^ variable.other.fortran
!
#ifdef myVar
!      ^^^^^ variable.other.fortran
   integer, parameter :: p = 1
#else
   integer, parameter :: p = 2
#endif
!<- support.function
!^^^^^ support.function

#include "someFile.F08"
!         ^^^^^^^^^^^^ string.quoted.single.fortran

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
!  ^^^^^ variable.other.fortran
!           ^^^^^^ string.quoted.single.fortran
!                     ^^^^^^ comment.line
!
   used_diag(j) = 5 ! should not recognize "use" as keyword
!  ^^^^^^^^^ variable.function
!
   real(dp) function myFunction(a, b, c) result(someResult)
!  ^^^^ storage.type
!       ^^ variable.other.fortran
!                                               ^^^^^^^^^^ variable.other.fortran
!           ^^^^^^^^ keyword.declaration.function
!                    ^^^^^^^^^^ entity.name.function
!
program myProgram
!<-^^^^ keyword.declaration.program.fortran
!       ^^^^^^^^^ entity.name
!
!  Program contents
!
end program myProgram
!   ^^^^^^^ keyword.declaration.program.fortran
!           ^^^^^^^^^ entity.name
!
   DO I = 1, 10
!  ^^ keyword.control.fortran
!     ^ variable.other.fortran
!
   ENDDO 
!  ^^^^^ keyword.control.fortran
!
   TYPE simpleStruct
!  ^^^^ keyword.declaration.class
!       ^^^^^^^^^^^^ entity.name.class
!
      integer :: x
      integer :: y 
!
   END TYPE simpleStruct
!      ^^^^ keyword.declaration.class
!           ^^^^^^^^^^^^ entity.name.class
   ALLOCATE(array(10))
!  ^^^^^^^^ variable.function
   do I = 1, 10; array(I) = I; end do 
!              ^ punctuation.terminator.fortran
!                            ^ punctuation.terminator.fortran
!
   DEALLOCATE(array)
!  ^^^^^^^^^^ variable.function
   select case (myString)
!
      case ('Phaedo')
         ! do stuff
!
      case ('Crito')
         ! do other stuff 
!
      case default
         ! what to do if string doesn't match any case
!
   end select
!
   type, abstract, extends(cat) :: superCat
!      ^ punctuation.separator.comma.fortran
!                ^ punctuation.separator.comma.fortran
!                               ^^ punctuation.separator.double-colon.fortran
!
   MODULE SUBROUTINE MY_SUBROUTINE(A, B, Cee%Dee)
!  ^^^^^^ storage.modifier.fortran
!         ^^^^^^^^^^ keyword.declaration.function
!                    ^^^^^^^^^^^^^ entity.name.function
!                                  ^ variable.other.fortran
!                                   ^ punctuation.separator.comma.fortran
!                                      ^ punctuation.separator.comma.fortran
!                                     ^ variable.other.fortran
!                                            ^^^ variable.other.fortran
!                                        ^^^ storage.type.class

   read(unit=fileUnit, *) myVariable
!  ^^^^ variable.function 
!       ^^^^ variable.language
!
   DO CONCURRENT (I = 1:N, J(I) > 0) LOCAL(M) SHARED(J, K)
!     ^^^^^^^^^^ keyword.control.fortran
!                                    ^^^^^ keyword.control.fortran
!                                             ^^^^^^ keyword.control.fortran
      M =  MOD (K(I), J(I))
      K(I) = K(I) – M
!
   END DO
!
   extraordinaryLoop: do i = 1, 5
!  ^^^^^^^^^^^^^^^^^ entity.name.label.conditional.fortran
!                   ^ punctuation.separator.single-colon.fortran
!
      print*, "I can count: ", i
!
   end do extraordinaryLoop
!         ^^^^^^^^^^^^^^^^^ entity.name.label.conditional.fortran
!
   readingTime : if (.not. person%hasBooks()) then 
!              ^ punctuation.separator.single-colon.fortran
!  ^^^^^^^^^^^ entity.name.label.conditional.fortran
!
      call person%read()
!
   else if (person%hasMoney()) then readingTime
!                                   ^^^^^^^^^^^ entity.name.label.conditional.fortran
!
      call person%buyBooks()
      call person%read()
!
   else readingTime
!       ^^^^^^^^^^^ entity.name.label.conditional.fortran
!
      call person%cry()
!
   end if readingTime
!      ^^ keyword.control.fortran
!         ^^^^^^^^^^^ entity.name.label.conditional.fortran
!
   integer, codimension(*) :: myInt
!           ^^^^^^^^^^^ storage.modifier.fortran
!
   sync all
!  ^^^^ keyword.control.fortran
!       ^^^ keyword.control.fortran
   sync images
!  ^^^^ keyword.control.fortran
!       ^^^^^^ keyword.control.fortran
   sync memory
!  ^^^^ keyword.control.fortran
!       ^^^^^^ keyword.control.fortran
!
   lock 
!  ^^^^ keyword.control.fortran
   unlock
!  ^^^^^^ keyword.control.fortran
!
   complex :: c(7,0:13) [-3:2,5,*] ! complex array coarray of corank 3
!  ^^^^^^^ storage.type.intrinsic
!             ^ variable.other.fortran
!                        ^ keyword.operator.arithmetic
!                         ^ constant.numeric
!                          ^ punctuation.separator.single-colon.fortran
!                           ^ constant.numeric
!                            ^ punctuation.separator.comma.fortran
!               ^ constant.numeric
!                 ^ constant.numeric
!                   ^^ constant.numeric
!                ^ punctuation.separator
!                  ^ punctuation.separator
!
   if ( this_image() .eq. 2 ) sync images( 3 )
!       ^^^^^^^^^^ variable.function
!                             ^^^^ keyword.control.fortran
!                                  ^^^^^^ keyword.control.fortran
!                                          ^ constant.numeric
!
!
   type(t) :: myValue[*]
   if ( img .eq. num_images() ) myValue%i(1) = myValue[1]%i(1)
!                               ^^^^^^^ storage.type.class 
!                                              ^^^^^^^ storage.type.class 
!
   a = gei[i,k](j)%asd
!      ^^^ storage.type.class 
!                 ^ punctuation.accessor
!          ^ variable.other.fortran 
!           ^ punctuation.separator 
!            ^ variable.other.fortran 
!                  ^^^ variable.other.fortran 
!
   a = this_image()
!      ^^^^^^^^^^ variable.function
!
   allocate (co % data (10 * this_image()))
!                            ^^^^^^^^^^ variable.function
!  ^^^^^^^^ variable.function
!            ^^ storage.type.class
!                 ^^^^ variable.function
!                       ^^ constant.numeric
!
   result = thisFunction ()
!  ^^^^^^ variable.other.fortran
!           ^^^^^^^^^^^^ variable.function
!
   event post (...)
!  ^^^^^ keyword.control.fortran
!        ^^^^ keyword.control.fortran
!
   procedure, public :: theRoutine & ! side-comment
!                                  ^ punctuation.separator.continuation.fortran
!                       ^^^^^^^^^^ entity.name.function
                     => theRoutine_impl ! just a side comment
!                       ^^^^^^^^^^^^^^^ entity.name.function
!
   procedure, public :: get_oei_1der &
!                                    ^ punctuation.separator.continuation.fortran
!                       ^^^^^^^^^^^^ entity.name.function
! interrupting comment!
                     => get_oei_1der_ao_tool
!                       ^^^^^^^^^^^^^^^^^^^^ entity.name.function
!
   generic :: genericRoutine => specificRoutineA, & ! and another side comment
!             ^^^^^^^^^^^^^^ entity.name.function
!                               ^^^^^^^^^^^^^^^^ entity.name.function
!                                                 ^ punctuation.separator.continuation.fortran
!                                               ^ punctuation.separator
                                specificRoutineB    ! just a side comment
!                               ^^^^^^^^^^^^^^^^ entity.name.function
!
   generic :: genericRoutine     & 
!             ^^^^^^^^^^^^^^ entity.name.function
           => specificRoutineA,  &
!          ^^ keyword.operator.points-to.fortran 
!             ^^^^^^^^^^^^^^^^ entity.name.function
              specificRoutineB ! just a side comment
!             ^^^^^^^^^^^^^^^^ entity.name.function
!
