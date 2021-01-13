! SYNTAX TEST "modern-fortran.sublime-syntax"
!
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
   a == b .and. c
!    ^^ keyword.operator.comparison.fortran
!         ^^^^^ keyword.operator.word.fortran
!
   a => b
!    ^^ keyword.operator.points-to.fortran
!
   a = b
!    ^ keyword.operator.assignment.fortran
!
   integer(kind=8), dimension(:,:), allocatable :: myInt
!  ^^^^^^^ storage.type.intrinsic.fortran
!                   ^^^^^^^^^ storage.modifier.fortran
!                                   ^^^^^^^^^^^ storage.modifier.fortran
!
   real(4) :: aNumber
!  ^^^^ storage.type.intrinsic.fortran
!          ^^ punctuation.separator.double-colon.fortran
!             ^^^^^^^ variable.other.fortran
!
   do while (a .ne. b) ! a side-comment
!  ^^ keyword.control.fortran
!     ^^^^^ keyword.control.fortran
!                        ^^^^^^^^^^^^^^ comment.line.fortran
!
   enddo
!  ^^^^^ keyword.control.fortran

   real(dp), intent(in) :: myReal ! a side-comment
!  ^^^^ storage.type.intrinsic.fortran
!       ^^ variable.other.fortran
!            ^^^^^^ storage.modifier.fortran
!                   ^^ keyword.other.intent.fortran
!                          ^^^^^^ variable.other.fortran
!                                   ^^^^^^^^^^^^^^ comment.line.fortran

   class(myClass), allocatable :: myClassInstance
!  ^^^^^ storage.type.class.fortran
!        ^^^^^^^ entity.name.class.fortran
!                  ^^^^^^^^^^^ storage.modifier.fortran
!
   type :: myClass1
!  ^^^^ keyword.declaration.class.fortran
!       ^^ punctuation.separator.double-colon.fortran
!          ^^^^^^^^ entity.name.class.fortran
!
      class(abstractClass), allocatable :: polymorphicStrategy
!     ^^^^^ storage.type.class.fortran
!                                          ^^^^^^^^^^^^^^^^^^^ variable.other.fortran
!                                       ^^ punctuation.separator.double-colon.fortran
!           ^^^^^^^^^^^^^ entity.name.class.fortran
!                           ^^^^^^^^^^^ storage.modifier.fortran
!
!     ...
!
   contains
!  ^^^^^^^^ keyword.declaration.contains.fortran
!
      procedure :: doStuff => myDoStuffRoutine
!     ^^^^^^^^^ keyword.declaration.function.fortran
!               ^^ punctuation.separator.double-colon.fortran
!                  ^^^^^^^ entity.name.function.fortran
!                          ^^ keyword.operator.points-to.fortran
!                             ^^^^^^^^^^^^^^^^ entity.name.function.fortran
!
      procedure, private :: doStuffMyWay => doStuffMyWayRoutine
!     ^^^^^^^^^ keyword.declaration.function.fortran
!                        ^^ punctuation.separator.double-colon.fortran
!                                        ^^ keyword.operator.points-to.fortran
!              ^ punctuation.separator.comma.fortran
!                ^^^^^^^ storage.modifier.fortran
!                           ^^^^^^^^^^^^ entity.name.function.fortran
!                                            ^^^^^^^^^^^^^^^^^^ entity.name.function.fortran
!
      procedure, private :: aRatherLongFunctionNameIndeed &
!                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ entity.name.function.fortran
!                                                         ^ punctuation.separator.continuation.fortran
                         => theImplementationNameOfTheRatherLongFunction
!                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ entity.name.function.fortran
!                        ^^ keyword.operator.points-to.fortran
!
   end type myClass1
!  ^^^ keyword.declaration.class.fortran
!      ^^^^ keyword.declaration.class.fortran
!           ^^^^^^^^ entity.name.class.fortran
!
!
   type, abstract :: myClass2
!  ^^^^ keyword.declaration.class.fortran
!      ^ punctuation.separator.comma.fortran
!        ^^^^^^^^ storage.modifier.fortran
!                 ^^ punctuation.separator.double-colon.fortran
!                    ^^^^^^^ entity.name.class.fortran

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
!  ^^^^^^^^ keyword.declaration.function.fortran
!           ^^^^^^^^^^^ entity.name.function.fortran
   pure function theFunction()
!  ^^^^ storage.modifier.function.prefix.fortran
!       ^^^^^^^^ keyword.declaration.function.fortran
!                ^^^^^^^^^^^ entity.name.function.fortran
   recursive module function theFunction(a)
!  ^^^^^^^^^ storage.modifier.function.prefix.fortran
!            ^^^^^^ storage.modifier.function.prefix.fortran
!                                        ^ variable.other.fortran
   function theFunction(a, bee, cesium)
!                       ^ variable.other.fortran
!                          ^^^ variable.other.fortran
!                               ^^^^^^ variable.other.fortran
!                        ^ punctuation.separator.comma.fortran
!
   pure function getStuff(a) result(theStuff)
!                            ^^^^^^ keyword.control.function-result.fortran
!                                   ^^^^^^^^ variable.other.fortran
!
   end function getStuff
!  ^^^ keyword.declaration.function.fortran
!      ^^^^^^^^ keyword.declaration.function.fortran
!               ^^^^^^^^ entity.name.function.fortran
!
   real(dp) function myFunction(a, b, c)
!  ^^^^ storage.type.intrinsic.fortran
!       ^^ variable.other.fortran
! ...
!
   end function myFunction
!
   module subroutine doStuff(a, b, c)
!  ^^^^^^ storage.modifier.function.prefix.fortran
!         ^^^^^^^^^^ keyword.declaration.function.fortran
!                    ^^^^^^^ entity.name.function.fortran
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
!  ^^^^^^ keyword.declaration.interface.module.fortran
!         ^^^^^^^^ entity.name.interface.module.fortran
!
   submodule (moduleName) submoduleName
!  ^^^^^^^^^ keyword.declaration.interface.submodule.fortran
!             ^^^^^^^^^^ entity.name.interface.inherited-module.fortran
!                         ^^^^^^^^^^^^^ entity.name.interface.submodule.fortran
!
   end submodule submoduleName
!      ^^^^^^^^^ keyword.declaration.interface.submodule.fortran
!                ^^^^^^^^^^^^^ entity.name.interface.submodule.fortran
!
   end submodule ! just empty end-name also allowed
!      ^^^^^^^^^ keyword.declaration.interface.submodule.fortran
   8
!  ^ constant.numeric.fortran
!
   123
!  ^^^ constant.numeric.fortran
!
   1.0d-12
!  ^^^^^^^ constant.numeric.fortran
!
   1.2345E-10
!  ^^^^^^^^^^ constant.numeric.fortran
!
   1.23_dp
!      ^ punctuation.separator.underscore.fortran
!       ^^ variable.other.fortran
!
   a = minval(b)
!      ^^^^^^ variable.function.function.intrinsic.fortran
!
!  type casting versus variable declaration
   real(8) :: aRealNumber
!  ^^^^ storage.type.intrinsic.fortran
!
   interface myInterface
!  ^^^^^^^^^ keyword.declaration.interface.interface.fortran
!            ^^^^^^^^^^^ entity.name.interface.interface.fortran
!
      include "path/to/file.F90"
!     ^^^^^^^ keyword.control.import.fortran
!
   end interface myInterface
!  ^^^ keyword.declaration.interface.interface.fortran
!      ^^^^^^^^^ keyword.declaration.interface.interface.fortran
!                ^^^^^^^^^^^ entity.name.interface.interface.fortran
!
   abstract interface
!  ^^^^^^^^ storage.modifier.fortran
!           ^^^^^^^^^ keyword.declaration.interface.interface.fortran
!
   end interface
!  ^^^ keyword.declaration.interface.interface.fortran
!      ^^^^^^^^^ keyword.declaration.interface.interface.fortran
!
   if (thing == object%getThing('string', .true.)) call object%doStuff()
!               ^^^^^^ storage.type.class.fortran
!                                       ^ punctuation.separator.comma.fortran
!                      ^^^^^^^^ variable.function.fortran
!                                ^^^^^^ string.quoted.single.fortran
!                                         ^^^^^^ constant.language.fortran
!
   use moduleName, only: moduleRoutine, moduleObject, &
!  ^^^ keyword.control.import.fortran
!      ^^^^^^^^^^ entity.name.interface.module.fortran
!                        ^^^^^^^^^^^^^ variable.other.fortran
!                  ^^^^ keyword.control.fortran
!                ^ punctuation.separator.comma.fortran
!                      ^ punctuation.separator.single-colon.fortran
!                                                     ^ punctuation.separator.continuation.fortran
                           otherModuleRoutine, anotherModuleObject
!                          ^^^^^^^^^^^^^^^^^^ variable.other.fortran
!                                            ^ punctuation.separator.comma.fortran
   evenNumber = 0
!
   do I = 1, 10
!  ^^ keyword.control.fortran
!     ^ variable.other.fortran
!         ^ constant.numeric.fortran
!          ^ punctuation.separator.comma.fortran
!            ^^ constant.numeric.fortran
!
      evenNumber = evenNumber + 2*I
!     ^^^^^^^^^^ variable.other.fortran
!                  ^^^^^^^^^^ variable.other.fortran
!                                 ^ variable.other.fortran
!                               ^ constant.numeric.fortran
   enddo
!  ^^^^^ keyword.control.fortran
!

   thing = object%inObject%inInObject(I)%get_thing(J)
!  ^^^^^ variable.other.fortran
!          ^^^^^^ storage.type.class.fortran
!                 ^^^^^^^^ storage.type.class.fortran
!                          ^^^^^^^^^^ storage.type.class.fortran
!                                        ^^^^^^^^^ variable.function.fortran
!
   aRealNumber = real(anInteger)
!                ^^^^ variable.function.function.intrinsic.fortran
!                     ^^^^^^^^^ variable.other.fortran
!  simple function call
   call mySubroutine(a, b, c)
!  ^^^^ keyword.control.fortran
!       ^^^^^^^^^^^^ variable.function.fortran
!
   myVar = object%objectVariable
!  ^^^^^ variable.other.fortran
!          ^^^^^^ storage.type.class.fortran
!                ^ punctuation.accessor.fortran
!                 ^^^^^^^^^^^^^^ variable.other.fortran
!
   myVar = object%objectVariable%getStuff(a, b)
!          ^^^^^^ storage.type.class.fortran
!                 ^^^^^^^^^^^^^^ storage.type.class.fortran
!                                ^^^^^^^^ variable.function.fortran
!
   myVar = object%objectFunction(a, b, c, otherObject%variable)
!          ^^^^^^ storage.type.class.fortran
!                 ^^^^^^^^^^^^^^ variable.function.fortran
!                                         ^^^^^^^^^^^ storage.type.class.fortran
!                                                     ^^^^^^^^ variable.other.fortran
!
   call object%calculateStuff()
!       ^^^^^^ storage.type.class.fortran
!              ^^^^^^^^^^^^^^ variable.function
!
   if (a == b) call mySubroutine(a, b, c)
!              ^^^^ keyword.control.fortran
!                   ^^^^^^^^^^^^ variable.function
!
   call object%objectFunction(anotherObject%variable)
!                             ^^^^^^^^^^^^^ storage.type.class.fortran
!                                           ^^^^^^^^ variable.other.fortran
!
   call object%objectFunction(anotherObject%myFunction())
!                                           ^^^^^^^^^^ variable.function.fortran
!
   if (present(myArgument)) call doThing(myArgument)
!  ^^ keyword.control.fortran
!      ^^^^^^^ variable.function.function.intrinsic.fortran
!                                ^^^^^^^ variable.function.fortran
!
   real(dp), dimension(wf%n_ao**2, wf%n_densities), intent(in), optional :: prev_ao_density
!  ^^^^ storage.type.intrinsic.fortran
!            ^^^^^^^^^ storage.modifier.fortran
!                                                          ^^ keyword.other.intent.fortran
!                                                   ^^^^^^ storage.modifier.fortran
!                      ^^ storage.type.class.fortran
!                        ^ punctuation.accessor.fortran
!                         ^^^ variable.other.fortran
!
#ifdef myVar
!      ^^^^^ variable.other.fortran
   integer, parameter :: p = 1
#else
   integer, parameter :: p = 2
#endif
!<- support.function.fpp
!^^^^^ support.function.fpp

#include "someFile.F08"
!         ^^^^^^^^^^^^ string.quoted.single.fortran

!$omp parallel do private(I) schedule(dynamic)
!^ support.function.omp
!                            ^^^^^^^^ support.function.omp
!                         ^ variable.other.fortran
!                                     ^^^^^^^ support.constant.omp
!                 ^^^^^^^ support.constant.omp
! ^^^ support.function.omp
   do I = 1, 10
!
      f(I) = someThing(I)
!
   enddo
!$omp end parallel do

   type1 = "hello!" ! should understand that 'type1' is a variable
!  ^^^^^ variable.other.fortran
!           ^^^^^^ string.quoted.single.fortran
!                     ^^^^^^ comment.line.fortran
!
   used_diag(j) = 5 ! should not recognize "use" as keyword
!  ^^^^^^^^^ variable.function.fortran
!
   real(dp) function myFunction(a, b, c) result(someResult)
!  ^^^^ storage.type.intrinsic.fortran
!       ^^ variable.other.fortran
!                                               ^^^^^^^^^^ variable.other.fortran
!           ^^^^^^^^ keyword.declaration.function.fortran
!                    ^^^^^^^^^^ entity.name.function.fortran
!
program myProgram
!<-^^^^ keyword.declaration.program.fortran
!       ^^^^^^^^^ entity.name.program.fortran
!
!  Program contents
!
end program myProgram
!   ^^^^^^^ keyword.declaration.program.fortran
!           ^^^^^^^^^ entity.name.program.fortran
!
   DO I = 1, 10
!  ^^ keyword.control.fortran
!     ^ variable.other.fortran
!
   ENDDO
!  ^^^^^ keyword.control.fortran
!
   TYPE simpleStruct
!  ^^^^ keyword.declaration.class.fortran
!       ^^^^^^^^^^^^ entity.name.class.fortran
!
      integer :: x
      integer :: y
!
   END TYPE simpleStruct
!      ^^^^ keyword.declaration.class.fortran
!           ^^^^^^^^^^^^ entity.name.class.fortran
   ALLOCATE(array(10))
!  ^^^^^^^^ variable.function.subroutine.intrinsic.fortran
   do I = 1, 10; array(I) = I; end do
!              ^ punctuation.terminator.fortran
!                            ^ punctuation.terminator.fortran
!
   DEALLOCATE(array)
!  ^^^^^^^^^^ variable.function.subroutine.intrinsic.fortran
!
   select case (myString)
!  ^^^^^^ keyword.control.fortran
!         ^^^^ keyword.control.fortran
!
      case ('Phaedo')
!     ^^^^ keyword.control.fortran
         ! do stuff
!
      case ('Crito')
!     ^^^^ keyword.control.fortran
         ! do other stuff
!
      case default
!     ^^^^ keyword.control.fortran
!          ^^^^^^^ keyword.control.fortran
         ! what to do if string doesn't match any case
!
   end select
!  ^^^ keyword.control.fortran
!      ^^^^^^ keyword.control.fortran
!
   namedSelect: select case (myString)
!  ^^^^^^^^^^^ entity.name.label.conditional.fortran
!
   end select namedSelect
!             ^^^^^^^^^^^ entity.name.label.conditional.fortran
!
   select type (animal)
!  ^^^^^^^^^^^ keyword.control.fortran
!               ^^^^^^ variable.other.fortran
!
      type is (cat)
!     ^^^^^^^ keyword.control.fortran
!              ^^^ entity.name.class.fortran

      type is (dog)
!     ^^^^^^^ keyword.control.fortran
!              ^^^ entity.name.class.fortran
!
   end select
!
   animalCasting: select class (animal)
!  ^^^^^^^^^^^^^ entity.name.label.conditional.fortran
!                 ^^^^^^^^^^^^ keyword.control.fortran
!                               ^^^^^^ variable.other.fortran
!
      class is (cat)
!     ^^^^^^^^ keyword.control.fortran
!               ^^^ entity.name.class.fortran
!
      class is (dog)
!     ^^^^^^^^ keyword.control.fortran
!               ^^^ entity.name.class.fortran
!
      class default
!     ^^^^^^^^^^^^^ keyword.control.fortran
!
   end select animalCasting
!             ^^^^^^^^^^^^^ entity.name.label.conditional.fortran
!  ^^^^^^^^^^ keyword.control.fortran
!
!
   type, abstract, extends(cat) :: superCat
!      ^ punctuation.separator.comma.fortran
!        ^^^^^^^^ storage.modifier.fortran
!                  ^^^^^^^ keyword.control.extends.fortran
!                ^ punctuation.separator.comma.fortran
!                               ^^ punctuation.separator.double-colon.fortran
!
   MODULE SUBROUTINE MY_SUBROUTINE(A, B, Cee%Dee)
!  ^^^^^^ storage.modifier.function.prefix.fortran
!         ^^^^^^^^^^ keyword.declaration.function.fortran
!                    ^^^^^^^^^^^^^ entity.name.function.fortran
!                                  ^ variable.other.fortran
!                                   ^ punctuation.separator.comma.fortran
!                                      ^ punctuation.separator.comma.fortran
!                                     ^ variable.other.fortran
!                                            ^^^ variable.other.fortran
!                                        ^^^ storage.type.class.fortran

   read(unit=fileUnit, *) myVariable
!  ^^^^ variable.function.subroutine.intrinsic.io.fortran
!       ^^^^ variable.language.io.fortran
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
!  ^^^^^^^ storage.type.intrinsic.fortran
!             ^ variable.other.fortran
!                        ^ keyword.operator.arithmetic.fortran
!                         ^ constant.numeric.fortran
!                          ^ punctuation.separator.single-colon.fortran
!                           ^ constant.numeric.fortran
!                            ^ punctuation.separator.comma.fortran
!               ^ constant.numeric.fortran
!                 ^ constant.numeric.fortran
!                   ^^ constant.numeric.fortran
!                ^ punctuation.separator.comma.fortran
!                  ^ punctuation.separator.single-colon.fortran
!
   a = 3 + 4*(12/5)
!  ^ variable.other.fortran
!    ^ keyword.operator.assignment.fortran
!        ^ keyword.operator.arithmetic.fortran
!           ^ keyword.operator.arithmetic.fortran
!               ^ keyword.operator.arithmetic.fortran
!
   if ( this_image() .eq. 2 ) sync images( 3 )
!       ^^^^^^^^^^ variable.function.function.intrinsic.fortran
!                             ^^^^ keyword.control.fortran
!                                  ^^^^^^ keyword.control.fortran
!                                          ^ constant.numeric.fortran
!
!
   type(t) :: myValue[*]
   if ( img .eq. num_images() ) myValue%i(1) = myValue[1]%i(1)
!                               ^^^^^^^ storage.type.class.fortran
!                                              ^^^^^^^ storage.type.class.fortran
!
   a = gei[i,k](j)%asd
!      ^^^ storage.type.class.fortran
!                 ^ punctuation.accessor.fortran
!          ^ variable.other.fortran
!           ^ punctuation.separator.comma.fortran
!            ^ variable.other.fortran
!                  ^^^ variable.other.fortran
!
   a = this_image()
!      ^^^^^^^^^^ variable.function.function.intrinsic.fortran
!
   allocate (co % data (10 * this_image()))
!                            ^^^^^^^^^^ variable.function.function.intrinsic.fortran
!  ^^^^^^^^ variable.function.subroutine.intrinsic.fortran
!            ^^ storage.type.class.fortran
!                 ^^^^ variable.function.fortran
!                       ^^ constant.numeric.fortran
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
!                       ^^^^^^^^^^ entity.name.function.fortran
                     => theRoutine_impl ! just a side comment
!                       ^^^^^^^^^^^^^^^ entity.name.function.fortran
!
   procedure, public :: get_oei_1der &
!  ^^^^^^^^^ keyword.declaration.function.fortran
!                                    ^ punctuation.separator.continuation.fortran
!                       ^^^^^^^^^^^^ entity.name.function.fortran
! interrupting comment!
                     => get_oei_1der_ao_tool
!                       ^^^^^^^^^^^^^^^^^^^^ entity.name.function.fortran
!
   generic :: genericRoutine => specificRoutineA, & ! and another side comment
!  ^^^^^^^ keyword.declaration.function.fortran
!             ^^^^^^^^^^^^^^ entity.name.function.fortran
!                               ^^^^^^^^^^^^^^^^ entity.name.function.fortran
!                                                 ^ punctuation.separator.continuation.fortran
!                                               ^ punctuation.separator
                                specificRoutineB    ! just a side comment
!                               ^^^^^^^^^^^^^^^^ entity.name.function.fortran
!
   generic :: genericRoutine     &
!             ^^^^^^^^^^^^^^ entity.name.function.fortran
           => specificRoutineA,  &
!          ^^ keyword.operator.points-to.fortran
!             ^^^^^^^^^^^^^^^^ entity.name.function.fortran
              specificRoutineB ! just a side comment
!             ^^^^^^^^^^^^^^^^ entity.name.function.fortran
!
   final :: myDestructor
!  ^^^^^ keyword.declaration.function.fortran
!           ^^^^^^^^^^^^ entity.name.function.fortran
!
   type, extends (animal) :: cat ! with spaces & commment
!                                ^ punctuation.definition.comment.fortran
!                                  ^^^^^^^^^^^^^^^^^^^^^^ comment.line.fortran
!        ^^^^^^^ keyword.control.extends.fortran
!                 ^^^^^^ entity.other.inherited-class.fortran
!                            ^^^ entity.name.class.fortran
!
   return
!  ^^^^^^ keyword.control.fortran
!
   use :: myModule, only: thatRoutine, thisRoutine
!  ^^^ keyword.control.import.fortran
!                       ^ punctuation.separator.single-colon.fortran
!                                    ^ punctuation.separator.comma.fortran
!                         ^^^^^^^^^^^ variable.other.fortran
!                                      ^^^^^^^^^^^ variable.other.fortran
!                   ^^^^ keyword.control.fortran
!      ^^ punctuation.separator.double-colon.fortran
!         ^^^^^^^^ entity.name.interface.module.fortran
!
   stop 98
!       ^^ constant.numeric.fortran
!  ^^^^ keyword.control.fortran
!
   error stop 1
!             ^ constant.numeric.fortran
!  ^^^^^ keyword.control.fortran
!        ^^^^ keyword.control.fortran
!
   error stop "Something went wrong!"
!             ^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.single.fortran
!  ^^^^^ keyword.control.fortran
!        ^^^^ keyword.control.fortran
!
   error stop 1, quiet=(a .eq. b)
!             ^ constant.numeric.fortran
!                ^^^^^ keyword.control.fortran
!  ^^^^^ keyword.control.fortran
!        ^^^^ keyword.control.fortran
!
   LOOP_A : DO I = 1, 15
!  ^^^^^^ entity.name.label.conditional.fortran
      N = N + 1
      IF (N > I) EXIT LOOP_A
!                     ^^^^^^ entity.name.label.conditional.fortran
!
   END DO LOOP_A
!         ^^^^^^ entity.name.label.conditional.fortran
!
