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
   end
!  ^^^ keyword.control.fortran
!
   read(unit=myUnit, end=200) byte ! Read until end of file, then go to 200
!       ^^^^ variable.language.io.fortran
!                    ^^^ variable.language.io.fortran
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
!                                                  ^^^^^ variable.other.fortran
!                                                 ^ - variable.other.fortran
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
!
   elsei ! should not recognize 'else' in 'elsei'
!  ^^^^ - keyword.control.fortran
!
!
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
!  ^^^^^^^^^^^^^^^^ meta.class.declaration.fortran
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
!           ^^^^^^^^ entity.name.class.fortran - meta.class.declaration.fortran
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
!  ^ punctuation.definition.string.begin.fortran
!                          ^ punctuation.definition.string.end.fortran
   &
!  ^ punctuation.separator.continuation.fortran
!
   'This is a simple string,  &
   & but "with" continuation, &
   & and more continuation!'
!  ^ punctuation.separator.continuation.fortran
!
   'This is a simple string & !no comment here'
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.single.fortran - punctuation.separator.continuation - comment
!
   'This is a simple string, &
   ! this is a comment, followed by a blank line

   & and this is continuation of the string'

   'Line continuation character missing on the next line, &
      but highlighting of the rest of the file is still not messed up.'
    x = 1
!   ^^^^^ - string
!
   'string' // ' contatenation!'
!           ^^ keyword.operator.arithmetic.string-concatenation.fortran
!
   function theFunction()
!  ^^^^^^^^^^^^^^^^^^^^ meta.function.declaration.fortran
!                      ^^ meta.function.parameters.fortran
!  ^^^^^^^^ keyword.declaration.function.fortran
!           ^^^^^^^^^^^ entity.name.function.fortran
   pure function theFunction()
!  ^^^^ storage.modifier.function.prefix.fortran
!       ^^^^^^^^^^^^^^^^^^^^ meta.function.declaration.fortran
!                           ^^ meta.function.parameters.fortran
!       ^^^^^^^^ keyword.declaration.function.fortran
!                ^^^^^^^^^^^ entity.name.function.fortran
   recursive module function theFunction(a)
!  ^^^^^^^^^ storage.modifier.function.prefix.fortran
!            ^^^^^^ storage.modifier.function.prefix.fortran
!                                       ^^^ meta.function.parameters.fortran
!                                       ^ punctuation.section.parens.begin.fortran
!                                        ^ variable.parameter.input.fortran
!                                         ^ punctuation.section.parens.end.fortran
   function theFunction(a, bee, cesium)
!                      ^^^^^^^^^^^^^^^^ meta.function.parameters.fortran
!                      ^ punctuation.section.parens.begin.fortran
!                       ^ variable.parameter.input.fortran
!                        ^ punctuation.separator.comma.fortran
!                          ^^^ variable.parameter.input.fortran
!                             ^ punctuation.separator.comma.fortran
!                               ^^^^^^ variable.parameter.input.fortran
!                                     ^ punctuation.section.parens.end.fortran
   function theFunction(a, & ! comment
!                      ^^^^^^^^^^^^^^^^ meta.function.parameters.fortran
!                          ^ punctuation.separator.continuation.fortran
!                            ^^^^^^^^^^ comment.line.fortran

      b, c)
!^^^^^^^^^^ meta.function.parameters.fortran
!     ^ variable.parameter.input.fortran
!
   function result(a)
!           ^^^^^^ entity.name.function.fortran - keyword
!
   pure function getStuff(a) result(theStuff)
!       ^^^^^^^^^^^^^^^^^ meta.function.declaration.fortran - meta.function meta.function
!                        ^^^ meta.function.parameters.fortran - meta.function meta.function
!                           ^^^^^^^ meta.function.declaration.fortran - meta.function meta.function
!                                  ^^^^^^^^^^ meta.function.parameters.fortran - meta.function meta.function
!                                            ^ - meta.function.parameters
!                        ^ punctuation.section.parens.begin.fortran
!                         ^ variable.parameter.input.fortran
!                          ^ punctuation.section.parens.end.fortran
!                            ^^^^^^ keyword.control.function-result.fortran
!                                  ^ punctuation.section.parens.begin.fortran
!                                   ^^^^^^^^ variable.parameter.output.fortran
!                                           ^ punctuation.section.parens.end.fortran
!
   end function getStuff
!  ^^^ keyword.declaration.function.fortran
!     ^ - keyword
!      ^^^^^^^^ keyword.declaration.function.fortran
!               ^^^^^^^^ entity.name.function.fortran - meta.function.declaration
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
!         ^^^^^^^^^^^^^^^^^^ meta.function.declaration.fortran
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
!                            ^^^^ variable.parameter.fortran
!
   module myModule
!  ^^^^^^ keyword.declaration.interface.module.fortran
!         ^^^^^^^^ entity.name.interface.module.fortran
!  ^^^^^^^^^^^^^^^ meta.module.declaration.fortran
!
   module myModule ! comment
!  ^^^^^^ keyword.declaration.interface.module.fortran
!         ^^^^^^^^ entity.name.interface.module.fortran
!  ^^^^^^^^^^^^^^^ meta.module.declaration.fortran
!
   end module myModule
!  ^^^ keyword.declaration.interface.module.fortran
!      ^^^^^^ keyword.declaration.interface.module.fortran
!             ^^^^^^^^ entity.name.interface.module.fortran - meta.module.declaration.fortran
!
   end module
!  ^^^ keyword.declaration.interface.module.fortran
!      ^^^^^^ keyword.declaration.interface.module.fortran
!
   submodule (moduleName) submoduleName
!  ^^^^^^^^^ keyword.declaration.interface.submodule.fortran
!             ^^^^^^^^^^ entity.name.interface.inherited-module.fortran
!                         ^^^^^^^^^^^^^ entity.name.interface.submodule.fortran
!  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ meta.submodule.declaration.fortran
!
   end submodule submoduleName
!      ^^^^^^^^^ keyword.declaration.interface.submodule.fortran
!                ^^^^^^^^^^^^^ entity.name.interface.submodule.fortran - meta.submodule.declaration.fortran
!
   end submodule ! just empty end-name also allowed
!      ^^^^^^^^^ keyword.declaration.interface.submodule.fortran
!
   end submodule
!  ^^^ keyword.declaration.interface.submodule.fortran
!      ^^^^^^^^^keyword.declaration.interface.submodule.fortran
!
   8
!  ^ meta.number.integer.decimal.fortran constant.numeric.value.fortran
   123
!  ^^^ meta.number.integer.decimal.fortran constant.numeric.value.fortran
   1_8
!  ^ meta.number.integer.decimal.fortran constant.numeric.value.fortran
!   ^^ meta.number.integer.decimal.fortran constant.numeric.suffix.fortran
   123.
!  ^^^^ meta.number.float.decimal.fortran constant.numeric.value.fortran
!     ^ punctuation.separator.decimal.fortran
   .123
!  ^^^^ meta.number.float.decimal.fortran constant.numeric.value.fortran
!  ^ punctuation.separator.decimal.fortran
   1.0d-12
!  ^^^^^^^ meta.number.float.decimal.fortran constant.numeric.value.fortran
!   ^ punctuation.separator.decimal.fortran
   1.2345E-10
!  ^^^^^^^^^^ meta.number.float.decimal.fortran constant.numeric.value.fortran
!   ^ punctuation.separator.decimal.fortran
   1e2
!  ^^^ meta.number.float.decimal.fortran constant.numeric.value.fortran
   1.23_dp
!  ^^^^^^^ meta.number.float.decimal.fortran
!  ^^^^ constant.numeric.value.fortran
!      ^^^ constant.numeric.suffix.fortran
!   ^ punctuation.separator.decimal.fortran
!
   a = minval(b)
!      ^^^^^^ support.function.intrinsic.fortran
!
!  type casting versus variable declaration
   real(8) :: aRealNumber
!  ^^^^ storage.type.intrinsic.fortran
!
   interface myInterface
!  ^^^^^^^^^ keyword.declaration.interface.interface.fortran
!            ^^^^^^^^^^^ entity.name.interface.interface.fortran
!           ^ - entity.name.interface.interface.fortran
!                       ^^^ - entity.name.interface.interface.fortran
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
!         ^ constant.numeric.value.fortran
!          ^ punctuation.separator.comma.fortran
!            ^^ constant.numeric.value.fortran
!
      evenNumber = evenNumber + 2*I
!     ^^^^^^^^^^ variable.other.fortran
!                  ^^^^^^^^^^ variable.other.fortran
!                                 ^ variable.other.fortran
!                               ^ constant.numeric.value.fortran
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
!                ^^^^ support.function.intrinsic.fortran
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
!      ^^^^^^^ support.function.intrinsic.fortran
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
!<-^^^ keyword.control.directive.fortran
!      ^^^^^ variable.other.fortran
   integer, parameter :: p = 1
#else
!<-^^ keyword.control.directive.fortran
   integer, parameter :: p = 2
#endif
!<- keyword.control.directive.fortran
!^^^^^ keyword.control.directive.fortran

#include "someFile.F08"
!        ^^^^^^^^^^^^^^ string.quoted.double.fortran
!        ^ punctuation.definition.string.begin.fortran
!                     ^ punctuation.definition.string.end.fortran

!$omp parallel do private(I) schedule(dynamic)
!<^^^ keyword.control.directive.fortran
!                            ^^^^^^^^ keyword.control.directive.fortran
!     ^^^^^^^^ keyword.control.directive.fortran
!              ^^ keyword.control.directive.fortran
!                 ^^^^^^^ keyword.control.directive.fortran
!                            ^^^^^^^^ keyword.control.directive.fortran
!                         ^ variable.other.fortran
!                                     ^^^^^^^ support.constant.omp
   do I = 1, 10
!
!$    thread = omp_get_num_threads()
!^ keyword.control.directive.fortran
!              ^^^^^^^^^^^^^^^^^^^ support.function.omp
!
      f(I) = someThing(I)
!
   enddo
!$omp end parallel do
!<^^^ keyword.control.directive.fortran
!                  ^^ keyword.control.directive.fortran
!     ^^^ keyword.control.directive.fortran
!         ^^^^^^^^ keyword.control.directive.fortran

   type1 = "hello!" ! should understand that 'type1' is a variable
!  ^^^^^ variable.other.fortran
!          ^^^^^^^^ string.quoted.double.fortran
!          ^ punctuation.definition.string.begin.fortran
!                 ^ punctuation.definition.string.end.fortran
!                     ^^^^^^ comment.line.fortran
!
   used_diag(j) = 5 ! should not recognize "use" as keyword
!  ^^^^^^^^^ variable.function.fortran
!
   real(dp) function myFunction(a, b, c) result(someResult)
!  ^^^^ storage.type.intrinsic.fortran
!       ^^ variable.other.fortran
!                                               ^^^^^^^^^^ variable.parameter.output.fortran
!           ^^^^^^^^ keyword.declaration.function.fortran
!                    ^^^^^^^^^^ entity.name.function.fortran
!
program myProgram
!<-^^^^ keyword.declaration.program.fortran
!       ^^^^^^^^^ entity.name.program.fortran
!<-^^^^^^^^^^^^^^ meta.program.declaration.fortran
!
!  Program contents
!
end program myProgram
!<- keyword.declaration.program.fortran
!   ^^^^^^^ keyword.declaration.program.fortran
!           ^^^^^^^^^ entity.name.program.fortran - meta.program.declaration.fortran
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
!  ^^^^^^^^^^^^^^^^^ meta.class.declaration.fortran
!
      integer :: x
      integer :: y
!
   END TYPE simpleStruct
!      ^^^^ keyword.declaration.class.fortran
!           ^^^^^^^^^^^^ entity.name.class.fortran
   ALLOCATE(array(10))
!  ^^^^^^^^ support.function.subroutine.fortran
!
   do I = 1, 10; array(I) = I; end do
!              ^ punctuation.terminator.fortran
!                            ^ punctuation.terminator.fortran
!
   DEALLOCATE(array)
!  ^^^^^^^^^^ support.function.subroutine.fortran
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
!  ^^^ keyword.control.fortran
!      ^^^^^^ keyword.control.fortran
!
!
   type, abstract, extends(cat) :: superCat
!      ^ punctuation.separator.comma.fortran
!        ^^^^^^^^ storage.modifier.fortran
!                  ^^^^^^^ keyword.control.extends.fortran
!                ^ punctuation.separator.comma.fortran
!                               ^^ punctuation.separator.double-colon.fortran
!
   MODULE SUBROUTINE MY_SUBROUTINE(A, B, C)
!  ^^^^^^ storage.modifier.function.prefix.fortran
!         ^^^^^^^^^^ keyword.declaration.function.fortran
!                    ^^^^^^^^^^^^^ entity.name.function.fortran
!                                  ^ variable.parameter.fortran
!                                   ^ punctuation.separator.comma.fortran
!                                      ^ punctuation.separator.comma.fortran
!                                     ^ variable.parameter.fortran
!                                        ^ variable.parameter.fortran


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
   readingTime : if (person%hasBooks()) then
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
!                         ^ constant.numeric.value.fortran
!                          ^ punctuation.separator.single-colon.fortran
!                           ^ constant.numeric.value.fortran
!                            ^ punctuation.separator.comma.fortran
!               ^ constant.numeric.value.fortran
!                 ^ constant.numeric.value.fortran
!                   ^^ constant.numeric.value.fortran
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
!       ^^^^^^^^^^ support.function.intrinsic.fortran
!                             ^^^^ keyword.control.fortran
!                                  ^^^^^^ keyword.control.fortran
!                                          ^ constant.numeric.value.fortran
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
!      ^^^^^^^^^^ support.function.intrinsic.fortran
!
   allocate (co % data (10 * this_image()))
!                            ^^^^^^^^^^ support.function.intrinsic.fortran
!  ^^^^^^^^ support.function.subroutine.fortran
!            ^^ storage.type.class.fortran
!                 ^^^^ variable.function.fortran
!                       ^^ constant.numeric.value.fortran
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
!       ^^ constant.numeric.value.fortran
!  ^^^^ keyword.control.fortran
!
   error stop 1
!             ^ constant.numeric.value.fortran
!  ^^^^^ keyword.control.fortran
!        ^^^^ keyword.control.fortran
!
   error stop "Something went wrong!"
!             ^^^^^^^^^^^^^^^^^^^^^^^ string.quoted.double.fortran
!  ^^^^^ keyword.control.fortran
!        ^^^^ keyword.control.fortran
!
   error stop 1, quiet=(a .eq. b)
!             ^ constant.numeric.value.fortran
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
