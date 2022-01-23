c SYNTAX TEST "fortran.sublime-syntax"

      DO 50 I = 1, 10
c     ^^ keyword.control.fortran
c        ^^ entity.name.label.statement-number.fortran
c           ^ variable.other.fortran
c             ^ keyword.operator.assignment.fortran
c               ^ meta.number.integer.decimal.fortran constant.numeric.value.fortran
c                ^ punctuation.separator.comma.fortran
c                  ^^ meta.number.integer.decimal.fortran constant.numeric.value.fortran
   50 CONTINUE
c  ^^ entity.name.label.statement-number.fortran
c     ^^^^^^^^ keyword.control.fortran 
c
     50 CONTINUE
c    ^^ - entity.name.label.statement-number.fortran
c       ^^^^^^^^ keyword.control.fortran
*
c<- -comment.line.fortran punctuation.definition.comment.fortran
c
      FUNCTION FNC(ARG, ARGG, ARGGH)
c     ^^^^^^^^ meta.function.declaration.fortran keyword.declaration.function.fortran
c              ^^^ meta.function.declaration.fortran entity.name.function.fortran
c                 ^ meta.function.parameters.fortran punctuation.section.parens.begin.fortran
c                  ^^^ meta.function.parameters.fortran variable.parameter.input.fortran
c                     ^ meta.function.parameters.fortran punctuation.separator.comma.fortran
c                       ^^^^ meta.function.parameters.fortran variable.parameter.input.fortran
c                           ^ meta.function.parameters.fortran punctuation.separator.comma.fortran
c                             ^^^^^ meta.function.parameters.fortran variable.parameter.input.fortran
c                                  ^ meta.function.parameters.fortran punctuation.section.parens.end.fortran
c
      FUNCTION FNC(ARG, ARGG, ARGGH, 
     *                            ARGSGH)
c    ^ meta.function.parameters.fortran punctuation.separator.continuation.fortran
c                                 ^^^^^^ meta.function.parameters.fortran variable.parameter.input.fortran     
c