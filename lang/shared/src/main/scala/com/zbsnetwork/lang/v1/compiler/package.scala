package com.zbsplatform.lang.v1

import cats.implicits._
import com.zbsplatform.lang.v1.task.TaskM
import com.zbsplatform.lang.v1.task.imports._

package object compiler {
  type CompileM[A] = TaskM[CompilerContext, CompilationError, A]

  implicit class EiExt[A](ei: Either[CompilationError, A]) {
    def toCompileM: CompileM[A] =
      ei.fold(raiseError, _.pure[CompileM])
  }
}
