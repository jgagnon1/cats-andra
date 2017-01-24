/*
 * Copyright 2017 Jerome Gagnon
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.jgagnon.cats.andra

import com.jgagnon.cats.andra.interpreters.ScalaMapInterpreter

/**
  * Sample application showing the use of Operations using the Free Monad backend by multiple Interpreters
  */
object Application extends App {

  import Operations._

  def program: Storable[Option[Int]] =
    for {
      _ <- put("alpha", 2)
      _ <- put("beta", 5)
      _ <- update[Int]("alpha", _ + 1)
      _ <- delete("beta")
      u <- get[Int]("alpha")
    } yield u

  val interpreter = ScalaMapInterpreter.pureCompiler

  val finalValue = program.foldMap(interpreter).run(Map.empty).value

  println(finalValue)

}
