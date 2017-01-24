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

package com.jgagnon.cats.andra.interpreters

import cats._
import data.State

import com.jgagnon.cats.andra.Operations.StorableA

object ScalaMapInterpreter {

  import StorableA._

  // Scala Map backed interpreter
  type KVStoreState[A] = State[Map[String, Any], A]

  val pureCompiler: StorableA ~> KVStoreState =
    new (StorableA ~> KVStoreState) {
      def apply[A](fa: StorableA[A]): KVStoreState[A] =
        fa match {
          case Put(key, value) =>
            State.modify(_.updated(key, value))
          case Get(key) =>
            State.inspect(_.get(key).map(_.asInstanceOf[A]))
          case Delete(key) =>
            State.modify(_ - key)
        }
    }

}
