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

import cats._
import free._
import Free._

object Operations {

  // Operation ADT for StorableA
  sealed trait StorableA[A]

  object StorableA {

    final case class Put[T](key: String, value: T) extends StorableA[Unit]

    final case class Get[T](key: String) extends StorableA[Option[T]]

    final case class Delete(key: String) extends StorableA[Unit]

  }

  import StorableA._

  // Lift ADT Operations StorableA[_] into Storable[_] free monad
  type Storable[A] = Free[StorableA, A]

  def put[T](key: String, value: T): Storable[Unit] =
    liftF[StorableA, Unit](Put(key, value))

  def get[T](key: String): Storable[Option[T]] =
    liftF[StorableA, Option[T]](Get(key))

  def delete[T](key: String): Storable[Unit] =
    liftF[StorableA, Unit](Delete(key))

  def update[T](key: String, f: T => T): Storable[Unit] =
    for {
      mV <- get[T](key)
      _  <- mV.map(v => put(key, f(v))).getOrElse(Free.pure(()))
    } yield ()
}
