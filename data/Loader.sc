#! /usr/bin/env nix-shell
#! nix-shell -i amm -p ammonite -I nixpkgs="https://github.com/NixOS/nixpkgs/archive/8ecc61c91a5.tar.gz"

import $ivy.{
  `co.fs2::fs2-io:3.1.0`,
  `io.circe::circe-generic:0.14.1`,
  `org.gnieh::fs2-data-csv:1.0.1`,
  `org.gnieh::fs2-data-csv-generic:1.0.1`,
  `org.tpolecat::skunk-circe:0.2.0`
}

import $file.Domain
import $file.MovieStore

import cats.effect._
import cats.syntax.all._
import fs2.Stream
import fs2.data.csv._
import fs2.data.csv.generic.CsvName
import fs2.data.csv.generic.semiauto._
import fs2.io.file.{ Files, Path }

object Loader {
  import Domain._

  val run: IO[Unit] =
    MovieStore.make.use { store =>
      Files[IO]
        .readAll(Path("dataset/movies.csv"))
        .through(fs2.text.utf8.decode)
        .through(decodeUsingHeaders[Movie]())
        .chunkN(1000)
        .zip(Stream.iterate(0)(_ + 1))
        .parEvalMap(10) {
          case (c, n) =>
            IO.println(s"Processing chunk #$n") >> store.persist(c)
        }
        .onFinalize(IO.println("Done!"))
        .compile
        .drain
    }
}

Loader.run.unsafeRunSync()(cats.effect.unsafe.implicits.global)
