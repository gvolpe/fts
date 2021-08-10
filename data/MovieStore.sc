import $file.DB
import $file.Domain

import java.time.ZoneOffset

import cats.effect._
import fs2.Chunk
import io.circe.syntax._
import natchez.Trace.Implicits.noop
import skunk._
import skunk.circe.codec.all._
import skunk.codec.all._
import skunk.implicits._

trait MovieStore[F[_]] {
  def persist(movies: fs2.Chunk[Domain.Movie]): F[Unit]
}

import Domain._

def make: Resource[IO, MovieStore[IO]] =
  Session
    .pooled[IO](
      host = DB.config.host,
      port = DB.config.port,
      user = DB.config.user,
      password = DB.config.password,
      database = DB.config.database,
      max = DB.config.max
    )
    .evalTap {
      _.use {
        _.unique(sql"select version();".query(text)).flatMap { v =>
          IO.println(s"Connected to Postgres $v")
        }
      }
    }
    .map { pool =>
      new MovieStore[IO] {
        def persist(movies: Chunk[Movie]): IO[Unit] = {
          val xs = movies.toList
          pool.use(_.prepare(SQL.insertMovies(xs)).use(_.execute(xs))).void
        }
      }

    }

object Codecs {
  val currency: Encoder[Currency] = jsonb.contramap(_.asJson)

  val durationMins: Codec[DurationMinutes] = int8.imap(DurationMinutes.apply)(_.value)

  val publishingDate: Codec[PublishingDate] =
    timestamptz.imap(d => PublishingDate(d.toInstant()))(_.value.atOffset(ZoneOffset.UTC))

  val year: Codec[Year] = int4.imap(Year.apply)(_.value)

  val movie: Encoder[Movie] =
    (varchar ~ varchar ~ varchar ~ year.opt ~ publishingDate.opt ~ varchar ~ durationMins ~ varchar.opt ~ varchar.opt ~ varchar.opt ~ varchar.opt ~ varchar.opt ~ varchar.opt ~ varchar.opt ~ float8 ~ int4 ~ currency.opt ~ varchar.opt ~ varchar.opt ~ float8.opt ~ float8.opt ~ float8.opt)
      .contramap { m =>
        m.id ~ m.title ~ m.original_title ~ m.year ~ m.date_published ~ m.genre ~ m.duration ~ m.country ~ m.language ~ m.director ~ m.writer ~ m.production_company ~ m.actors ~ m.description ~ m.avg_vote ~ m.votes ~ m.budget ~ m.usa_gross_income ~ m.worlwide_gross_income ~ m.metascore ~ m.reviews_from_users ~ m.reviews_from_critics
      }
}

object SQL {
  def insertMovies: List[Movie] => Command[List[Movie]] =
    xs => sql"""
              INSERT INTO movies
              VALUES ${Codecs.movie.values.list(xs.length)}
              ON CONFLICT DO NOTHING
             """.command
}
