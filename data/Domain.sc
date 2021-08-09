import java.time.Instant

import cats.syntax.all._
import fs2.data.csv._
import fs2.data.csv.generic.CsvName
import fs2.data.csv.generic.semiauto._

case class Year(value: Int)               extends AnyVal
case class PublishingDate(value: Instant) extends AnyVal
case class DurationMinutes(value: Long)   extends AnyVal

case class Currency(name: String, amount: Long)
object Currency {
  implicit val jsonEncoder: io.circe.Encoder[Currency] = io.circe.generic.semiauto.deriveEncoder
}

case class Movie(
    @CsvName("imdb_title_id") id: String,
    title: String,
    original_title: String,
    year: Option[Year],
    date_published: Option[PublishingDate],
    genre: String,
    duration: DurationMinutes,
    country: Option[String],
    language: Option[String],
    director: Option[String],
    writer: Option[String],
    production_company: Option[String],
    actors: Option[String],
    description: Option[String],
    avg_vote: Double,
    votes: Int,
    budget: Option[Currency],
    usa_gross_income: Option[String],
    worlwide_gross_income: Option[String],
    metascore: Option[Double],
    reviews_from_users: Option[Double],
    reviews_from_critics: Option[Double]
)

implicit val publishingDateDecoder: CellDecoder[Option[PublishingDate]] =
  CellDecoder.instantDecoder.map(x => Some(PublishingDate(x))).or(CellDecoder.const(None))

// One of the values is "TV Movie 2019"
implicit val yearDecoder: CellDecoder[Year] =
  CellDecoder.intDecoder.map(Year.apply).or {
    CellDecoder.fromString(_.dropWhile(!_.isDigit).trim.toInt).map(Year.apply)
  }

implicit val durationMinsDecoder: CellDecoder[DurationMinutes] = deriveCellDecoder

implicit val currencyDecoder: CellDecoder[Currency] =
  CellDecoder
    .instance[Currency] { s =>
      Either
        .catchNonFatal {
          val values = s.trim.split(" ").toList
          Currency(values(0).replace("$", "USD"), values(1).toLong)
        }
        .leftMap(e => new DecoderError(s"Invalid currency: ${e.getMessage}"))
    }

implicit val movieDecoder: CsvRowDecoder[Movie, String] = deriveCsvRowDecoder
