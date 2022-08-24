package controllers

import models.Movie
import play.api.libs.json.{JsValue, Json, OFormat}
import play.api.mvc.{AnyContent, BaseController, ControllerComponents, Request}

import scala.collection.mutable.ListBuffer
import javax.inject.Inject

class MovieController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  implicit val movieFormat: OFormat[Movie] = Json.format[Movie]

  private val moviesList = ListBuffer[Movie]()
  moviesList += Movie(1, "Star Wars")
  moviesList += Movie(2, "Chlopaki nie placza")
  moviesList += Movie(2, "Harry Potter")

  def showAll() = Action { implicit request: Request[AnyContent] =>
    if (moviesList.isEmpty){
      NotFound
    } else {
      Ok(Json.toJson(moviesList))
    }
  }

  def showByID(id: Int) = Action {
    val itemToShow = moviesList.find(_.id == id)
    itemToShow match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def update(id: Int) = Action { request: Request[AnyContent] =>
    val body: AnyContent = request.body
    val jsonBody: Option[JsValue] = body.asJson
    val movieToEdit = moviesList.find(_.id == id)
    movieToEdit match {
      case Some(item) =>
        jsonBody
          .map { json =>
            val title = (json \ "title").as[String]
            val newMovie = item.copy(id = id, title = title)
            moviesList.update(moviesList.indexOf(item), newMovie)
            Created(Json.toJson(newMovie))
          }
          .getOrElse {
            BadRequest("Expecting application/json request body")
          }
      case None => NotFound
    }
  }

  def delete(id: Int) = Action {
    val movieToDelete = moviesList.find(_.id == id)
    movieToDelete match {
      case Some(item) =>
        moviesList -= item
        Ok(Json.toJson(moviesList))
      case None => NotFound
    }
  }

  def add() = Action { request: Request[AnyContent] =>
    val body: AnyContent = request.body
    val jsonBody: Option[JsValue] = body.asJson
    jsonBody
      .map { json =>
        val id = moviesList.map(_.id).max + 1
        val title = (json \ "title").as[String]
        val newMovie = Movie(id,title)
        moviesList += newMovie
        Created(Json.toJson(newMovie))
      }
      .getOrElse {
        BadRequest("Expecting application/json request body")
      }
  }
}
