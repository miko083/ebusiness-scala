package controllers

import models.Task
import play.api.libs.json.{JsValue, Json, OFormat}
import play.api.mvc.{AnyContent, BaseController, ControllerComponents, Request}

import scala.collection.mutable.ListBuffer
import javax.inject.Inject

class TaskController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  implicit val TaskFormat: OFormat[Task] = Json.format[Task]

  private val tasksList = ListBuffer[Task]()
  tasksList += Task(1, "Posprzatac pokoj")
  tasksList += Task(2, "Zrobic zakupy")
  tasksList += Task(2, "Naprawic komputer")

  def showAll() = Action { implicit request: Request[AnyContent] =>
    if (tasksList.isEmpty){
      NotFound
    } else {
      Ok(Json.toJson(tasksList))
    }
  }

  def showByID(id: Int) = Action {
    val itemToShow = tasksList.find(_.id == id)
    itemToShow match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def update(id: Int) = Action { request: Request[AnyContent] =>
    val body: AnyContent = request.body
    val jsonBody: Option[JsValue] = body.asJson
    val taskToEdit = tasksList.find(_.id == id)
    taskToEdit match {
      case Some(item) =>
        jsonBody
          .map { json =>
            val content = (json \ "content").as[String]
            val newTask = item.copy(id = id, content = content)
            tasksList.update(tasksList.indexOf(item), newTask)
            Created(Json.toJson(newTask))
          }
          .getOrElse {
            BadRequest("Expecting application/json request body")
          }
      case None => NotFound
    }
  }

  def delete(id: Int) = Action {
    val taskToDelete = tasksList.find(_.id == id)
    taskToDelete match {
      case Some(item) =>
        tasksList -= item
        Ok(Json.toJson(tasksList))
      case None => NotFound
    }
  }

  def add() = Action { request: Request[AnyContent] =>
    val body: AnyContent = request.body
    val jsonBody: Option[JsValue] = body.asJson
    jsonBody
      .map { json =>
        val id = tasksList.map(_.id).max + 1
        val content = (json \ "content").as[String]
        val newTask = Task(id,content)
        tasksList += newTask
        Created(Json.toJson(newTask))
      }
      .getOrElse {
        BadRequest("Expecting application/json request body")
      }
  }
}
