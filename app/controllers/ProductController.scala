package controllers

import models.Product
import play.api.libs.json.{JsValue, Json, OFormat}
import play.api.mvc.{AnyContent, BaseController, ControllerComponents, Request}

import scala.collection.mutable.ListBuffer
import javax.inject.Inject

class ProductController @Inject()(val controllerComponents: ControllerComponents) extends BaseController {

  implicit val productFormat: OFormat[Product] = Json.format[Product]

  private val productsList = ListBuffer[Product]()
  productsList += Product(1, "Gaming PC", 3000)
  productsList += Product(2, "Xbox Series X", 2500)
  productsList += Product(3, "Playstation 5", 2600)
  productsList += Product(4, "Nintendo Switch", 1400)

  def showAll() = Action { implicit request: Request[AnyContent] =>
    if (productsList.isEmpty){
      NotFound
    } else {
      Ok(Json.toJson(productsList))
    }
  }

  def showByID(id: Int) = Action {
    val itemToShow = productsList.find(_.id == id)
    itemToShow match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def update(id: Int) = Action { request: Request[AnyContent] =>
    val body: AnyContent = request.body
    val jsonBody: Option[JsValue] = body.asJson
    val productToEdit = productsList.find(_.id == id)
    productToEdit match {
      case Some(item) =>
        jsonBody
          .map { json =>
            val name = (json \ "name").as[String]
            val price = (json \ "price").as[Int]
            val newProduct = item.copy(id = id, name = name, price = price)
            productsList.update(productsList.indexOf(item), newProduct)
            Created(Json.toJson(newProduct))
          }
          .getOrElse {
            BadRequest("Expecting application/json request body")
          }
      case None => NotFound
    }
  }

  def delete(id: Int) = Action {
    val productToDelete = productsList.find(_.id == id)
    productToDelete match {
      case Some(item) =>
        productsList -= item
        Ok(Json.toJson(productsList))
      case None => NotFound
    }
  }

  def add() = Action { request: Request[AnyContent] =>
    val body: AnyContent = request.body
    val jsonBody: Option[JsValue] = body.asJson
    jsonBody
      .map { json =>
        val id = productsList.map(_.id).max + 1
        val name = (json \ "name").as[String]
        val price = (json \ "price").as[Int]
        val newProduct = Product(id, name, price)
        productsList += newProduct
        Created(Json.toJson(newProduct))
      }
      .getOrElse {
        BadRequest("Expecting application/json request body")
      }
  }
}
