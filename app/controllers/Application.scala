package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  
  def index = Action {
  	implicit request => {
		request.queryString.get("q").map(_.mkString).map(
			q => q match {
				case "Quelle est ton adresse email" => Ok("nremond+jugoxx@gmail.com")
				case "Es tu abonne a la mailing list(OUI/NON)" => Ok("OUI")
				case "Es tu heureux de participer(OUI/NON)" => Ok("NON")
				case "Es tu pret a recevoir une enonce au format markdown par http post(OUI/NON)" => Ok("OUI")
				case "As tu bien recu le premier enonce(OUI/NON)" => Ok("OUI")
				case "As tu trouve le dernier exercice difficile(OUI/NON)" => Ok("NON")
				case "As tu bien recu le deuxieme enonce(OUI/NON)" => Ok("OUI")
				case "Veux tu tenter ta chance pour gagner un des prix(reserve aux membres du JUGL)(OUI/NON)" => Ok("OUI")
				case _ => NotImplemented(s"""Sorry, but I can't answer the following question: "${q}".""")
			}
		) getOrElse(BadRequest("Ask me a question."))
  	}
  }
  
	def enonce(id:Int) = Action { request =>
		val body: AnyContent = request.body
		val textBody: Option[String] = body.asText 

		// Expecting text body
		textBody.map { text => {
				Logger.info(s"here is the test: ${text}")
				Ok("Ok, let me think")
			}
		}.getOrElse {
			BadRequest("Expecting text/plain request body")  
		}
	}
}