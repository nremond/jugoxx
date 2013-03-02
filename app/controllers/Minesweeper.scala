package controllers

import play.api._
import play.api.mvc._

object Minesweeper extends Controller {

	private def countNeighbourMines(i:Int, j:Int, mines:Array[Array[Char]]) = {
		val s = for {x <- Range(-1,2) if(x+i>=0 && x+i<mines.length)
			 		 y <- Range(-1,2) if(y+j>=0 && y+j<mines(x+i).length && !(x==0 && y==0))}
			 			yield mines(i+x)(y+j)
		s.count(_=='*')
	}


	private def countMines(input:String) = {
		val size = input.split("\n").head.split(" ").map(_.toInt)
		require(size.length == 2)
		val h = size(0).toInt
		val w = size(1).toInt

		val mines = input.split("\n").tail.map(_.toCharArray)
		
		val positions = for(i <- Range(0, h); j <- Range(0, w)) yield (i,j)

		val solved = positions.map {
			case (i,j) => 
				if( mines(i)(j) == '*') "*"
				else countNeighbourMines(i,j, mines).toString
		} grouped(w)

		solved.map(_.mkString).mkString("\n")
	}


	def resolve = Action { request =>
		val body: AnyContent = request.body
		val textBody: Option[String] = body.asText 

		// Expecting text body
		textBody.map { text => {
				Logger.info(s"got the following problem:\n${text}")
				val result = countMines(text)
				Logger.info(s"proposing the following solution:\n${result}")
				Ok(result)
			}
		}.getOrElse {
			BadRequest("Expecting text/plain request body")  
		}
	}
}