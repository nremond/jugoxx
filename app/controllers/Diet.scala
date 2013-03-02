package controllers

import play.api._
import play.api.mvc._
import play.api.libs.json._

// you need this import to have combinators
import play.api.libs.functional.syntax._
import play.api.libs.json.util._

object Diet extends Controller {

	case class Thing(name:String, value:Int)

	def subsetSum(things:List[Thing]) = {
		val (allPos, allNegs) = things.partition(_.value>0)
		val pos = allPos.foldLeft(0)( (sum, t) => t.value + sum)
		val neg = allNegs.foldLeft(0)( (sum, t) => t.value + sum)
		val zeroIndex = -neg


		def _subsetSum(sums:Array[List[Thing]], things:List[Thing]) : List[Thing] = {
			
			things match {
				case t :: otherThings => {
					val s = sums.clone

					if(s(t.value+zeroIndex).isEmpty) 
						s(t.value+zeroIndex) = t :: Nil

					for((theseThings, i) <- sums.zipWithIndex if( !theseThings.isEmpty && s(i+t.value).isEmpty))
						 s(i+t.value) = t :: theseThings
				
					val zeroSums = s(zeroIndex)
					if(! zeroSums.isEmpty) {
						zeroSums
					} else {
						_subsetSum(s, otherThings)
					}
				}
				case Nil => Nil
			}		 
		}

		val initialSums = Array.fill[List[Thing]](pos - neg + 1)(Nil)
		_subsetSum(initialSums, things)
	}


	// or in a simpler way as case class has a companion object with an apply function
	implicit val thingReads = (
	  (__ \ "name").read[String] and
	  (__ \ "value").read[Int] 
	)(Thing)


	def resolve = Action(parse.json) { request =>
		request.body.validate[List[Thing]].map {
			case things => 
				Logger.info(s"Trying to find a subset for these things : $things")
				val t1 = System.currentTimeMillis
				val subsetThings = subsetSum(things)
				val t = System.currentTimeMillis - t1
				val solution = if(!subsetThings.isEmpty) subsetThings.map(_.name) else List("no solution")
				Logger.info(s"Computed the subest sum in $t ms and the solution is $solution")
				Ok(Json.toJson(solution))
		}
		.recoverTotal{
			e => {
				val error = JsError.toFlatJson(e)
				Logger.error(s"Detected error: $error")
				BadRequest(s"Detected error: $error")
			}
		}
	} 

}