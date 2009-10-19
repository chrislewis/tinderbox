val accept = "text/html,application/xhtml+xml,application/xml;q=0.9,application/xml;level=1;q=0.9,*/*;q=0.8"

class Accepted(val mimeType: String, val qvalue: Float, val parameters: Map[String, String]) {
    
  def this(mimeType: String) = this(mimeType, 1f, Map[String, String]())
  
  def withQuality(q: Float) = new Accepted(mimeType, q, parameters)
  
  def withParameters(params: Map[String, String]) = new Accepted(mimeType, qvalue, params)
  
  override def toString() = {
    getClass().getName() + "{" + mimeType + ";q=" + qvalue +
      (if(parameters isEmpty) "" else "; " + parameters) + "}"
  }
  
}

def doParseMutable(params: Seq[String]): Map[String, String] = 
  Map(params.map {
    _ split("=", 2) match {
      case Array(name, value) => (name, value)
    }
  }:_*)


def extract(mimeType: String, params: Map[String, String]) =
  new Accepted(mimeType, params.getOrElse("q", "1").toFloat, params - "q")

def extractAccepted(accept: String) =
  accept split "," map {
    _ split ";" match {
      case Array(theType) => new Accepted(theType)
      case Array(theType, q @ _ *) => extract(theType, doParseMutable(q))
    }
  }
