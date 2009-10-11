import scala.xml._

trait Extractor {
  
  type X  = NodeSeq => NodeSeq
  type MX = Map[Symbol, X]
  type GM = Map[Symbol, Tuple2[X, MX]]
  
  val groupMap: GM
  
}

trait Handler {
  this: Extractor => // must be mixed into an Extractor
  
  def apply(hrXml: NodeSeq) = groupMap map {
    case (group, (groupExtractor, extractorMap)) => Map(group -> doIt1(groupExtractor(hrXml), extractorMap))
  }
  
  def doIt1(xml: NodeSeq, f: Map[Symbol, X]) = {
    xml map {
      group => Map(f flatMap {
        case (field, extract) => extract(group) map(x => (field, x.text))
      } toSeq:_*)
    }
  }
  
}

trait Dispatch {
  
  def dispatch(results: Iterable[Map[Symbol, Seq[Map[Symbol,String]]]]) = {
    results foreach(_ map handle)
  }
  
  def handle(t: Tuple2[Symbol, Seq[Map[Symbol, String]]]): Unit
  
}

trait MappedDispatcher extends Dispatch {
  
  type WONG2 = Map[Symbol, String] => Unit
  
  val handlerMap: Map[Symbol, WONG2]
  
  override def handle(t: Tuple2[Symbol, Seq[Map[Symbol, String]]]): Unit = { 
    handlerMap get(t._1) foreach {
        handler => 
        t._2.foreach(handler)
    }
  }
}

trait HrXmlExtractors extends Extractor {
  
  val employmentHistory: MX = Map(
    ('EmployerOrgName,    _ \ "EmployerOrgName"),
    ('Title,              _ \ "PositionHistory" \ "Title"),
    ('Description,        _ \ "PositionHistory" \ "Description"),
    ('StartDate,          _ \ "PositionHistory" \ "StartDate"),
    ('FAKE,               _ \ "Sausage" \ "Knockwurst")
  )
  
  val educationHistory: MX = Map(
    ('SchoolName,         _ \ "SchoolName"),
    ('DegreeMajor,        _ \ "DegreeMajor" \ "Name"),
    ('DatesOfAttendance,  _ \ "DatesOfAttendance" \ "StartDate"),
    ('PositionHistory,    _ \ "PositionHistory" \ "EndDate")
  )
  
  
  val groupMap = Map[Symbol, Tuple2[X, MX]](
    'EmploymentHistory -> (_ \ "EmploymentHistory" \ "EmployerOrg", employmentHistory),
    'EducationHistory ->  (_ \ "EducationHistory" \ "SchoolOrInstitution", educationHistory)
  )
  
}

object h2 extends HrXmlExtractors with Handler with MappedDispatcher {
  
  def tryIt(xml: NodeSeq) = dispatch(apply(xml))
  
  val handleEducation = (edu: Map[Symbol, String]) =>
    edu foreach {
        case ('SchoolName, s) => println("school: " + s)
        case ('FAKE, s) => println("FAKE!: " + s)
        case _ => ()
    }
    
  override val handlerMap: Map[Symbol, WONG2] = Map('EducationHistory -> handleEducation)
}
