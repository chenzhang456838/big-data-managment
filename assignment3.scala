import java.io.File
import scala.xml.XML
import scalaj.http._
import scala.xml.Elem
import scala.xml.Node
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s.Xml.toJson

object CaseIndex {
  // get the files under the given folder
  def getListOfFiles(dir: String):List[File] = {
      val d = new File(dir)
      if (d.exists && d.isDirectory) {
          d.listFiles.filter(_.isFile).toList
      } else {
          List[File]()
      }
  }

  // add a node into the xml node
  def addNode(to: Node, newNode: Node) = to match {
   case Elem(prefix, label, attributes, scope, child@_*) => Elem(prefix, label, attributes, scope, child ++ newNode: _*)
   case _ => println("could not find node"); to
  }

  // process a string for NER properties, and return it and its NER, if its NER is "location", "person" or "organization"
  def processLine(input: String, existing_res: Map[String, Set[String]]): Map[String, Set[String]]  = {
  
      var locations:Set[String] = Set() ++  existing_res("location")
      var persons:Set[String] = Set() ++  existing_res("person")
      var organizations:Set[String] = Set() ++  existing_res("organization")
  
      var res= Http("http://localhost:9000/").param("annotators","ner").param("outputFormat","xml").postData(input).header("Content-Type", "application/json").asString
      val xml = XML.loadString(res.body)
  
      for(one <- (xml\\"root"\\"document"\\"sentences"\\"sentence"\\"tokens"\\"token")) {
        //println((one\\"word").text + " => " + (one\\"NER").text)
        if ((one\\"NER").text == "LOCATION"){
          locations += (one\\"word").text
        }
        if ((one\\"NER").text == "PERSON"){
          persons += (one\\"word").text
        }
        if ((one\\"NER").text == "ORGANIZATION"){
          organizations += (one\\"word").text
        }
      }
  
      var result:Map[String, Set[String]] = Map()
      result += ("location" -> locations)
      result += ("person" -> persons)
      result += ("organization" -> organizations)
  
      result
  }

 def main(args: Array[String]) {
   
    // get the files under the folder
    val files = getListOfFiles(args(0))
    
    // delete the index if it exists
    Http("http://localhost:9200/legal_idx?pretty").method("DELETE").asString
    
    // create the index
    Http("http://localhost:9200/legal_idx?pretty").method("PUT").asString
    
    // define the index mapping
    Http("http://localhost:9200/legal_idx/cases/_mapping?pretty").postData("{\"cases\": {\"properties\": {\"name\": {\"type\": \"text\"},\"AustLII\": {\"type\": \"text\"},\"catchphrase\": {\"type\": \"text\"},\"sentence\": {\"type\": \"text\"},\"location\": {\"type\": \"text\"},\"person\": {\"type\": \"text\"},\"organization\": {\"type\": \"text\"} }}}").header("Content-Type", "application/json").asString
    
    // process each file in the folder
    for(file <-files){
        if (file.toString.endsWith("xml")){
          // read files as xml
          var x = XML.loadFile(file).asInstanceOf[scala.xml.Node]
    
          // map for NER mapping 
          var words: Map[String, Set[String]] = Map()
    
          var list = List("location", "person", "organization")
          for (ner <- list)
            words += (ner -> Set())
    
          // process the text of each xml element with NLP for NER
          for (name <- (x\\"case"\\"name") )
            words = processLine(name.text, words)
          for (AustLII <- (x\\"case"\\"AustLII") )  
            words = processLine(AustLII.text, words)
          for (catchphrase <- (x\\"case"\\"catchphrases"\\"catchphrase") )  
            words = processLine(catchphrase.text, words) 
          for (sentence <- (x\\"case"\\"sentences"\\"sentence") )  
            words = processLine(sentence.text, words) 
        
          // for all the words with NER "location", "person" or "organization", add them into the xml
          for (ner <- list){
            if (words(ner).size > 0){
              var words_str = "";
              for (elem <- words(ner))
                words_str = words_str + elem + " "
              words_str = words_str.replace("&", "&amp;")
              // construct the new node
              val newNode = XML.loadString("<" + ner + ">" + words_str + "</" + ner + ">")
              // add the new node
              x = addNode(x, newNode)
            }
          }
        
          // convert to json string
          var jsonStr = compact(render(toJson((x\\"case"))))
    
          // only keep what's inside the "case" property
          jsonStr = jsonStr.substring(jsonStr.indexOf(":") + 1, jsonStr.length - 1)
    
          // get the file name, which will be used as the id
          var array = file.toString.split("/")
          var fileName = array(array.size - 1)
    
          Http("http://localhost:9200/legal_idx/cases/" + fileName + "?pretty").postData(jsonStr).header("Content-Type", "application/json").asString
        }
		}
	}
}


