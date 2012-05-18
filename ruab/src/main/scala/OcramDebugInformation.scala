package ch.ethz.inf.vs.ruab {

import com.google.gson._
import java.lang.reflect.Type
import scala.collection.JavaConversions._

class OcramDebugInformation(val debugfile: String)
  extends DebugInformation
{
  class LocationDeserializer extends JsonDeserializer[Location] {
    override def deserialize(json: JsonElement, typeOfT: Type, context: JsonDeserializationContext): Location = {
      val elems = json.getAsJsonArray().map(_.getAsInt()).toArray
      val tid = if (elems.length == 5) None else Some(elems(5))
      new Location(new TLocation(elems(0), elems(1), elems(2)), new ELocation(elems(3), elems(4), tid))
    }
  }
  var info: Information = null

  override def load(): Information = {
    if (info == null) {
      val jsonstr = Utils.readFile(debugfile)
      val gson = new GsonBuilder().registerTypeAdapter(classOf[Location], new LocationDeserializer()).create()
      info = gson.fromJson(jsonstr, classOf[Information])
    }
    info
  }
  
}

}
