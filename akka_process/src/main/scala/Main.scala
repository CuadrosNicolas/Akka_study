


import scala.meta.{Pkg, _}
import scala.util.matching.Regex
import java.io.{BufferedWriter, File, FileWriter}

import scala.collection.mutable.ListBuffer

import scala.collection.immutable.Map;

import scala.math.{max}
class fileElement(val project:String,val file:String,val name:String,val body:String){
  override def toString(): String = {
    project+" "+file+" "+name+" "
  }
}
class Project(val id:String,val name:String,val path:String,val link:String)
{
  override def toString(): String = {
    id+" "+name+" "+path+" "+link
  }
}

class ProjectInfo(val p:Project,val nbActor : Int,val nbSystem:Int,val nbSpawn : Int,val nbFile:Int,
      val scalaTest:Boolean,val akkaTestKit : Boolean,val nShould:Int,val nMust:Int,
      val persistent:Boolean,val atLeastOnce:Boolean,depth:Integer,fail:Boolean)
{
  override def toString(): String = {
   p.toString()+" "+nbActor.toString()+" "+nbSystem+" "+nbSpawn.toString()+" "+nbFile+" "+
   scalaTest+" "+akkaTestKit+" "+nShould+" "+nMust+" "+(nShould+nMust)+
   " "+persistent+" "+atLeastOnce+" "+depth+" "+fail+"\n"
  }
}

object Main extends App {
  def getFolders: Array[Project] =
  {
    val path = java.nio.file.Paths.get("", "./list.csv")
    val bytes = java.nio.file.Files.readAllBytes(path)
    new String(bytes, "UTF-8").split("\n").map(l=>
    {
      val arr : Array[String] = l.split(" ")
      new Project(arr(0),arr(2),arr(1),arr(3))
    })
  }

  def collectFile(f: File, r: Regex,dir:String="",inDir:Boolean=false): Array[File]=
  {
    var inDir_ = false
    val these = f.listFiles
    if( (f.getName.contains(dir) && f.isDirectory) || inDir)
        inDir_ = true
    if(these==null)
      return Array[File]()

    var good  : Array[File] = Array()
    if(inDir_ || dir=="")
      good = these.filter(f => r.findFirstIn(f.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(collectFile(_,r,dir,inDir_))
  }

  def getExtendedClass(files : Array[File],baseClass : List[String])=
  {
    var out : ListBuffer[String] = new ListBuffer();
    var classMapping : Map[String,ListBuffer[String]] = Map();
    out = out ++=  baseClass;
    files.foreach(f=>{

      try {
        val path = java.nio.file.Paths.get("", f.getPath)
        val bytes = java.nio.file.Files.readAllBytes(path)
        val text = new String(bytes, "UTF-8")
        val input = Input.VirtualFile(path.toString, text)

        input.parse[Source].get.collect({
          case Defn.Class(a,name,c,d,e)=> {
            classMapping = classMapping + (name.toString() -> new ListBuffer[String]());
            e.collect({
              case Init(a) => {
                classMapping = classMapping + (name.toString() -> (classMapping(name.toString()) += a._1.toString()) );
              }
            })
          }})

      }
      catch
        {
          case a => {
          println("\tError : "+a.toString())

            List()
          }
        }

    })
    var alreadySeen : ListBuffer[String] = new ListBuffer()
    var extending  :  Map[String,Boolean] =Map();
    var Objectives : List[String] = baseClass;
    def follow(begin:String):Boolean={
        var result = false;
        if(classMapping.exists(_._1==begin) && !alreadySeen.contains(begin))
        {
          for(clazz <- classMapping(begin))
          {
            if(Objectives.contains(clazz))
            {
              result = true;
              extending = extending + (begin -> true)
            }
            else{
              alreadySeen = alreadySeen += clazz;
              result = result || follow(clazz);
              if(result)
                extending = extending + (begin -> true)
            }
          }
        }
        result;
    }
    for((clazz,_) <- classMapping)
    {
      if(!alreadySeen.contains(clazz))
      {
        follow(clazz)
      }
    }
    for(s <- extending.filterKeys(k=>extending(k)).keySet){
      out = out += s
    }
    out;
  }

  def collectTest(f:File,project:String,id:String): (Boolean,Boolean,Int,Int) =
  {
      var scTest = false;
      var akTest = false;
      var nMust = 0;
      var nShould = 0;

    try {

      val path = java.nio.file.Paths.get("", f.getPath)
      val bytes = java.nio.file.Files.readAllBytes(path)
      val text = new String(bytes, "UTF-8")
      val input = Input.VirtualFile(path.toString, text)

      input.parse[Source].get.collect({
        case Importer(a) if a.toString().contains("scalatest")=>{
              scTest = true;
        }
        case Importer(a) if a.toString().contains("akka.testkit")=>{
          akTest = true;
        }
        case Term.ApplyInfix(l,o,r,b) if o.toString().contains("must")=>{
          nMust += 1
        }
        case Term.ApplyInfix(l,o,r,b) if o.toString().contains("should")=>{
          nShould +=1
        }
        })

    }
    catch
      {
        case a => {
          println("\tError : "+a.toString())

          List()
        }
      }
    (scTest,akTest,nShould,nMust)
  }

  def collectSpawn(f:File,project:Project,acceptedActorClass:ListBuffer[String],
    acceptedSystemClass:ListBuffer[String],_spawnList : Map[String,ListBuffer[String]] ) = {
    var spawnList : Map[String,ListBuffer[String]] = _spawnList;
    var fail = false;
    try {
      val path = java.nio.file.Paths.get("", f.getPath)
      val bytes = java.nio.file.Files.readAllBytes(path)
      val text = new String(bytes, "UTF-8")
      val input = Input.VirtualFile(path.toString, text)
      def testInit(list:ListBuffer[String],init:String):Boolean={
        list.exists((p)=>init.contains(p))
      }
      def addType(parent:String)={
              if(!(spawnList isDefinedAt parent))
                  spawnList = spawnList + (parent -> new ListBuffer[String]());
      }
      def addActorSpawn(parent:String,child:String)={
        if(!(spawnList(parent).exists((name)=>name==child)))
          spawnList = spawnList + (parent-> (spawnList(parent) += child) );
      }
      input.parse[Source].get.collect({
        case Defn.Class(a,name,c,d,e)=> {
          e.collect({
            case Init(a) if testInit(acceptedActorClass,a.toString())
               || testInit(acceptedSystemClass,a.toString()) => {
              addType(name.toString())
              e.collect({
                case apply : Term.Apply =>{
                    apply.fun match {
                      case fun : Term.Select if(fun.name.toString()=="actorOf") =>{
                        apply.args(0) match {

                          case selectProps : Term.Select if(selectProps.name.value.toString() == "props") =>{
                            addActorSpawn(name.toString(),selectProps.qual.toString());
                          }
                          case applyTypeProps : Term.ApplyType =>{
                            try{
                              val fun = applyTypeProps.fun.asInstanceOf[Term.Name];
                              val t = applyTypeProps.targs(0).asInstanceOf[Type.Name];
                              if(fun.toString()=="Props")
                              {
                                addActorSpawn(name.toString(),t.value.toString())
                              }
                            }
                            catch{
                              case _ =>{
                                    println("Error ApplyType : "+apply.args(0))

                              }
                            }
                          }
                          case applyNew : Term.New =>{
                            addActorSpawn(name.toString(),applyNew.init.tpe.toString())
                          }
                          case applyNew : Term.Apply =>{
                              applyNew.fun match {
                                case select : Term.Select=>{
                                  if(select.name=="props")
                                  {
                                    addActorSpawn(name.toString(),select.qual.toString());
                                  }
                                }
                                case tName: Term.Name=>{
                                  if(tName.toString()=="Props")
                                  {
                                    applyNew.args(0) match {
                                      case newProps : Term.New =>{
                                        addActorSpawn(name.toString(),newProps.init.tpe.toString());
                                      }
                                      case appType : Term.ApplyType=>{
                                        if(appType.fun.toString()=="classOf")
                                        {
                                          addActorSpawn(name.toString(),appType.targs(0).toString());
                                        }
                                      }
                                      case _ =>{
                                        fail = true;
                                      }
                                    }
                                  }
                                }
                              }
                            }
                            case _ =>{
                            }
                          }

                          }
                          case _ =>{

                          }
                        }
                }
                case _=>{

                }
              }
          )
            }
          })
        }})
      }
      catch
      {
        case a => {
          println("\tError : "+a.toString())

        }
      }
      (spawnList,fail)
    }
  def computeDepthGraph(_spawnList : Map[String,ListBuffer[String]]) : Integer={

    def follow(key:String,_depth:Integer = 0,
    _alreadyVisited : Map[String,Boolean] = Map()) : Integer=
    {
      var depth = _depth
      if(!(_alreadyVisited isDefinedAt key))
      {
        var alreadyVisited = _alreadyVisited + (key->true)
        if(_spawnList isDefinedAt key)
        {
          depth += 1
          for(child <- _spawnList(key))
          {
            depth = max(depth,follow(child,_depth+1,alreadyVisited))
          }
        }
      }
      depth
    }
    var depth = 0;
    for((parent,children) <- _spawnList)
    {
      for(child <-children)
      {
        depth = max(depth,follow(child))
      }
    }
    depth
  }

  def collectClasses(f:File,project:String,id:String,acceptedActorClass:ListBuffer[String],
    acceptedSystemClass:ListBuffer[String])={


    var nbActor = 0;
    var nbSystem =0;
    var nbSpawn = 0;
    var persistent = false;
    var atLeastOnce = false;


    try {

      val path = java.nio.file.Paths.get("", f.getPath)
      val bytes = java.nio.file.Files.readAllBytes(path)
      val text = new String(bytes, "UTF-8")
      val input = Input.VirtualFile(path.toString, text)
      def testInit(list:ListBuffer[String],init:String):Boolean={
        list.exists((p)=>init.contains(p))
      }
      input.parse[Source].get.collect({
        case Defn.Class(a,name,c,d,e)=> {
          var classAnalysed = false;
          def analyseClass = {
              atLeastOnce = atLeastOnce || e.toString().contains("AtLeastOnceDelivery");
              persistent = persistent || a.toString().contains("PersistentActor");
              if(!classAnalysed)
              {
                e.collect({
                  case select :  Term.Select if select.name.toString() == "actorOf" => {
                    nbSpawn+=1;
                  }
                })
              }
              classAnalysed = true;
          }
          e.collect({
            case Init(a) if testInit(acceptedActorClass,a.toString()) => {
              nbActor+=1;
              analyseClass
            }
            case Init(a) if testInit(acceptedSystemClass,a.toString()) => {
              nbSystem+=1;
              analyseClass
            }
          })
        }})
      }
      catch
      {
        case a => {
          println("\tError : "+a.toString())

        }
      }
      (nbActor,nbSystem,nbSpawn,persistent,atLeastOnce)
  }
 var projects : ListBuffer[ProjectInfo]  = new ListBuffer();
getFolders.map(project=>{
    println("Analysing project for spawn: "+project.name)
    var nbSpawn = 0;
    var nbActor = 0;
    var nbSystem = 0;
    var nbFile = 0;
    var nShould = 0;
    var nMust = 0;
    var scalaTest = false;
    var akkaTestKit = false;
    var per = false;
    var alo = false;
    var depth = 0;
    var spawnList : Map[String,ListBuffer[String]] = Map()
    var fail = false
    var acceptedActorClass = getExtendedClass(collectFile(new File(project.path),""".*\.scala$""".r),
       List("Actor","PersistentActor"));
    var acceptedSystemClass = getExtendedClass(collectFile(new File(project.path),""".*\.scala$""".r),
       List("ActorSystem"));
    collectFile(new File(project.path),""".*\.scala$""".r).map(f=>{
        nbFile += 1;
        val (sList,_fail) = collectSpawn(f,project,acceptedActorClass,acceptedSystemClass,spawnList)
        spawnList = sList;
        val (nAct,nbSys,nSp,persistent,atLeastOnce) = collectClasses(f,project.name,project.id,acceptedActorClass,acceptedSystemClass);
        val (scalTest,akTest,nSh,nMu) = collectTest(f,project.name,project.id);
        nbSpawn += nSp;
        nbActor += nAct;
        nbSystem += nbSys;
        nShould += nSh;
        nMust += nMu;
        fail = fail || _fail;
        scalaTest = scalaTest || scalTest;
        akkaTestKit = akkaTestKit || akTest;
        per = per || persistent;
        alo = alo || atLeastOnce;
    })
    depth = computeDepthGraph(spawnList)
  projects += new ProjectInfo(project,nbActor,nbSystem,nbSpawn,nbFile,
    scalaTest,akkaTestKit,nShould,nMust,per,alo,depth,fail);
  })

  val file = new File("./Results_projects_actors.csv")
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write("id name path link nbActor nbSystem nbSpawn nbFile scalaTest akkaTest nShould nMust nTestCases persistentActor atLeastOnceDelivery depth fail\n");
  for(projects <- projects)
    {
      bw.write(projects.toString());
    }
  bw.close()

}