
class Pos private ( val prog : String , val offs : Int , val line : Int , val col : Int ) {
  def this (prog : String) = this (prog , 0, 1, 1)
  def ch = if (offs == prog.length) -1 else prog(offs).toInt
  def inc = ch match {
    case '\n' => new Pos(prog,offs +1,line +1,1)
    case -1 => this
    case _ => new Pos(prog,offs +1,line,col +1)
  }
  override def toString = "(" + line + "," + col + ")"

  def isLetter = if (ch >= 'a' && ch <= 'z') true else false

  def isDigit = if (ch >= '0' && ch <= '9') true else false
}

object DomainTags extends Enumeration {
  type Tag = Value
  val WHITESPACE , IDENT, END_OF_PROGRAM, KEYWORD, COMMENT, UNKNOWN = Value
}
import DomainTags._

class Scanner {

  def scan (start: Pos): (Tag,Pos) =
    sys.error("syntax error at " + start)

}

class Token ( val start: Pos , scanner: Scanner) {
  val (tag: Tag, follow: Pos) = start.ch match {
      case -1 => (END_OF_PROGRAM, start)
      case _ => scanner.scan(start)
    }

  def image = start.prog.substring(start.offs,follow.offs)
  def next = new Token(follow,scanner)

  override def toString=
    tag.toString + " " + start + "-" + follow + " : " + image  + "\n"
}

trait Whitespaces extends Scanner {
  private def missWhitespace (pos: Pos): Pos = pos.ch match {
    case ' ' | '\t' | '\n'  => missWhitespace (pos.inc)
    case _                  => pos
  }

  override def scan (start: Pos) = {
    val follow = missWhitespace ( start )
    if (start != follow) (WHITESPACE,follow)
    else super.scan(start)
  }
}
trait Comments extends Scanner{
  private def scanComment(pos: Pos, flag: Boolean) : (Pos,Boolean) = {
    (pos,flag) match {

        case (p,f) if ("\t" contains(p.ch)) || (p.ch == -1)  =>(p,false)
        case (p,f) if ("\n" contains(p.ch)) =>(p, true)
        case (p,f) => scanComment(p.inc,f)
    }

  }
  override def scan(start: Pos) = {
    if (start.ch == '/' && start.inc.ch == '/') {
        val followComment = scanComment(start, false)
        if (followComment._2)
          (COMMENT, followComment._1)
        else {
          val add_error = " \n Error: Invalid COMMENT! Was found 'end of program' "
          (COMMENT, followComment._1)
        }
      }
    else super.scan(start)
  }
}

trait Idents extends Scanner {
  private def scanIdent(pos: Pos, flag: Boolean): (Pos, Boolean) = {
    (pos, flag) match {
      case _ if pos.ch == '/' => (pos.inc, false)
      case (p, f) if p.isDigit || ("\n" contains(p.ch)) || (pos.ch== ' ') => scanIdent(pos.inc, f)
      case (p, f) if p.isLetter || ("\t" contains(p.ch)) || (p.ch == -1)  => scanIdent(pos.inc, f)
      case _ => (pos, true)
    }
  }

  override def scan(start: Pos) = {
    val add_error = "\nError: Invalid IDENT!"
    if (start.ch == '/' ) {
      val follow = scanIdent(start.inc, true)
      if (follow._2 == false) {
        val image = start.prog.substring(start.offs, follow._1.offs).toLowerCase()
        if (image.equals("/while/") || image.equals("/do/") || image.equals("/end/"))
          (KEYWORD, follow._1)
        else if (image.contains("while") || image.contains("end")  || image.contains("do") ){
          val add = "\nError: Invalid KEYWORD!"
          (IDENT, follow._1)
        } else
          (IDENT, follow._1)
      } else {
        val add = "\nError: Invalid IDENT! set at ending = '/' in IDENT"
        (IDENT, follow._1)
      }

    } else
    if (start.isDigit|| start.isLetter) {
      val followis = scanIdent(start.inc, true)
      if (followis._2 == false) {
      
        (IDENT, followis._1)
      }
      else {

        (IDENT, followis._1)
      }
    } else
      super.scan(start)
  }
}

trait Unknown extends Scanner {
  override def scan(start: Pos) = {
    (UNKNOWN, start.inc)
  }
}
object lexerScala {
  def main(args: Array[String]): Unit = {

    val t = new Token(
      new Pos("_*//папапа /end/\n/sdsdsd/ sdsds/ \n//end   dffs//end//end/dfsfs314***while /sdj af***jsw3\n42424/ //sdcdscs\n//"),
      new Scanner
        with Unknown
        with Idents
        with Comments
        with Whitespaces

    )
    lazy val tokens: Stream[Token] = t #:: tokens.map(_.next)
    println(tokens.takeWhile(_.tag != END_OF_PROGRAM).mkString("\n"))
  }
}
