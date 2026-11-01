import java.nio.file.{Files, Path}
import scala.util.Try

/** A key in a VDF block. Steam is inconsistent about key case, so lookups ignore it. */
object VdfKey extends SimpleNewtype[String]
type VdfKey = VdfKey.Type

object Vdf:
  final case class Str(value: String)
  final case class Int32(value: Int)
  final case class Int64(value: Long)
  final case class Float32(value: Float)

  /** A value in a block: either a nested block or a leaf of type `L`. Tagged, so matching recovers the exact type. */
  enum Value[+L]:
    case Nested(block: Block[L])
    case Leaf(value: L)

    def asBlock: Option[Block[L]] = this match
      case Nested(block) => Some(block)
      case Leaf(_)       => None

    def asLeaf: Option[L] = this match
      case Nested(_)   => None
      case Leaf(value) => Some(value)

  /** A block of key/value pairs. Covariant, so a block with fewer kinds of leaves can be used where one with more
    * is expected.
    */
  final case class Block[+L](entries: Map[VdfKey, Value[L]])

  /** Text VDF (libraryfolders.vdf, localconfig.vdf, ...) only has strings. */
  type TextLeaf = Str
  /** Binary VDF (appinfo.vdf) has everything text VDF has, plus typed numbers. */
  type BinaryLeaf = TextLeaf | Int32 | Int64 | Float32

  type TextBlock = Block[TextLeaf]
  type BinaryBlock = Block[BinaryLeaf]

  // Every text block is a valid binary block.
  summon[TextBlock <:< BinaryBlock]

extension [L](b: Vdf.Block[L])
  /** Looks up a direct child, matching keys case-insensitively (Steam is inconsistent about case). */
  def get(key: String): Option[Vdf.Value[L]] =
    b.entries.get(VdfKey(key))
      .orElse(b.entries.collectFirst { case (k, v) if k.unwrap.equalsIgnoreCase(key) => v })
  /** Follows a path of keys through nested blocks. */
  def at(path: String*): Option[Vdf.Value[L]] =
    path.foldLeft(Option[Vdf.Value[L]](Vdf.Value.Nested(b)))((value, key) => value.flatMap(_.asBlock).flatMap(_.get(key)))
  def block(path: String*): Option[Vdf.Block[L]] = at(path*).flatMap(_.asBlock)
  def leaf(path: String*): Option[L] = at(path*).flatMap(_.asLeaf)
  /** Child blocks with their keys, ignoring leaf values. */
  def childBlocks: List[(VdfKey, Vdf.Block[L])] =
    b.entries.toList.flatMap((key, value) => value.asBlock.map(key -> _))
  /** Leaf values directly in this block. */
  def leaves: List[L] = b.entries.values.toList.flatMap(_.asLeaf)

// Leaf accessors are defined on BinaryBlock, so they cover every leaf kind exhaustively; TextBlocks get them through
// covariance.
extension (b: Vdf.BinaryBlock)
  def str(path: String*): Option[String] = b.leaf(path*).flatMap(asString)
  /** String values directly in this block. */
  def strings: List[String] = b.leaves.flatMap(asString)
  /** A whole number, whether stored as a binary integer or as text. */
  def long(path: String*): Option[Long] = b.leaf(path*).flatMap {
    case Vdf.Int32(i)   => Some(i.toLong)
    case Vdf.Int64(l)   => Some(l)
    case Vdf.Str(s)     => s.toLongOption
    case Vdf.Float32(_) => None
  }

def asString(leaf: Vdf.BinaryLeaf): Option[String] = leaf match
  case Vdf.Str(s)                                  => Some(s)
  case Vdf.Int32(_) | Vdf.Int64(_) | Vdf.Float32(_) => None

/** Minimal parser for Steam's text VDF format. */
def parseTextVdf(s: String): Vdf.TextBlock =
  var i = 0
  def skipWs(): Unit =
    while i < s.length && (s(i).isWhitespace || s.startsWith("//", i)) do
      if s(i) == '/' then while i < s.length && s(i) != '\n' do i += 1
      else i += 1
  def token(): String =
    if s(i) == '"' then
      val sb = StringBuilder()
      i += 1
      while i < s.length && s(i) != '"' do
        if s(i) == '\\' && i + 1 < s.length then
          sb += (s(i + 1) match { case 'n' => '\n'; case 't' => '\t'; case c => c })
          i += 2
        else
          sb += s(i)
          i += 1
      i += 1
      sb.result()
    else
      val start = i
      while i < s.length && !s(i).isWhitespace && s(i) != '{' && s(i) != '}' do i += 1
      s.substring(start, i)
  def block(): Vdf.TextBlock =
    val b = Map.newBuilder[VdfKey, Vdf.Value[Vdf.TextLeaf]]
    skipWs()
    while i < s.length && s(i) != '}' do
      val k = VdfKey(token())
      skipWs()
      if i < s.length && s(i) == '{' then
        i += 1
        b += k -> Vdf.Value.Nested(block())
        i += 1 // closing brace
      else b += k -> Vdf.Value.Leaf(Vdf.Str(token()))
      skipWs()
    Vdf.Block(b.result())
  block()

def readTextVdf(file: Path): Option[Vdf.TextBlock] =
  if !Files.isRegularFile(file) then None else Try(parseTextVdf(Files.readString(file))).toOption
