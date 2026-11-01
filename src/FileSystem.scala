import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*
import scala.util.{Try, Using}

def listDir(dir: Path): List[Path] =
  if !Files.isDirectory(dir, LinkOption.NOFOLLOW_LINKS) then Nil
  else Using.resource(Files.list(dir))(_.iterator.asScala.toList.sortBy(_.getFileName.toString))

/** Total size of regular files in a tree, not following symlinks. */
def treeSize(path: Path): Bytes =
  var total = 0L
  Files.walkFileTree(
    path,
    new SimpleFileVisitor[Path]:
      override def visitFile(f: Path, attrs: BasicFileAttributes) =
        if attrs.isRegularFile then total += attrs.size
        FileVisitResult.CONTINUE
      override def visitFileFailed(f: Path, e: java.io.IOException) = FileVisitResult.CONTINUE
  )
  Bytes(total)

/** Recursively deletes a tree, deleting symlinks themselves rather than their targets. */
def deleteTree(dir: Path): Unit =
  val _ = Files.walkFileTree(
    dir,
    new SimpleFileVisitor[Path]:
      override def visitFile(f: Path, attrs: BasicFileAttributes) =
        Files.delete(f)
        FileVisitResult.CONTINUE
      override def postVisitDirectory(d: Path, e: java.io.IOException) =
        if e != null then throw e
        Files.delete(d)
        FileVisitResult.CONTINUE
  )

/** Runs a command and returns its trimmed output, or None if it is missing, fails or takes too long. */
def commandOutput(command: String*): Option[String] =
  Try {
    val process = ProcessBuilder(command*).redirectErrorStream(true).start()
    val output = String(process.getInputStream.readAllBytes, StandardCharsets.UTF_8)
    if process.waitFor(3, TimeUnit.SECONDS) && process.exitValue == 0 then Some(output.trim) else None
  }.toOption.flatten
