package cz.cvut.fit.prl.scalaimplicit.core.runners

import java.nio.file.{Files, Path}

import org.langmeta.internal.semanticdb.{schema => s}
import org.langmeta.{semanticdb => d}

object DBOps {
  // Transform the database from the internal format to the scala.meta format
  private def toMetaDB(from: s.Database): d.Database = {
    // Discard documents that have only warning messages
    def discardMessageDocs(docs: Seq[s.Document]): Seq[s.Document] =
      docs.filterNot(doc => doc.messages.nonEmpty && doc.contents.isEmpty)

    from.copy(documents = discardMessageDocs(from.documents)).toDb(None)
  }

  def loadDB(filePath: Path): d.Database = {
    toMetaDB(s.Database.parseFrom(Files.readAllBytes(filePath)))
  }
}
