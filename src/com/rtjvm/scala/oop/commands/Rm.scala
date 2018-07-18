package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.Directory
import com.rtjvm.scala.oop.filesystem.State

class Rm(name: String) extends Command {

  override def apply(state: State): State = {
    val wd = state.wd
    val currentPath = wd.path
    val absolutePath = {
      if (name.startsWith(Directory.SEPARATOR)) name
      else if (wd.isRoot) currentPath + name
      else currentPath + Directory.SEPARATOR + name
    }
    if (absolutePath.equals(Directory.ROOT_PATH))
      state.setMessage("Fail to remove.")
    else
      doRm(state, absolutePath)
  }

  def doRm(state: State, path: String): State = {

    def doRmHelper(currentDirectory: Directory, path: List[String]): Directory = {
      if (path.isEmpty) currentDirectory
      else if (path.tail.isEmpty) currentDirectory.removeEntry(path.head)
      else {
        val nextDirectory = currentDirectory.findEntry(path.head)
        if (!nextDirectory.isDirectory) currentDirectory
        else {
          val newNextDir = doRmHelper(nextDirectory.asDirectory, path.tail)
          if (newNextDir == nextDirectory) currentDirectory
          else currentDirectory.replaceEntry(nextDirectory.name, newNextDir)
        }
      }
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newRoot = doRmHelper(state.root, tokens)
    if (newRoot == state.root)
      state.setMessage("Fail to remove.")
    else {
      val newWd = newRoot.findDescendant(state.wd.path.substring(1))
      State(newRoot, newWd)
    }
  }
}
