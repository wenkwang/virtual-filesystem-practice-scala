package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

class Mkdir(name: String) extends Command {

  override def apply(state: State): State = {

    val wd = state.wd

    if (wd.hasEntry(name)) {
      state.setMessage("Entry " + name + " already exists.")
    } else if (!isValidEntry(name)) {
      state.setMessage("Entry name invalid.")
    } else {
      doMkdir(state, name)
    }
  }

  def doMkdir(state: State, name: String): State = {

    def updateStructure(currentDir: Directory, path: List[String], newEntry: DirEntry): Directory = {
      if (path.isEmpty) currentDir.addEntry(newEntry)
      else {
        val oldEntry = currentDir.findEntry(path.head).asDirectory
        currentDir.replaceEntry(oldEntry.name, updateStructure(oldEntry, path.tail, newEntry))
      }
    }

    val wd = state.wd
    val allDirsInPath = wd.getAllDirsInPath
    val newDir = Directory.empty(wd.path, name)
    val newRoot = updateStructure(state.root, allDirsInPath, newDir)
    val newWd = newRoot.findDescendant(allDirsInPath)

    State(newRoot, newWd)
  }

  def isValidEntry(name: String): Boolean = {
    if (name.contains(Directory.SEPARATOR)) false
    else if (name.contains(".")) false
    else true
  }

}
