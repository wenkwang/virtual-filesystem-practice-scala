package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.{DirEntry, Directory}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Cd(dir: String) extends Command {

  override def apply(state: State): State = {
    val root = state.root
    val wd = state.wd
    val absolutePath = {
      if (dir.startsWith(Directory.SEPARATOR)) dir
      else if (wd.isRoot) wd.path + dir
      else wd.path + Directory.SEPARATOR + dir
    }

    val destinationDirectory  = doFindEntry(root, absolutePath)
    if (destinationDirectory == null || !destinationDirectory.isDirectory)
      state.setMessage(dir + ": no such directory.")
    else State(root, destinationDirectory.asDirectory)
  }

  def doFindEntry(root: Directory, path: String): DirEntry = {
    @tailrec
    def findEntryHelper(currentDirectory: Directory, tokens: List[String]): DirEntry = {

      if (tokens.isEmpty || tokens.head.isEmpty) currentDirectory
      else if (tokens.tail.isEmpty) currentDirectory.findEntry(tokens.head)
      else {
        val nextDir = currentDirectory.findEntry(tokens.head)
        if (nextDir == null || !nextDir.isDirectory) null
        else findEntryHelper(nextDir.asDirectory, tokens.tail)
      }
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    findEntryHelper(root, tokens)
  }
}
