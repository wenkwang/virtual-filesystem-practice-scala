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

    @tailrec
    def collapseRelativeTokens(path: List[String], result: List[String]): List[String] = {
      if (path.isEmpty) result
      else if (path.head.equals(".")) collapseRelativeTokens(path.tail, result)
      else if (path.head.equals("..")) {
        if (result.isEmpty) null
        else collapseRelativeTokens(path.tail, result.init)
      } else collapseRelativeTokens(path.tail, result :+ path.head)
    }

    val tokens = path.substring(1).split(Directory.SEPARATOR).toList
    val newTokens = collapseRelativeTokens(tokens, List())
    if (newTokens == null) null
    findEntryHelper(root, newTokens)
  }
}
