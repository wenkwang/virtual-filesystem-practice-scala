package com.rtjvm.scala.oop.commands
import com.rtjvm.scala.oop.files.{Directory, File}
import com.rtjvm.scala.oop.filesystem.State

import scala.annotation.tailrec

class Echo(args: Array[String]) extends Command {

  override def apply(state: State): State = {
    if (args.length == 0) state
    else if (args.length == 1) state.setMessage(args(0))
    else {
      val operator = args(args.length - 2)
      val filename = args(args.length - 1)
      val contents = createContents(args, args.length - 2)

      if (operator.equals(">"))
        doEcho(state, filename, contents, false)
      else if (operator.equals((">>")))
        doEcho(state, filename, contents, true)
      else
        state.setMessage(createContents(args, args.length))
    }
  }

  def getNewRootAfterEcho(currentDirectory: Directory,
                          path: List[String],
                          contents: String,
                          append: Boolean): Directory = {
    if (path.isEmpty) currentDirectory
    else if (path.tail.isEmpty) {
      val dirEntry = currentDirectory.findEntry(path.head)
      if (dirEntry == null) currentDirectory.replaceEntry(path.head, new File(currentDirectory.path, path.head, contents))
      else if (dirEntry.isDirectory) currentDirectory
      else {
        if (append) currentDirectory.replaceEntry(path.head, dirEntry.asFile.appendContents(contents))
        else currentDirectory.replaceEntry(path.head, dirEntry.asFile.setContents(contents))
      }
    } else {
      val nextDirectory = currentDirectory.findEntry(path.head)
      val nextNewDirectory = getNewRootAfterEcho(nextDirectory.asDirectory, path.tail, contents, append)
      if (nextNewDirectory == nextDirectory) currentDirectory
      else {
        currentDirectory.replaceEntry(path.head, nextNewDirectory)
      }

    }

  }

  def doEcho(state: State, filename: String, contents: String, append: Boolean): State = {
    if (filename.contains(Directory.SEPARATOR)) state
    else {
      val newRoot = getNewRootAfterEcho(state.root, state.wd.getAllDirsInPath :+ filename, contents, append)
      if (newRoot == state.root) state.setMessage("Echo fail.")
      else {
        val wd = newRoot.findDescendant(state.wd.getAllDirsInPath)
        State(newRoot, wd)
      }
    }
  }

  def createContents(contents: Array[String], topIndex: Int): String = {

    @tailrec
    def createContentsHelper(currentIndex: Int, accumlator: String): String = {
      if (currentIndex >= topIndex) accumlator
      else createContentsHelper(currentIndex + 1, accumlator + " " + contents(currentIndex))
    }

    createContentsHelper(0, "")
  }
}
