package com.rtjvm.scala.oop.files

import com.rtjvm.scala.oop.filesystem.FileSystemException

import scala.annotation.tailrec

class Directory(override val parentPath: String, override val name: String, val contents: List[DirEntry])
  extends DirEntry(parentPath, name) {

  def hasEntry(name: String): Boolean = {
    findEntry(name) != null
  }

  def getAllDirsInPath: List[String] = {
    path.substring(1).split(Directory.SEPARATOR).toList.filter(x => !x.isEmpty)
  }

  def findDescendant(dirsInPath: List[String]): Directory = {
    if (dirsInPath.isEmpty) this
    else findEntry(dirsInPath.head).asDirectory.findDescendant(dirsInPath.tail)
  }

  def addEntry(newEntry: DirEntry): Directory = {
    new Directory(parentPath, name, contents :+ newEntry)
  }

  def findEntry(entryName: String): DirEntry = {

    @tailrec
    def findEntryHelper(name: String, contentList: List[DirEntry]): DirEntry = {
      if (contentList.isEmpty) null
      else if (contentList.head.name.equals(name)) contentList.head
      else findEntryHelper(name, contentList.tail)
    }

    findEntryHelper(entryName, contents)
  }

  def replaceEntry(entryName: String, newEntry: DirEntry): Directory = {
    new Directory(parentPath, name, contents.filter(e => !e.name.equals(entryName)) :+ newEntry)
  }

  def asDirectory: Directory = this

  def asFile: File = {
    throw new FileSystemException("Directory cannot be converted to a file.")
  }

  def isRoot: Boolean = parentPath.isEmpty

  def getType: String = "Directory"

  def isDirectory: Boolean = true

  def isFile: Boolean = false

}

object Directory {
  val SEPARATOR = "/"
  val ROOT_PATH = "/"

  def ROOT: Directory = empty("", "")

  def empty(parentPath: String, name: String): Directory =
    new Directory(parentPath, name, List())
}