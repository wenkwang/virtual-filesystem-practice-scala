package com.rtjvm.scala.oop.files

import com.rtjvm.scala.oop.filesystem.FileSystemException

class File(override val parentPath: String, override val name: String, val contents: String)
  extends DirEntry(parentPath, name) {

  def asDirectory: Directory = {
    throw new FileSystemException(("File cannot be converted to a directory."))
  }

  def asFile: File = this

  def getType: String = "File"

  def isDirectory: Boolean = false

  def isFile: Boolean = true

  def appendContents(newContents: String): File = {
    setContents(contents + "\n" + newContents)
  }

  def setContents(newContents: String): File = {
    new File(parentPath, name, newContents)
  }

}

object File {
  def empty(parentPath: String, name: String): File = {
    new File(parentPath, name, "")
  }
}
