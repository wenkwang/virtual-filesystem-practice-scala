package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.files.DirEntry
import com.rtjvm.scala.oop.filesystem.State
import com.rtjvm.scala.oop.files.File


class Touch(name: String) extends CreateEntry(name) {

  override def createSpecificEntry(state: State): DirEntry = {
    File.empty(state.wd.path, name)
  }

}
