package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

trait Command {

  def apply(state: State): State

}

object Command {

  val COMMAND_MKDIR = "mkdir"
  val COMMAND_LS = "ls"
  val COMMAND_PWD = "pwd"
  val COMMAND_TOUCH = "touch"
  val COMMAND_CD = "cd"

  def emptyCommand: Command = new Command {
    override def apply(state: State): State = state.setMessage("")
  }

  def incompleteCommand(name: String): Command = new Command {
    override def apply(state: State): State = state.setMessage(name + ": incomplete command.")
  }

  def from(message: String): Command = {
    val msgArray: Array[String] = message.split(" ")
    if (message.isEmpty || msgArray.isEmpty) emptyCommand
    else if (msgArray(0).equals(COMMAND_MKDIR)) {
      if (msgArray.length < 2) incompleteCommand(COMMAND_MKDIR)
      else new Mkdir(msgArray(1))
    } else if (msgArray(0).equals(COMMAND_LS)) {
      new Ls
    } else if (msgArray(0).equals(COMMAND_PWD)) {
      new Pwd
    } else if (msgArray(0).equals(COMMAND_TOUCH)) {
      if (msgArray.length < 2) incompleteCommand(COMMAND_TOUCH)
      else new Touch(msgArray(1))
    } else if (msgArray(0).equals(COMMAND_CD)) {
      if (msgArray.length < 2) incompleteCommand(COMMAND_CD)
      else new Cd(msgArray(1))
    }
    else new UnknownCommand
  }
}
