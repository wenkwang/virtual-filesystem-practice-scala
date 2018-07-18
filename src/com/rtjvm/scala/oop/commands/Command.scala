package com.rtjvm.scala.oop.commands

import com.rtjvm.scala.oop.filesystem.State

trait Command extends (State => State) {
}

object Command {

  val COMMAND_MKDIR = "mkdir"
  val COMMAND_LS = "ls"
  val COMMAND_PWD = "pwd"
  val COMMAND_TOUCH = "touch"
  val COMMAND_CD = "cd"
  val COMMAND_RM = "rm"
  val COMMAND_ECHO = "echo"
  val COMMAND_CAT = "cat"

  def emptyCommand: Command = new Command {
    override def apply(state: State): State = state.setMessage("")
  }

  def incompleteCommand(name: String): Command = new Command {
    override def apply(state: State): State = state.setMessage(name + ": incomplete command.")
  }

  def from(message: String): Command = {
    val msgArray: Array[String] = message.split(" ")
    if (message.isEmpty || msgArray.isEmpty) emptyCommand
    else msgArray(0) match {
      case COMMAND_MKDIR =>
        if (msgArray.length < 2) incompleteCommand(COMMAND_MKDIR)
        else new Mkdir(msgArray(1))

      case COMMAND_LS =>
        new Ls

      case COMMAND_PWD =>
        new Pwd

      case COMMAND_TOUCH =>
        if (msgArray.length < 2) incompleteCommand(COMMAND_TOUCH)
        else new Touch(msgArray(1))

      case COMMAND_CD =>
        if (msgArray.length < 2) incompleteCommand(COMMAND_CD)
        else new Cd(msgArray(1))

      case COMMAND_RM =>
        if (msgArray.length < 2) incompleteCommand(COMMAND_RM)
        else new Rm(msgArray(1))

      case COMMAND_ECHO =>
        if (msgArray.length < 2) incompleteCommand(COMMAND_ECHO)
        else new Echo(msgArray.tail)

      case COMMAND_CAT =>
        if (msgArray.length < 2) incompleteCommand(COMMAND_CAT)
        else new Cat(msgArray(1))

      case _ =>
        new UnknownCommand
    }
  }
}
