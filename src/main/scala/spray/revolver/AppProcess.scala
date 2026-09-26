/*
 * Copyright (C) 2009-2012 Johannes Rudolph and Mathias Doenitz
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package spray.revolver

import java.lang.{Runtime => JRuntime}
import java.util.concurrent.TimeUnit
import sbt.{Logger, ProjectRef}

import scala.sys.process.Process

/**
 * A token which we put into the SBT state to hold the Process of an application running in the background.
 */
case class AppProcess(projectRef: ProjectRef, consoleColor: String, log: Logger)(process: Process) {
  val shutdownHook = createShutdownHook("... killing ...")

  def createShutdownHook(msg: => String) =
    new Thread(new Runnable {
      def run() {
        if (isRunning) {
          log.info(msg)
          process.destroy()
        }
      }
    })

  @volatile var finishState: Option[Int] = None

  val watchThread = {
    val thread = new Thread(new Runnable {
      def run() {
        val code = process.exitValue()
        finishState = Some(code)
        log.info("... finished with exit code %d" format code)
        unregisterShutdownHook()
        Actions.unregisterAppProcess(projectRef)
      }
    })
    thread.setDaemon(true)
    thread.start()
    thread
  }
  def projectName: String = projectRef.project

  registerShutdownHook()

  /**
   * Stop the running application. Sends SIGTERM to the child process
   * (which triggers JVM shutdown hooks) and waits up to 2 seconds for
   * graceful exit before forcing a kill. The shutdown hook remains
   * registered for true JVM shutdown.
   */
  def stop() {
    // Send SIGTERM first to allow the child JVM to run its shutdown hooks
    process.destroy()

    // Wait for graceful exit (allows shutdown hooks to run)
    val exited = process.waitFor(2, TimeUnit.SECONDS)
    if (!exited) {
      log.info("[YELLOW]Application did not shut down gracefully, forcing kill ...")
      process.destroyForcibly()
    }

    try {
      finishState = Some(process.exitValue())
    } catch {
      case _: IllegalStateException => // process still running, ignore
    }
  }

  def registerShutdownHook() {
    JRuntime.getRuntime.addShutdownHook(shutdownHook)
  }

  def unregisterShutdownHook() {
    JRuntime.getRuntime.removeShutdownHook(shutdownHook)
  }

  def isRunning: Boolean =
    finishState.isEmpty
}
