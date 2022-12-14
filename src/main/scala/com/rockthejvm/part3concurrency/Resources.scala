package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.debugThread

import java.io.File
import java.util.Scanner
import javax.swing.Spring

object Resources extends ZIOAppDefault:

  def unsafeMethod(): Int = throw new RuntimeException("Not an int here for you")
  val anAttempt           = ZIO.attempt(unsafeMethod())

  // finalizer
  val attemptWithFinalizer = anAttempt.ensuring(ZIO.succeed("finalizer!").debugThread)
  // multiple finalizers (in order)
  val extraFinalizer       = attemptWithFinalizer.ensuring(ZIO.succeed("another finalizer").debugThread)

  // .onInterrupt, .onError, .onDone, .onExit - to manage lifecycle of resources

  class Connection(url: String):
    def open  = ZIO.succeed(s"opening connection to $url").debugThread
    def close = ZIO.succeed(s"closing connection to $url").debugThread

  object Connection:
    def create(url: String) = ZIO.succeed(new Connection(url))

  val leakyFetchUrl =
    for
      conn <- Connection.create("rockthejvm.com")
      fib  <- (conn.open *> ZIO.sleep(300.seconds)).fork
      _    <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
      _    <- fib.join
    yield () // resource leak

  val fetchUrl =
    for
      conn <- Connection.create("rockthejvm.com")
      fib  <- (conn.open *> ZIO.sleep(300.seconds)).ensuring(conn.close).fork
      _    <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
      _    <- fib.join
    yield ()

  // tedious to use ensuring

  /*
    acquireRelease:
      - acquiring cannot be interrupted
      - all finalizers are guaranteed to run
   */
  val cleanConnection = ZIO.acquireRelease(Connection.create("rockthejvm.com"))(_.close)

  val fetchUrlWithResource =
    for
      conn <- cleanConnection
      fib  <- (conn.open *> ZIO.sleep(300.seconds)).fork
      _    <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
      _    <- fib.join
    yield ()

  val fetchWithScopedResource = ZIO.scoped(fetchUrlWithResource) // to eliminate Scope dependency

  // acquireReleaseWith
  val cleanConnection_v2 = ZIO.acquireReleaseWith(
    Connection.create("rockthejvm.com")
  )(
    _.close // release
  )(conn => conn.open *> ZIO.sleep(300.seconds))

  val fetchUrlWithResource_v2 =
    for
      fib <- cleanConnection_v2.fork
      _   <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
      _   <- fib.join
    yield ()

  /** Exercises
    *   1. Use the acquireRelease to open a file, print all lines, (one every 100 millis), then close the file
    */

  def openFileScanner(path: String): UIO[Scanner] =
    ZIO.succeed(s"opening file at $path").debugThread *> ZIO.succeed(new Scanner(new File(path)))

  def closeFileScanner(scanner: Scanner) =
    ZIO.succeed(s"closing file..").debugThread *> ZIO.attempt(scanner.close()).orDie

  def readLineByLineDelayed(scanner: Scanner) =
    ZIO.whileLoop(scanner.hasNext)(
      ZIO.attempt(scanner.nextLine).orDie.flatMap(line => ZIO.debug(line).delay(100.millis).unit)
    )(_ => ())

  def acquireOpenFile(path: String): UIO[Unit] =
    ZIO.acquireReleaseWith(openFileScanner(path))(closeFileScanner)(readLineByLineDelayed)

  val testInterruptFileDisplay =
    for
      fib <- acquireOpenFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala").fork
      _   <- fib.interrupt.delay(2.seconds)
    yield ()

  def connFromConfig(path: String) =
    ZIO.acquireReleaseWith(openFileScanner(path))(closeFileScanner) { scanner =>
      ZIO.acquireReleaseWith(Connection.create(scanner.nextLine()))(_.close) { conn =>
        conn.open *> ZIO.never // do whatever you want with collection... however this nesting sucks
      }
    }

  def connFromConfig_v2(path: String) =
    for
      scanner <- ZIO.acquireRelease(openFileScanner(path))(closeFileScanner)
      conn    <- ZIO.acquireRelease(Connection.create(scanner.nextLine()))(_.close)
      _       <- conn.open *> ZIO.never
    yield ()

  def run = connFromConfig_v2("src/main/resources/conn.conf")
