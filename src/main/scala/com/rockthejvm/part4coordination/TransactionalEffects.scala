package com.rockthejvm.part4coordination

import zio.*
import zio.stm.*
import com.rockthejvm.utils.*

object TransactionalEffects extends ZIOAppDefault:

  // STM = description of "atomic effects", once starts - can't be interrupted, should be completed
  val anStm        = STM.succeed(42L)
  val aFailedStm   = STM.fail("bad")
  val anAttemptStm = STM.attempt(42L / 0)

  // most often to use STM and USTM
  val alwaysSuccessfulStm: USTM[Long] = STM.succeed(42L)
  val failableStm: STM[String, Long]  = STM.succeed(42L)

  // ZIO vs STM
  // compose STMs to obtain other STMs
  // evaluation is FULLY ATOMIC
  // executed fully or not at all
  // blocks all shared resources
  val anAtomicEffect = anAttemptStm.commit // convert into ZIO

  def transferMoney(sender: Ref[BigDecimal], receiver: Ref[BigDecimal], amount: BigDecimal) =
    for
      senderBalance <- sender.get
      _             <- ZIO.fail("insufficient funds").when(senderBalance < amount)
      _             <- sender.update(_ - amount)
      _             <- receiver.update(_ + amount)
      newBalance    <- sender.get
    yield newBalance

  def exploitBuggy =
    for
      sender   <- Ref.make[BigDecimal](1_000.00)
      receiver <- Ref.make[BigDecimal](0.00)
      fib1     <- transferMoney(sender, receiver, 1_000.00).fork
      fib2     <- transferMoney(sender, receiver, 1_000.00).fork
      _        <- (fib1 zip fib2).join
      _        <- receiver.get.debugThread
    yield ()

  def loop(effect: ZIO[Any, Nothing, Unit], n: Int): ZIO[Any, String, Unit] =
    if n > 10_000 then ZIO.unit
    else effect.ignore *> loop(effect, n + 1)

  def transferMoneyStm(sender: TRef[BigDecimal], receiver: TRef[BigDecimal], amount: BigDecimal) =
    for
      senderBalance <- sender.get
      _             <- STM.fail("insufficient funds").when(senderBalance < amount)
      _             <- sender.update(_ - amount)
      _             <- receiver.update(_ + amount)
      newBalance    <- sender.get
    yield newBalance

  def cannotExploit: ZIO[Any, String, Unit] =
    for
      sender   <- TRef.make[BigDecimal](1_000.00).commit
      receiver <- TRef.make[BigDecimal](0.00).commit
      fib1     <- transferMoneyStm(sender, receiver, 1_000.00).commit.fork
      fib2     <- transferMoneyStm(sender, receiver, 1_000.00).commit.fork
      _        <- (fib1 zip fib2).join
      _        <- receiver.get.commit.debugThread
    yield ()

  /** STM data structure
    */
  // atomic variable: TRef
  // same API: get, update, modify, set
  val aVariable = TRef.make(42L)

  // transactional collection - TArray
  // all operations are atomic
  val specifiedValuesTArray = TArray.make(1, 2, 3)
  val iterableArray         = TArray.fromIterable(List(1, 2, 3, 4, 5))

  // get/apply
  val tArrayGetElement =
    for
      tArray <- iterableArray
      elem   <- tArray(2)
    yield elem
  // update
  val tArrayUpdateElem =
    for
      tArray <- iterableArray
      _      <- tArray.update(1, _ + 10)
    yield tArray
  // transform
  val transformTArray  =
    for
      tArray <- iterableArray
      _      <- tArray.transform(_ * 10)
    yield tArray

  // TSet
  val mySetStm     = TSet.make(1, 2, 3, 4, 5, 1, 2, 3)
  val containsElem =
    for
      tSet     <- mySetStm
      contains <- tSet.contains(3)
    yield contains

  // add in place
  val addElem =
    for
      tSet <- mySetStm
      _    <- tSet.put(5)
    yield tSet

  // delete in place
  val deleteElem =
    for
      tSet <- mySetStm
      _    <- tSet.delete(1)
    yield tSet

  // TMap
  val tMapStm = TMap.make("Alice" -> 1, "Bob" -> 2, "Charlie" -> 3)
  // put
  val putElem =
    for
      map <- tMapStm
      _   <- map.put("David", 4)
    yield map
  // get
  val getElem =
    for
      map  <- tMapStm
      elem <- map.get("Bob")
    yield elem

  // TQueue
  val tQueueStm = TQueue.bounded[Int](3)
  // offer
  val offer     =
    for
      queue <- tQueueStm
      _     <- queue.offerAll(List(1, 2, 3, 4, 5))
    yield queue

  val takeAllChunks =
    (for
      queue <- TQueue.bounded[Int](3)
      _     <- queue.offerAll(List(1, 2, 3)) // if capacity exceeds then hangs
      chunk <- queue.takeAll
    yield chunk.toList).commit

  val tQueueTake: UIO[Int] = (for
    tQueue <- TQueue.bounded[Int](3)
    _      <- tQueue.offerAll(List(1, 2))
    res    <- tQueue.take
  yield res).commit

  // TPriorityQueue
  val maxQueue = TPriorityQueue.make(3, 4, 1, 2, 5)

  /*
    Concurrent coordination
   */
  val tPromiseEff   = TPromise.make[String, Long]
  // await
  val tPromiseAwait =
    for
      p   <- tPromiseEff
      res <- p.await
    yield res

  // succeed, fail, complete
  val demoSucceed =
    for
      p <- tPromiseEff
      _ <- p.succeed(42L)
    // _ <- p.fail("bad")
    yield ()

  // TReentrantLock - can aquire the same lock multiple times without deadlock
  // readers/writers problem
  // has two locks: read lock and write lock
  val reentrantLockEffect = TReentrantLock.make
  val demoReentrantLock   =
    for
      lock       <- reentrantLockEffect
      _          <- lock.acquireRead // acquires read lock (writers can't write)
      _          <- STM.succeed(42L) // critical section
      readLocked <- lock.readLocked  // status of the lock if it's blocked for reading
    yield ()

  def demoReadersWriters =
    def read(i: Int, lock: TReentrantLock): UIO[Unit] =
      for
        _    <- lock.acquireRead.commit
        // critical region
        _    <- ZIO.succeed(s"[task $i] taken the read lock, reading....").debugThread
        time <- Random.nextIntBounded(1000)
        _    <- ZIO.sleep(time.millis)
        res  <- Random.nextIntBounded(100) // actual computation
        _    <- ZIO.succeed(s"[task $i] read value: $res").debugThread
        // critical region end
        _    <- lock.releaseRead.commit
      yield ()

    def write(lock: TReentrantLock): UIO[Unit] =
      for

        _ <- ZIO.succeed("[writer] trying to write...").debugThread
        _ <- lock.acquireWrite.commit
        // critical section
        _ <- ZIO.succeed("[writer] i'm able to write").debugThread
        // critical section end
        _ <- lock.releaseWrite.commit
      yield ()

    for
      lock       <- TReentrantLock.make.commit
      fibWriter1 <- write(lock).fork
      fibReader  <- ZIO.collectAllParDiscard((1 to 10).map(read(_, lock))).fork
      _          <- ZIO.sleep(200.millis)
      fibWriter2 <- write(lock).fork
      _          <- fibReader.join
      _          <- fibWriter1.join
      _          <- fibWriter2.join
    yield ()

  def run = demoReadersWriters
