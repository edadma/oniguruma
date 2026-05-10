package io.github.edadma.oniguruma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** The VM polls `Thread.interrupted()` every 65k bytecode steps so a
  * caller running the engine on a worker thread can kill a runaway
  * match by interrupting that thread. Without the poll the only out
  * is the 10M-step `StepLimit` ceiling, which on pathological patterns
  * can pin a CPU for many seconds — and the daemon-thread leak that
  * comes from a juicer-style 5s deadline + worker.interrupt() pattern
  * starves the rest of the JVM.
  *
  * This spec verifies the poll actually fires: a regex with a known
  * blow-up shape (the exponential `(a+)+b` against an all-`a` input)
  * should die within milliseconds of the interrupt instead of running
  * to StepLimit. */
class InterruptSpec extends AnyFreeSpec with Matchers:

  "VM cooperative cancellation" - {

    "InterruptedException propagates within ~100ms of Thread.interrupt() during one long match" in {
      // Classic exponential blowup: (a+)+b against many `a`s + no `b`.
      // The match has to try every partition of the as before failing.
      // 30+ a's is enough to keep the VM busy for seconds at StepLimit.
      val re    = Regex("(a+)+b")
      val input = "a" * 40

      val ref     = new java.util.concurrent.atomic.AtomicReference[Either[Throwable, Option[Match]]](null)
      val started = new java.util.concurrent.CountDownLatch(1)
      val worker  = new Thread({ () =>
        started.countDown()
        try ref.set(Right(re.findFirstMatchIn(input)))
        catch case t: Throwable => ref.set(Left(t))
      })
      worker.setDaemon(true)
      worker.start()
      started.await()
      Thread.sleep(50)
      worker.interrupt()
      worker.join(2000)

      worker.isAlive shouldBe false
      ref.get match
        case Left(_: InterruptedException) => succeed
        case Left(other)                   => fail(s"unexpected throwable: $other")
        case Right(maybe) =>
          fail(s"VM ran to completion (returned $maybe) instead of being interrupted — interrupt poll didn't fire")
    }

    "InterruptedException fires across many short matchAt calls (the tokenizer pattern)" in {
      // Highlighter-style call pattern: many small matchAt invocations
      // that each finish well under the in-loop poll's 65k-step
      // window. Without an entry-point check the interrupt flag would
      // stay set across calls forever and the worker never bails.
      // After the entry-point poll lands, even a sub-microsecond
      // matchAt aborts the moment the flag is set.
      val re      = Regex("\\d+")           // tiny pattern, fast match
      val input   = "abc"                   // never matches → many runOne calls
      val ref     = new java.util.concurrent.atomic.AtomicReference[Either[Throwable, Int]](null)
      val started = new java.util.concurrent.CountDownLatch(1)
      val worker  = new Thread({ () =>
        started.countDown()
        var calls = 0
        try
          while true do
            re.findFirstMatchIn(input)   // returns None each time
            calls += 1
        catch case t: Throwable =>
          ref.set(Left(t))
        ref.compareAndSet(null, Right(calls))
        ()
      })
      worker.setDaemon(true)
      worker.start()
      started.await()
      Thread.sleep(20)
      worker.interrupt()
      worker.join(2000)

      worker.isAlive shouldBe false
      ref.get match
        case Left(_: InterruptedException) => succeed
        case Left(other)                   => fail(s"unexpected throwable: $other")
        case Right(_)                      => fail("loop never threw — entry-point interrupt check didn't fire")
    }
  }
