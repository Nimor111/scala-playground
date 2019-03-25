package homework1

import org.scalatest.{FlatSpec, Matchers}

class QueueTest extends FlatSpec with Matchers {
  "an empty queue" should "produce a queue with a single element when that element is added to it" in {
    val emptyQueue = Queue.empty
    val singleElementQueue = emptyQueue.push(42)

    singleElementQueue.peek shouldBe 42
    singleElementQueue.size shouldBe 1
  }

  "pop" should "remove an element from the beginning of the queue" in {
    val queue = Queue(List(1, 2, 3))

    val shorterQueue = queue.pop

    shorterQueue.peek shouldBe 2
  }

  "empty" should "create an empty queue" in {
    val queue = Queue.empty

    queue.isEmpty shouldBe true
  }

  "apply method" should "create a new queue from a list" in {
    val queue = Queue(List(1, 2, 3))

    queue.size shouldBe 3
  }
}
