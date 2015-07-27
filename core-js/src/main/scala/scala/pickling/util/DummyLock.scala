package scala.pickling.util


private[pickling] class DummyLock {
  def lock(): Unit = {}
  def lockInterruptibly(): Unit = {}
  def tryLock(): Boolean = true
  def unlock(): Unit = {}
}
