package speak

trait Ctx {
  def setExecutor(n: Int)

  def setExecutorCore(n: Int)

  def start()
}

trait RDD[T] {
  def compute(split: Int): Iterable[T]

  def partition: Int

  def foreach(fn: T => Unit)
}

trait Executor {
  def runTask(task: Task)
}

trait Task {
  def runSplit[T](rdd: RDD[T], split: Int)
}

trait Rpc {
  def open(port: Int)

  def send
}
