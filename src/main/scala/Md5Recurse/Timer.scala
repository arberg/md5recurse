package Md5Recurse

/**
  * Created by Alex on 21-04-2017.
  */
object Timer {
    def apply(name: String, enable: Boolean)(task: () => Unit) {
        val timer = new Timer();
        task.apply()
        val elapsedMilli = timer.elapsedMilli()
        if (enable) {
            println("%s took %.3fs".format(name, elapsedMilli / 1000.0f))
        }
    }

    def withResult[T](name: String, enable: Boolean)(task: () => T) = {
        val timer = new Timer();
        val t = task.apply()
        val elapsedMilli = timer.elapsedMilli()
        if (enable) {
            println("%s took %.3fs".format(name, elapsedMilli / 1000.0f))
        }
        t
    }
}

class Timer {
    val startTime = System.nanoTime

    def elapsedSec() = {
        elapsedNano() / 1000000000
    }

    def elapsedMilli() = {
        elapsedNano() / 1000000
    }

    def elapsedMicro() = {
        elapsedNano() / 1000
    }

    def elapsedNano() = {
        System.nanoTime - startTime
    }
}
