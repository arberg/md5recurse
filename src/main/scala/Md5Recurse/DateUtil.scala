package Md5Recurse

import java.text.SimpleDateFormat
import java.util.Locale

object DateUtil {
    val formatter: SimpleDateFormat = new SimpleDateFormat("YYYY-MM-dd HH:mm:ss", Locale.ENGLISH)

    def timeToStr(timeInMillis: Long) =
        formatter.format(new java.util.Date(timeInMillis))
}
