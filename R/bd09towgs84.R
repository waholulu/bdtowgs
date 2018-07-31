x_pi = 3.1415926535897932384626 * 3000.0 / 180.0
pi = 3.1415926535897932384626  
a = 6378245.0  
ee = 0.00669342162296594323  

#' From bd09 to gcj02.
#'
#' @param bd_lon A number.
#' @param bd_lat A number.
#' @return result.
#' @export
bd09togcj02 <- function (bd_lon, bd_lat) {
  x = bd_lon - 0.0065
  y = bd_lat - 0.006
  z = sqrt(x * x + y * y) - 0.00002 * sin(y * x_pi)
  theta = atan2(y, x) - 0.000003 * cos(x * x_pi)
  gg_lng = z * cos(theta)
  gg_lat = z * sin(theta)
  return (c(gg_lng, gg_lat))
  
}

#' From gcj02 to wgs84
#'
#' @param lng A number.
#' @param lat A number.
#' @return result.
#' @export
gcj02towgs84 <- function (lng, lat) {
  dlat = transformlat(lng - 105.0, lat - 35.0)
  dlng = transformlng(lng - 105.0, lat - 35.0)
  radlat = lat / 180.0 * pi
  magic = sin(radlat)
  magic = 1 - ee * magic * magic
  sqrtmagic = sqrt(magic)
  dlat = (dlat * 180.0) / ((a * (1 - ee)) / (magic * sqrtmagic) * pi)
  dlng = (dlng * 180.0) / (a / sqrtmagic * cos(radlat) * pi)
  mglat = lat + dlat
  mglng = lng + dlng
  return (c(lng * 2 - mglng, lat * 2 - mglat))
  
}

#' transformlat
#'
#' @param lng A number.
#' @param lat A number.
#' @return result.
#' @export
transformlat <- function (lng, lat) {
  ret = -100.0 + 2.0 * lng + 3.0 * lat + 0.2 * lat * lat + 0.1 * lng * lat + 0.2 * sqrt(abs(lng))
  ret = ret + (20.0 * sin(6.0 * lng * pi) + 20.0 * sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret = ret + (20.0 * sin(lat * pi) + 40.0 * sin(lat / 3.0 * pi)) * 2.0 / 3.0
  ret = ret + (160.0 * sin(lat / 12.0 * pi) + 320 * sin(lat * pi / 30.0)) * 2.0 / 3.0
  return(ret)
}

#' transformlng
#'
#' @param lng A number.
#' @param lat A number.
#' @return result.
#' @export
transformlng <- function (lng, lat) {
  ret = 300.0 + lng + 2.0 * lat + 0.1 * lng * lng + 0.1 * lng * lat + 0.1 * sqrt(abs(lng))
  ret = ret + (20.0 * sin(6.0 * lng * pi) + 20.0 * sin(2.0 * lng * pi)) * 2.0 / 3.0
  ret = ret + (20.0 * sin(lng * pi) + 40.0 * sin(lng / 3.0 * pi)) * 2.0 / 3.0
  ret = ret + (150.0 * sin(lng / 12.0 * pi) + 300.0 * sin(lng / 30.0 * pi)) * 2.0 / 3.0
  return (ret)
  
}

#' transformlng
#'
#' @param df A two value series
#' @return result.
#' @export
bd09towgs84 <- function(df) {
  df <- as.numeric(df)
  lng <- df[1]
  lat <- df[2]
  tt <- bd09togcj02(lng, lat)
  return(gcj02towgs84(tt[1], tt[2]))
  
}
