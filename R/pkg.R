#' Tools to get interactively information of R packages
#'
#' @export
#'
pkginfo <- function(){

  pkg_path <- .libPaths()
  allpaths <- pkg_stat(pkg_path)$rlib

  for(i in 1:length(allpaths)){

    m_path <- allpaths[i]
    m_dir <- dir(m_path)

    for(j in 1:length(m_dir)){

      cur_dir <- m_dir[j]
      each_dir <- trimws(paste0(paste0(m_path,"/"), file.path(cur_dir)))
      size_mb <- pkg_size(each_dir)

      print(c(cur_dir, size_mb)) #Too slow

    }
  }
}

pkg_size <- function(path){

  setwd(dir = path)
  total <- round(
             sum(
                  file.info(
                            list.files( path=".", all.files=TRUE,recursive=TRUE)
                           )$size
                ) / 1024 ^ 2,4
           )
  invisible(total)
}

pkg_stat <- function(path){

  rpath <- data.frame()
  for(i in 1:length(path)){

    p <- path[i]
    r <- data.frame(rlib=p, nb_rlib=length(dir(setwd(p))), stringsAsFactors=FALSE)
    rpath <- rbind(rpath, r)

  }

  invisible(rpath)
}

