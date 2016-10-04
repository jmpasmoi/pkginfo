#' Tools to get interactively information of R packages
#'
#' @export
#'
pkgs <- function(){

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

  setwd(dir=path)
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

#' Tools to get interactively information of R packages
#'
#' @export
#'
pkginfo <- function(pkgname){

  pkg_path <- .libPaths()
  allpaths <- pkg_stat(pkg_path)$rlib
  np <- data.frame()
  cmp <- FALSE

  for(i in 1:length(allpaths)){

    m_path <- allpaths[i]
    m_dir <- dir(m_path)

    for(j in 1:length(m_dir)){

      cur_dir <- m_dir[j]
      each_dir <- trimws(paste0(paste0(m_path,"/"), file.path(cur_dir)))

      if(dir.exists(each_dir) && identical(cur_dir,pkgname)){

        cmp <- TRUE
        break;

      }else{ next}

    }
  }

  if(cmp){

     size_mb <- pkg_size(each_dir)
     dps <- desc::description$new(each_dir)
     cit <-  citation(package=pkgname)[[1]]$title
     namesp <- readLines(paste0(each_dir,"/","NAMESPACE"), warn = FALSE, skipNul = FALSE)

     grp <- grep("export", namesp)
     for(i in 1 : length(grp)){

        n <- namesp[grp[i]]
        npp <- data.frame(pkgfunctions = substr(n,8,nchar(n)-1))
        np <- rbind(np,npp)
     }

  }else{
    stop(paste0("There is no package called: ",pkgname), call.=FALSE)
  }

  pkg <- list(
                pkgname=pkgname,
                version=packageVersion(pkgname),
                pkgsize=paste0(size_mb,"Mb"),
                title=cit,
                maintainer=dps$get_maintainer(),
                depends=dps$get_deps(),
                nb_func=nrow(np),
                lst_func=np
              )

  invisible(pkg)
}
