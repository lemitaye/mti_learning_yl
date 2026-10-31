# Mirror generated tables/figures after their local write succeeds.
# Set MTI_OVERLEAF_DIR to override the destination; set it to "" to disable.
# Missing/unwritable Dropbox folders warn without discarding local results.
mirror_output <- function(files, kind = c("tables", "figures")) {
  kind <- match.arg(kind)
  destination <- Sys.getenv("MTI_OVERLEAF_DIR", unset =
    "/Users/user/Library/CloudStorage/Dropbox/Apps/Overleaf/MTI and Employment - Revision (AE)")
  if (!nzchar(destination)) return(invisible(FALSE))
  # The pre-revision project is an archive: never overwrite it, even through
  # a stale environment override or a different spelling of the same path.
  protected <- "/Users/user/Library/CloudStorage/Dropbox/Apps/Overleaf/MTI and Employment Outcomes NFR"
  if (identical(normalizePath(destination, mustWork = FALSE),
                normalizePath(protected, mustWork = FALSE))) {
    warning("Overleaf export skipped: the original pre-revision project is protected.",
            call. = FALSE)
    return(invisible(FALSE))
  }
  if (!dir.exists(destination)) {
    warning("Overleaf export skipped: project folder not found: ", destination,
            call. = FALSE)
    return(invisible(FALSE))
  }
  success <- TRUE
  for (src in files) {
    tryCatch({
      if (!file.exists(src) || file.info(src)$size == 0)
        stop("Missing or empty local output: ", src)
      target_dir <- file.path(destination, kind)
      dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
      target <- file.path(target_dir, basename(src))
      hash <- function(path) unname(tools::md5sum(path))
      if (file.exists(target)) {
        if (identical(hash(src), hash(target))) next
        # Keep each prior destination version once, before overwriting it.
        # Separate projects' version histories while preserving older backups.
        # The normalized project folder name identifies this destination.
        project_key <- gsub("[^A-Za-z0-9._-]", "_", basename(normalizePath(destination)))
        backup_dir <- file.path("output", "overleaf_backups", project_key, kind)
        dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)
        backup <- file.path(backup_dir, paste0(basename(target), ".", hash(target), ".bak"))
        if (!file.exists(backup) && !file.copy(target, backup))
          stop("Could not back up existing Overleaf output: ", target)
      }
      # Write beside the destination, then rename, so Dropbox sees a complete file.
      tmp <- tempfile(pattern = ".mti-export-", tmpdir = target_dir)
      tryCatch({
        if (!file.copy(src, tmp) || !identical(hash(src), hash(tmp)))
          stop("Could not copy/verify output: ", src)
        if (!file.rename(tmp, target)) stop("Could not replace output: ", target)
      }, finally = if (file.exists(tmp)) unlink(tmp))
      message("Exported to Overleaf Dropbox: ", target)
    }, error = function(e) {
      success <<- FALSE
      warning("Local output retained; Overleaf export failed: ",
              conditionMessage(e), call. = FALSE)
    })
  }
  invisible(success)
}
