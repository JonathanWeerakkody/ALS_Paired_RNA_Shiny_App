#!/usr/bin/env Rscript
# Copy the curated panel images and their source tables into the app.
#
# Only the panels listed in config/app_panels.csv are shipped. Everything else
# is available as Source Data with the paper, so the app stays a reader's tour
# rather than a second copy of the figures.
#
# Sizing: constrain by WIDTH, because browser legibility depends on horizontal
# pixels. Constraining the longest side squeezes tall panels until labels collide.

here <- normalizePath("."); root <- normalizePath(file.path(here,"..",".."))
figs <- file.path(root,"figures")
sel  <- read.csv(file.path(here,"config/app_panels.csv"), stringsAsFactors=FALSE)
outi <- file.path(here,"www/panels"); outd <- file.path(here,"data-processed/panels")
unlink(outi, recursive=TRUE); unlink(outd, recursive=TRUE)
dir.create(outi, recursive=TRUE); dir.create(outd, recursive=TRUE)

dims <- function(f) {
  o <- system2("sips", c("-g","pixelWidth","-g","pixelHeight", shQuote(f)), stdout=TRUE)
  c(as.integer(sub(".*: ","",grep("pixelWidth",o,value=TRUE))),
    as.integer(sub(".*: ","",grep("pixelHeight",o,value=TRUE))))
}
# published Extended number -> on-disk folder
ED <- c("1"="1","2"="3","3"="2","4"="8","5"="5","6"="4","7"="6","8"="7","9"="9","10"="10")
srcdir <- function(fig, panel) {
  if (grepl("^Figure", fig))
    file.path(figs, sub("Figure ","F",fig), "Main", paste0("Part ", panel))
  else file.path(figs, paste0("Extended", ED[[sub("Extended Data ","",fig)]]), paste0("Part_", panel))
}
MAXW <- 2400
rows <- list(); missing <- character()
for (i in seq_len(nrow(sel))) {
  fig <- sel$figure[i]; pan <- sel$panel[i]
  d <- srcdir(fig, pan); png <- file.path(d,"plot.png")
  if (!file.exists(png)) { missing <- c(missing, paste(fig,pan)); next }
  tag <- sprintf("%s_%s", fig, pan); dest <- file.path(outi, paste0(tag,".png"))
  w <- dims(png)[1]
  if (w > MAXW) system2("sips", c("--resampleWidth", MAXW, shQuote(png), "--out", shQuote(dest)),
                        stdout=NULL, stderr=NULL) else file.copy(png, dest, overwrite=TRUE)
  nd <- dims(dest); got <- ""
  for (c0 in c("plot_data.csv","source_data.csv")) {
    f <- file.path(d,c0)
    if (file.exists(f) && file.info(f)$size < 3e6) {
      got <- paste0(tag,".csv"); file.copy(f, file.path(outd,got), overwrite=TRUE); break }
  }
  rows[[length(rows)+1]] <- data.frame(figure=fig, panel=pan, section=sel$section[i],
    why=sel$why[i], image=paste0(tag,".png"), data=got,
    width=nd[1], height=nd[2], kb=round(file.info(dest)$size/1024), stringsAsFactors=FALSE)
}
ix <- do.call(rbind, rows)
write.csv(ix, file.path(here,"data-processed/panel_index.csv"), row.names=FALSE)
message(sprintf("%d panels · %.0f MB · %d with source data",
                nrow(ix), sum(ix$kb)/1024, sum(nzchar(ix$data))))
if (length(missing)) message("NOT FOUND: ", paste(missing, collapse=", "))
narrow <- ix[ix$width < 1100, ]
if (nrow(narrow)) { message("narrow (<1100 px), full-size link shown in app:")
  for (i in seq_len(nrow(narrow))) message(sprintf("   %-26s %d x %d",
    narrow$image[i], narrow$width[i], narrow$height[i])) }
