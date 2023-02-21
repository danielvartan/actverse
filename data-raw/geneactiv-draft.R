library(GENEAread)

# ?GENEAread::read.bin

binfile <- system.file("binfile/TESTfile.bin", package = "GENEAread")[1]

# procfile <- GENEAread::read.bin(binfile)
# procfile2 <- GENEAread::read.bin(binfile, calibrate = FALSE)
procfilelo <- read.bin(binfile, downsample = 10)
# object.size(procfilelo) / object.size(procfile)
# procfileshort <- read.bin(binfile, start = "16:50", end = "16:51")

View(procfilelo)
View(procfilelo$data.out)
