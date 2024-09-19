#relevant info: https://blog.r-hub.io/2019/05/21/nold/
install.packages("rhub")

library(rhub)

rhub_setup()

rhub_doctor()

rhub::rhub_check(
  platforms = c("linux", "macos", "windows", "ubuntu-next", "ubuntu-release", "nold")
)

