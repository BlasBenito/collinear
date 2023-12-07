#relevant info: https://blog.r-hub.io/2019/05/21/nold/
install.packages("rhub")

#check online
rhub::check(".", platform = "debian-gcc-devel-nold")

#check local
rhub::local_check_linux(".", image = "rhub/debian-gcc-devel-nold")
