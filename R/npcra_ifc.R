
ifc <- function(data, col_activity = "pim", timestamp="timestamp") {
    ism <- ism(data, col_activity, timestamp)
    message("ISm = ", ism)
    ivm <- ivm(data, col_activity, timestamp)
    message("IVm = ", ivm)
    ra <- ra(data, col_activity, timestamp, method=1)
    message("RA = ", ra)

    ifc<- (ism+ivm+ra)/3
    message("IFC = ", ifc)
    return(ifc)

}
