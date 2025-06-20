`getVersion` <-
    function(data=NULL) {
        if (is.SGP(data) && .hasSlot(data, "Version")) {
            existing <- data@Version
            return(
                Filter(Negate(is.null), 
                list(
                    SGP_Package_Version =
                      c(existing[["SGP_Package_Version"]], as.character(packageVersion("SGP"))),
                    Date_Prepared =
                      c(existing[["Date_Prepared"]], prettyDate()),
                    session_platform = if ("session_platform" %in% names(existing)) {
                      existing[["session_platform"]]
                    } else NULL,
                    attached_pkgs = if ("attached_pkgs" %in% names(existing)) {
                      existing[["attached_pkgs"]]
                    } else NULL,
                    namespace_pkgs = if ("namespace_pkgs" %in% names(existing)) {
                      existing[["namespace_pkgs"]]
                    } else NULL
                ))
            )
        } else {
            return(
                list(
                    SGP_Package_Version = as.character(packageVersion("SGP")),
                    Date_Prepared = prettyDate()
                )
            )
        }
    } ### END getVersion function
