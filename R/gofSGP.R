`gofSGP` <- function(
		sgp_object,
		state=NULL,
		years=NULL,
		content_areas=NULL,
		content_areas_prior=NULL,
		grades=NULL,
		use.sgp="SGP",
		output.format="PDF",
		color.scale="reds.and.blues") {

	### To prevent R CMD check warnings

	VALID_CASE <- CONTENT_AREA <- YEAR <- SCALE_SCORE_PRIOR <- NULL


        ### Create state (if NULL) from sgp_object (if possible)

        if (is.null(state) & is.SGP(sgp_object)) {
                tmp.name <- toupper(gsub("_", " ", deparse(substitute(sgp_object))))
                state <- getStateAbbreviation(tmp.name, "gofSGP")
        }

	### Create common object for data

	if (is.SGP(sgp_object)) tmp.data <- sgp_object@Data else tmp.data <- sgp_object


	### Set up parameters based upon the existence of ACHIEVEMENT_LEVEL_PRIOR

	if ("ACHIEVEMENT_LEVEL_PRIOR" %in% names(tmp.data)) {
		with.prior.achievement.level <- TRUE
		my.width <- 8.5; my.height <- 11
		variables.to.get <- c("SCALE_SCORE_PRIOR", "ACHIEVEMENT_LEVEL_PRIOR", "CONTENT_AREA_PRIOR", use.sgp, "GRADE")
	} else {
		with.prior.achievement.level <- FALSE
		my.width <- 8.5; my.height <- 5.5
		variables.to.get <- c("SCALE_SCORE_PRIOR", use.sgp, "GRADE")
	}
	
		
	### Utility functions

	pretty_year <- function(x) sub("_", "-", x)

	gof.draw <- function(content_area.year.grade.data, content_area, year, grade, content_areas_prior) {
		
		if (!"GROB" %in% output.format) {
			file.path <- file.path("Goodness_of_Fit", paste(content_area, year, sep="."))
			dir.create(file.path, showWarnings=FALSE, recursive=TRUE)
			if ("PDF" %in% output.format) pdf(file=paste(file.path, paste("/gofSGP_Grade", grade, sep="_"), ".pdf", sep=""), width=my.width, height=my.height)
			if ("PNG" %in% output.format) Cairo(file=paste(file.path, paste("/gofSGP_Grade", grade, sep="_"), ".png", sep=""), width=my.width, height=my.height, units="in", dpi=144, pointsize=24, bg="transparent")
			grid.draw(.goodness.of.fit(content_area.year.grade.data, content_area, year, grade, color.scale=color.scale, with.prior.achievement.level=with.prior.achievement.level, 
				content_areas_prior=content_areas_prior))
			dev.off()
			return(NULL)
		} else {
			.goodness.of.fit(content_area.year.grade.data, content_area, year, grade, color.scale=color.scale, with.prior.achievement.level=with.prior.achievement.level,
				content_areas_prior=content_areas_prior)
		}
	}

	.goodness.of.fit <- 
		function(data1, content_area, year, grade, color.scale="reds", with.prior.achievement.level=FALSE, content_areas_prior=NULL) {

		.cell.color <- function(x){
		my.blues.and.reds <- diverge_hcl(21, c = 100, l = c(50, 100))
		my.reds <- c("#FFFFFF", "#FEF1E1", "#FBD9CA", "#F9C1B4", "#F7A99E", "#F59188", "#F27972", "#F0615C", "#EE4946", "#EC3130", "#EA1A1A")
		if (color.scale=="reds") {
			tmp.cell.color <- my.reds[findInterval(abs(x - 10), 1:10)+1]
			tmp.cell.color[is.na(tmp.cell.color)] <- "#000000"
		} else {
			tmp.cell.color <- my.blues.and.reds[findInterval(x-10, -10:11, all.inside=TRUE)]
			tmp.cell.color[is.na(tmp.cell.color)] <- "#000000"
		}
		return(tmp.cell.color)
		}

		.quantcut <- function (x, q = seq(0, 1, by = 0.25), na.rm = TRUE, ...) { ### From the quantcut package (thanks!!)
			quant <- quantile(x, q, na.rm = na.rm)
			dups <- duplicated(quant)
			if (any(dups)) {
				flag <- x %in% unique(quant[dups])
				retval <- ifelse(flag, paste("[", as.character(x), "]", sep = ""), NA)
				uniqs <- unique(quant)
				reposition <- function(cut) {
					flag <- x >= cut
					if (sum(flag) == 0) return(cut) else return(min(x[flag], na.rm = na.rm))
				}

				newquant <- sapply(uniqs, reposition)
				retval[!flag] <- as.character(cut(x[!flag], breaks = newquant,
				include.lowest = TRUE, ...))
				levs <- unique(retval[order(x)])
				retval <- factor(retval, levels = levs)
				mkpairs <- function(x) sapply(x, function(y) if (length(y) == 2) y[c(2, 2)] else y[2:3])
				pairs <- mkpairs(strsplit(levs, "[^0-9+\\.\\-]+"))
				rownames(pairs) <- c("lower.bound", "upper.bound")
				colnames(pairs) <- levs
				closed.lower <- rep(FALSE, ncol(pairs))
				closed.upper <- rep(TRUE, ncol(pairs))
				closed.lower[1] <- TRUE
				for (i in 2:ncol(pairs)) if (pairs[1, i] == pairs[1, i - 1] && pairs[1, i] == pairs[2, i - 1]) closed.lower[i] <- FALSE
				for (i in 1:(ncol(pairs) - 1)) if (pairs[2, i] == pairs[1, i + 1] && pairs[2, i] == pairs[2, i + 1]) closed.upper[i] <- FALSE
				levs <- ifelse(pairs[1, ] == pairs[2, ], pairs[1, ], paste(ifelse(closed.lower, "[", "("), pairs[1, ], ",", pairs[2, ], ifelse(closed.upper, "]", ")"), sep = ""))
				levels(retval) <- levs
			} else {
				retval <- cut(x, quant, include.lowest = TRUE, ...)
			}
			return(retval)
		} ## END .quantcut function


		if (max(data1[['SGP']]==100) | min(data1[['SGP']]==0)) {
			my.percentile.labels <- paste(0:9*10, "to", c(seq(9,89,10),100))
		} else {
			my.percentile.labels <- paste(c(1,1:9*10), "to", seq(9,99,10))
		}

		.sgp.fit <- function (score, sgp) {
			gfittable <- prop.table(table(.quantcut(score, q=0:10/10, right=FALSE, dig.lab=3),
			cut(sgp, c(-1, 9.5, 19.5, 29.5, 39.5, 49.5, 59.5, 69.5, 79.5, 89.5, 100.5),
			labels=my.percentile.labels)), 1)*100
			return(gfittable)
		}

		SCALE_SCORE_PRIOR <- SGP <- NULL
		tmp.table <- .sgp.fit(data1[['SCALE_SCORE_PRIOR']], data1[['SGP']])
		tmp.cuts <- .quantcut(data1[['SCALE_SCORE_PRIOR']], 0:10/10, right=FALSE)
		tmp.cuts.percentages <- round(100*table(tmp.cuts)/sum(table(tmp.cuts)), digits=1)
		tmp.colors <- .cell.color(as.vector(tmp.table))
		tmp.list <- list()

		for (i in levels(tmp.cuts)) {
			tmp.list[[i]] <- quantile(data1$SGP[tmp.cuts==i], probs=ppoints(1:500))
		}

		if (with.prior.achievement.level) {
			tmp.prior.achievement.level.percentages <- table(factor(data1[['ACHIEVEMENT_LEVEL_PRIOR']]))/(dim(data1)[1])
			tmp.prior.achievement.level.colors <- rev(diverge_hcl(length(tmp.prior.achievement.level.percentages), h = c(180, 40), c = 255, l = c(20, 100)))
			tmp.prior.achievement.level.percentages.labels <- paste("(", round(100*table(factor(data1[['ACHIEVEMENT_LEVEL_PRIOR']]))/(dim(data1)[1]), digits=1), "%)", sep="")
			if (is.null(state)) {
				tmp.prior.achievement.level.labels <- row.names(tmp.prior.achievement.level.percentages)
			} else {
				tmp.prior.achievement.level.labels <- names(SGPstateData[[state]][['Student_Report_Information']][['Achievement_Level_Labels']])
			}
			tmp.prior.achievement.level.base.points <- cumsum(tmp.prior.achievement.level.percentages)+(seq_along(tmp.prior.achievement.level.percentages)-1)/100
			tmp.prior.achievement.level.centers <- tmp.prior.achievement.level.base.points-tmp.prior.achievement.level.percentages/2
			tmp.prior.achievement.level.quantiles <- tapply(data1[['SGP']], factor(data1[['ACHIEVEMENT_LEVEL_PRIOR']]), quantile, probs=1:9/10, simplify=FALSE)
			tmp.prior.content.area.label <- paste("Prior Achievement Level (", capwords(content_areas_prior), ")", sep="")

			layout.vp <- viewport(layout = grid.layout(7, 4, widths = unit(c(0.1, 4.9, 3.4, 0.1), rep("inches", 4)),
			heights = unit(c(0.2, 1.0, 0.1, 5.4, 0.1, 4.0, 0.2), rep("inches", 2))), name="layout")
			components <- vpList(viewport(layout.pos.row=2, layout.pos.col=2:3, name="title"),
				viewport(layout.pos.row=4, layout.pos.col=2:3, xscale=c(-30,110), yscale=c(-0.25, 1.25), name="prior_achievement_level"),
				viewport(layout.pos.row=6, layout.pos.col=2, xscale=c(-3,12), yscale=c(0,13), name="table"),
				viewport(layout.pos.row=6, layout.pos.col=3, xscale=c(-25,110), yscale=c(-8,130), name="qq"))
		} else {
			layout.vp <- viewport(layout = grid.layout(5, 4, widths = unit(c(0.1, 4.9, 3.4, 0.1), rep("inches", 4)),
			heights = unit(c(0.2, 1.0, 0.1, 4.0, 0.2), rep("inches", 2))), name="layout")
			components <- vpList(viewport(layout.pos.row=2, layout.pos.col=2:3, name="title"),
			viewport(layout.pos.row=4, layout.pos.col=2, xscale=c(-3,12), yscale=c(0,13), name="table"),
			viewport(layout.pos.row=4, layout.pos.col=3, xscale=c(-25,110), yscale=c(-8,130), name="qq"))
		}


		### grob with prior achievement

		if (with.prior.achievement.level) {

			gof.grob <- gTree(childrenvp=layout.vp,
				name=paste(content_area, ".", year, ".GRADE.", grade, sep=""),
				children=gList(gTree(vp="layout",
				childrenvp=components,
				name=paste("CHILDREN.", content_area, ".", year, ".GRADE.", grade, sep=""),
					children=gList(

			### title

				roundrectGrob(gp=gpar(fill="grey95"), vp="title", r=unit(3, "mm")),
				textGrob(x=0.5, y=0.65, "Student Growth Percentile Goodness-of-Fit Descriptives", gp=gpar(cex=1.75), vp="title"),
				textGrob(x=0.5, y=0.35, paste(pretty_year(year), " ", sub(' +$', '', capwords(paste(content_area, my.extra.label))),
					", Grade ", grade, " (N = ", format(dim(data1)[1], big.mark=","), ")", sep=""), vp="title", gp=gpar(cex=1.2)),

			### prior_achievement_level

				textGrob(x=unit(0.5, "npc"), y=unit(1.15, "native"), "SGP Deciles by Prior Achievement Level", gp=gpar(cex=1.7), vp="prior_achievement_level"),
				roundrectGrob(width=0.98, r=unit(2, "mm"), vp="prior_achievement_level"),
				rectGrob(x=rep(50, length(tmp.prior.achievement.level.base.points)), y=tmp.prior.achievement.level.base.points, 
					width=rep(100, length(tmp.prior.achievement.level.base.points)), height=tmp.prior.achievement.level.percentages, 
					just=c("center", "top"), vp="prior_achievement_level", default.units="native",
					gp=gpar(col="black", fill=tmp.prior.achievement.level.colors)),
				textGrob(x=-2, y=tmp.prior.achievement.level.centers, tmp.prior.achievement.level.labels, default.units="native", 
					just="right", vp="prior_achievement_level", gp=gpar(cex=0.8)),
				textGrob(x=-25, y=0.5, tmp.prior.content.area.label, gp=gpar(cex=1), default.units="native", rot=90, vp="prior_achievement_level"),
				textGrob(x=101, y=tmp.prior.achievement.level.centers, tmp.prior.achievement.level.percentages.labels, default.units="native", 
					just="left", vp="prior_achievement_level", gp=gpar(cex=0.7)),
				linesGrob(c(1,99), -0.05, gp=gpar(lwd=1.0), default.units="native", vp="prior_achievement_level"),
				polylineGrob(y=rep(c(-0.075,-0.05), 11), x=rep(c(1,1:9*10, 99), each=2), id=rep(1:11, each=2), default.units="native", vp="prior_achievement_level"),
				textGrob(x=c(1,1:9*10,99), y=-0.115, as.character(c(1,1:9*10,99)), gp=gpar(cex=0.8), default.units="native", vp="prior_achievement_level"),
				textGrob(y=-0.18, x=50, "Median Student Growth Percentile", gp=gpar(cex=0.8), default.units="native", vp="prior_achievement_level"),
				polylineGrob(x=rep(unlist(tmp.prior.achievement.level.quantiles), each=2), 
				y=as.numeric(rbind(rep(tmp.prior.achievement.level.base.points, each=9), rep(tmp.prior.achievement.level.base.points-tmp.prior.achievement.level.percentages, each=9))),
				id=rep(1:length(unlist(tmp.prior.achievement.level.quantiles)), each=2), 
				gp=gpar(lwd=c(rep(0.4,4),1.4,rep(0.4,4)), col=c(rep("grey75",4),"white",rep("grey75",4)), lty=c(rep(2,4),1,rep(2,4))), 
				vp="prior_achievement_level", default.units="native"),

			### table

				roundrectGrob(width=0.98, r=unit(2, "mm"), vp="table"),
				rectGrob(x=rep(1:10, each=dim(tmp.table)[1]), y=rep(10:(10-dim(tmp.table)[1]+1),10), width=1, height=1, default.units="native",
					gp=gpar(col="black", fill=tmp.colors), vp="table"),
				textGrob(x=0.35, y=10:(10-dim(tmp.table)[1]+1), paste(c("1st", "2nd", "3rd", paste(4:dim(tmp.table)[1], "th", sep="")),
					dimnames(tmp.table)[[1]], sep="/"), just="right", gp=gpar(cex=0.7), default.units="native", vp="table"),
				textGrob(x=10.65, y=10:(10-dim(tmp.table)[1]+1), paste("(", tmp.cuts.percentages, "%)", sep=""), just="left", gp=gpar(cex=0.7),
					default.units="native", vp="table"),
				textGrob(x=-2.5, y=5.5, "Prior Scale Score Decile/Range", gp=gpar(cex=0.8), default.units="native", rot=90, vp="table"),
				textGrob(x=1:10, y=10.8, dimnames(tmp.table)[[2]], gp=gpar(cex=0.7), default.units="native", rot=45, just="left", vp="table"),
				textGrob(x=5.75, y=12.5, "Student Growth Percentile Range", gp=gpar(cex=0.8), default.units="native", vp="table"),
				textGrob(x=rep(1:10,each=dim(tmp.table)[1]), y=rep(10:(10-dim(tmp.table)[1]+1),10),
					formatC(as.vector(tmp.table), format="f", digits=2), default.units="native", gp=gpar(cex=0.7), vp="table"),
				textGrob(x=-2.55, y=9.0, "*", default.units="native", rot=90, gp=gpar(cex=0.7), vp="table"),
				textGrob(x=-2.05, y=0.3, "*", default.units="native", gp=gpar(cex=0.7), vp="table"),
				textGrob(x=-2.0, y=0.25, "Prior score deciles can be uneven depending upon the prior score distribution", just="left", default.units="native",
					gp=gpar(cex=0.5), vp="table"),

			### qq

				roundrectGrob(width=0.98, r=unit(2, "mm"), vp="qq"),
				polylineGrob(unlist(tmp.list), rep(ppoints(1:500)*100, length(levels(tmp.cuts))),
					id=rep(seq(length(levels(tmp.cuts))), each=500), gp=gpar(lwd=0.35), default.units="native", vp="qq"),
				linesGrob(c(0,100), c(0,100), gp=gpar(lwd=0.75, col="red"), default.units="native", vp="qq"),
				linesGrob(x=c(-3,-3,103,103,-3), y=c(-3,103,103,-3,-3), default.units="native", vp="qq"),
				polylineGrob(x=rep(c(-6,-3), 11), y=rep(0:10*10, each=2), id=rep(1:11, each=2), default.units="native", vp="qq"),
				textGrob(x=-7, y=0:10*10, 0:10*10, default.units="native", gp=gpar(cex=0.7), just="right", vp="qq"),
				polylineGrob(x=rep(0:10*10, each=2), y=rep(c(103,106), 11), id=rep(1:11, each=2), default.units="native", vp="qq"),
				textGrob(x=0:10*10, y=109, 0:10*10, default.units="native", gp=gpar(cex=0.7), vp="qq"),
				textGrob(x=45, y=123, "QQ-Plot: Student Growth Percentiles", default.units="native", vp="qq"),
				textGrob(x=50, y=115, "Theoretical SGP Distribution", default.units="native", gp=gpar(cex=0.7), vp="qq"),
				textGrob(x=-17, y=50, "Empirical SGP Distribution", default.units="native", gp=gpar(cex=0.7), rot=90, vp="qq")))))

		} else {

			gof.grob <- gTree(childrenvp=layout.vp,
				name=paste(content_area, ".", year, ".GRADE.", grade, sep=""),
				children=gList(gTree(vp="layout",
				childrenvp=components,
				name=paste("CHILDREN.", content_area, ".", year, ".GRADE.", grade, sep=""),
					children=gList(

			### title

				roundrectGrob(gp=gpar(fill="grey95"), vp="title", r=unit(3, "mm")),
				textGrob(x=0.5, y=0.65, "Student Growth Percentile Goodness-of-Fit Descriptives", gp=gpar(cex=1.75), vp="title"),
				textGrob(x=0.5, y=0.35, paste(pretty_year(year), " ", sub(' +$', '', capwords(paste(content_area, my.extra.label))),
					", Grade ", grade, " (N = ", format(dim(data1)[1], big.mark=","), ")", sep=""), vp="title", gp=gpar(cex=1.2)),

			### table

				roundrectGrob(width=0.98, r=unit(2, "mm"), vp="table"),
				rectGrob(x=rep(1:10, each=dim(tmp.table)[1]), y=rep(10:(10-dim(tmp.table)[1]+1),10), width=1, height=1, default.units="native",
					gp=gpar(col="black", fill=tmp.colors), vp="table"),
				textGrob(x=0.35, y=10:(10-dim(tmp.table)[1]+1), paste(c("1st", "2nd", "3rd", paste(4:dim(tmp.table)[1], "th", sep="")),
					dimnames(tmp.table)[[1]], sep="/"), just="right", gp=gpar(cex=0.7), default.units="native", vp="table"),
				textGrob(x=10.65, y=10:(10-dim(tmp.table)[1]+1), paste("(", tmp.cuts.percentages, "%)", sep=""), just="left", gp=gpar(cex=0.7),
					default.units="native", vp="table"),
				textGrob(x=-2.5, y=5.5, "Prior Scale Score Decile/Range", gp=gpar(cex=0.8), default.units="native", rot=90, vp="table"),
				textGrob(x=1:10, y=10.8, dimnames(tmp.table)[[2]], gp=gpar(cex=0.7), default.units="native", rot=45, just="left", vp="table"),
				textGrob(x=5.75, y=12.5, "Student Growth Percentile Range", gp=gpar(cex=0.8), default.units="native", vp="table"),
				textGrob(x=rep(1:10,each=dim(tmp.table)[1]), y=rep(10:(10-dim(tmp.table)[1]+1),10),
					formatC(as.vector(tmp.table), format="f", digits=2), default.units="native", gp=gpar(cex=0.7), vp="table"),
				textGrob(x=-2.55, y=9.2, "*", default.units="native", rot=90, gp=gpar(cex=0.7), vp="table"),
				textGrob(x=-2.05, y=0.3, "*", default.units="native", gp=gpar(cex=0.7), vp="table"),
				textGrob(x=-2.0, y=0.25, "Prior score deciles can be uneven depending upon the prior score distribution", just="left", default.units="native",
					gp=gpar(cex=0.5), vp="table"),

			### qq

				roundrectGrob(width=0.98, r=unit(2, "mm"), vp="qq"),
				polylineGrob(unlist(tmp.list), rep(ppoints(1:500)*100, length(levels(tmp.cuts))),
					id=rep(seq(length(levels(tmp.cuts))), each=500), gp=gpar(lwd=0.35), default.units="native", vp="qq"),
				linesGrob(c(0,100), c(0,100), gp=gpar(lwd=0.75, col="red"), default.units="native", vp="qq"),
				linesGrob(x=c(-3,-3,103,103,-3), y=c(-3,103,103,-3,-3), default.units="native", vp="qq"),
				polylineGrob(x=rep(c(-6,-3), 11), y=rep(0:10*10, each=2), id=rep(1:11, each=2), default.units="native", vp="qq"),
				textGrob(x=-7, y=0:10*10, 0:10*10, default.units="native", gp=gpar(cex=0.7), just="right", vp="qq"),
				polylineGrob(x=rep(0:10*10, each=2), y=rep(c(103,106), 11), id=rep(1:11, each=2), default.units="native", vp="qq"),
				textGrob(x=0:10*10, y=109, 0:10*10, default.units="native", gp=gpar(cex=0.7), vp="qq"),
				textGrob(x=45, y=123, "QQ-Plot: Student Growth Percentiles", default.units="native", vp="qq"),
				textGrob(x=50, y=115, "Theoretical SGP Distribution", default.units="native", gp=gpar(cex=0.7), vp="qq"),
				textGrob(x=-17, y=50, "Empirical SGP Distribution", default.units="native", gp=gpar(cex=0.7), rot=90, vp="qq")))))

		} ### END else

		return(gof.grob)
	} ### END .goodness.of.fit function

	### Define variables

	if (use.sgp!="SGP") my.extra.label <- use.sgp else my.extra.label <- "SGP"


	### Get arguments

	if (is.null(years)) {
		years <- unique(tmp.data[!is.na(tmp.data[[use.sgp]]),][['YEAR']])
	} 

	if (is.null(content_areas)) {
		content_areas <- unique(tmp.data[!is.na(tmp.data[[use.sgp]]),][['CONTENT_AREA']])
	}

	setkey(tmp.data, VALID_CASE, YEAR, CONTENT_AREA)

	for (years.iter in years) {
		for (content_areas.iter in content_areas) {
			tmp.data_1 <- tmp.data[data.table("VALID_CASE", years.iter, content_areas.iter)][, intersect(variables.to.get, names(tmp.data)), with=FALSE]
			if (is.null(grades)) {
				grades <- sort(unique(tmp.data_1[!is.na(tmp.data_1[[use.sgp]]),][['GRADE']]))
			}
			for (grades.iter in grades) {
				tmp.data.final <- tmp.data_1[tmp.data_1[['GRADE']]==grades.iter & !is.na(tmp.data_1[[use.sgp]]) & !is.na(SCALE_SCORE_PRIOR),]
				if ("ACHIEVEMENT_LEVEL_PRIOR" %in% names(tmp.data.final)) {
					if ("CONTENT_AREA_PRIOR" %in% names(tmp.data.final)) content_areas_prior <- tmp.data.final[["CONTENT_AREA_PRIOR"]][1]
					gof.object <- gof.draw(
						data.frame(
							SCALE_SCORE_PRIOR=tmp.data.final[['SCALE_SCORE_PRIOR']], 
							SGP=tmp.data.final[[use.sgp]], 
							ACHIEVEMENT_LEVEL_PRIOR=tmp.data.final[['ACHIEVEMENT_LEVEL_PRIOR']]), 
							content_area=content_areas.iter,
							content_areas_prior=content_areas_prior,
							year=years.iter, 
							grade=grades.iter)
				} else {
					gof.object <- gof.draw(
						data.frame(
							SCALE_SCORE_PRIOR=tmp.data.final[['SCALE_SCORE_PRIOR']], 
							SGP=tmp.data.final[[use.sgp]]), 
							content_area=content_areas.iter, 
							year=years.iter, 
							grade=grades.iter)
				}
				if (!is.null(gof.object)) return(gof.object)
			}
		}
	}
} ### END gofSGP function
