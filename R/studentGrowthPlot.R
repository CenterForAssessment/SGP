`studentGrowthPlot` <- 
function(Scale_Scores,               ## List of Scale Scores
	Plotting_Scale_Scores,       ## Score used for plotting, if missing, then Scale_Scores are used for plotting,
                                     ## if supplied Scale_Scores used for text
	Achievement_Levels,          ## NOTE: Achievement_Levels must/should be supplied as factors with appropriate level codings 
	SGP,                         ## List of SGPs
	SGP_Levels,                  ## List of SGP Levels
	Grades,                      ## List of Grade levels for student
	Cuts_NY1,                    ## Vector of NY1 cutscores
	Connect_Points="Arrows",     ## Current "Arrows" or "None"
	Cutscores,                   ## data.frame of long formatted achievement level cutscores
	Report_Parameters) {         ## list containing Current_Year, Content_Area, State

### Load packages

require(colorspace)


### Create relevant variables

content.area.label <- SGPstateData[[Report_Parameters$State]][["Student_Report_Information"]][["Content_Areas_Labels"]][[Report_Parameters$Content_Area]]
CUTLEVEL <- level_1_curve <- NULL ## To prevent R CMD check warnings
number.achievement.level.regions <- length(SGPstateData[[Report_Parameters$State]][["Student_Report_Information"]][["Achievement_Level_Labels"]])
achievement.level.labels <- SGPstateData[[Report_Parameters$State]][["Student_Report_Information"]][["Achievement_Level_Labels"]]
number.growth.levels <- length(SGPstateData[[Report_Parameters$State]][["Growth"]][["Levels"]])
growth.level.labels <- SGPstateData[[Report_Parameters$State]][["Growth"]][["Levels"]]
growth.level.cutscores <- SGPstateData[[Report_Parameters$State]][["Growth"]][["Cutscores"]][["Cuts"]]
growth.level.cutscores.text <- SGPstateData[[Report_Parameters$State]][["Growth"]][["Cutscores"]][["Labels"]]
grades.reported.in.state <- SGPstateData[[Report_Parameters$State]][["Student_Report_Information"]][["Grades_Reported"]][[Report_Parameters$Content_Area]]
test.abbreviation <- SGPstateData[[Report_Parameters$State]][["Assessment_Program_Information"]][["Assessment_Abbreviation"]]

achievement.level.region.colors <- paste("grey", round(seq(62, 91, length=number.achievement.level.regions)), sep="")
border.color <- "grey25"
arrow.legend.color <- rev(diverge_hcl(number.growth.levels, h = c(180, 40), c = 255, l = c(20, 100)))
missing.data.symbol <- "--"


### Utility functions

ach.level.labels <- function(perlevel){
           tmp <- names(achievement.level.labels)[match(perlevel, achievement.level.labels)]
           tmp[is.na(tmp) & !is.na(perlevel)] <- perlevel[is.na(tmp) & !is.na(perlevel)]
           tmp[is.na(tmp)] <- missing.data.symbol
           return(tmp)
}

sgp.level.labels <- function(sgp_level){
           sgp_level[is.na(sgp_level)] <- missing.data.symbol
           return(sgp_level)
}

arrow.color <- function(sgp){
          arrow.legend.color[findInterval(sgp, growth.level.cutscores)+1]
}

.year.increment <- function(year, increment) {
      paste(as.numeric(unlist(strsplit(as.character(year), "_")))+increment, collapse="_")
}

get.my.cutscore.year <- function(state, content_area, year) {
        tmp.cutscore.years <- sapply(strsplit(names(SGPstateData[[state]][["Achievement"]][["Cutscores"]])[grep(content_area, names(SGPstateData[[state]][["Achievement"]][["Cutscores"]]))], "[.]"),
                function(x) x[2])
        if (year %in% tmp.cutscore.years) {
                return(year)
        } else {
                if (year==sort(c(year, tmp.cutscore.years))[1]) {
                        return(NA)
                } else {
                        return(sort(tmp.cutscore.years)[which(year==sort(c(year, tmp.cutscore.years)))-1])
                }
        }
}

interpolate.grades <- function(grades, data.year.span) {

            last.number <- function (x) {
               if (sum(!is.na(x)) > 0) return(max(which(!is.na(x))))
               else return (0)
            }

            first.number <- function (x) {
               if (sum(!is.na(x)) > 0 ) return(min(which(!is.na(x))))
               else return (0)
            }

            extend.grades <- function (x) {
                return(c(head(x,1)-1, x, tail(x,1)+1))
            }

            first.scale.score <- first.number(head(grades, data.year.span-1))
            last.scale.score <- last.number(grades)

            if (first.scale.score == 0) {
                   year_span <- 0
                   return (list(interp.df = data.frame(GRADE=2:8), 
					year_span=year_span,
					years=rep(NA, 7)))
            } else {
              if (last.scale.score < data.year.span) {
                  grades[(last.scale.score+1):data.year.span] <- (grades[last.scale.score]-1):(grades[last.scale.score] - (data.year.span - last.scale.score))
                  grades[grades < 3] <- 3
              }
                    
              if (first.scale.score > 1) {
                  grades[1:(first.scale.score-1)] <- (grades[first.scale.score] + (first.scale.score - 1)):(grades[first.scale.score]+1)
                  grades[grades > 10] <- 10
              }
                       
              grades[which(is.na(grades))] <- approx(grades, xout=which(is.na(grades)))$y

              if (grades[1] == max(grades.reported.in.state)) {
                  year_span <- 5
                  temp.grades <- extend.grades(rev(grades))
                  return (list(interp.df = data.frame(GRADE=temp.grades), 
				year_span=year_span, 
				years=sapply(seq(1-max(which(grades[1]==temp.grades)), length=length(temp.grades)), function(x) .year.increment(Report_Parameters$Current_Year, x))))
              } else {
                  if (grades[1] == 11) {
                     year_span <- 4
                     temp.grades <- extend.grades(c(rev(head(grades, -1)), 12))
                     return (list(interp.df = data.frame(GRADE=temp.grades), 
				year_span=year_span, 
				years=sapply(seq(1-max(which(grades[1]==temp.grades)), length=length(temp.grades)), function(x) .year.increment(Report_Parameters$Current_Year, x))))
                  }

                  if (grades[1] == 10) {
                     year_span <- 4
                     temp.grades <- extend.grades(c(rev(head(grades, -1)), 11))
                     return (list(interp.df = data.frame(GRADE=temp.grades), 
				year_span=year_span, 
				years=sapply(seq(1-max(which(grades[1]==temp.grades)), length=length(temp.grades)), function(x) .year.increment(Report_Parameters$Current_Year, x))))
                  }

                  if (grades[1] == 9) {
                     year_span <- 4
                     temp.grades <- extend.grades(c(rev(head(grades, -1)), 10))
                     return (list(interp.df = data.frame(GRADE=temp.grades), 
				year_span=year_span, 
				years=sapply(seq(1-max(which(grades[1]==temp.grades)), length=length(temp.grades)), function(x) .year.increment(Report_Parameters$Current_Year, x))))
                  }
             
                  if (grades[1] == 8) {
                     year_span <- 4
                     temp.grades <- extend.grades(c(rev(head(grades, -1)), 9))
                     return (list(interp.df = data.frame(GRADE=temp.grades), 
				year_span=year_span, 
				years=sapply(seq(1-max(which(grades[1]==temp.grades)), length=length(temp.grades)), function(x) .year.increment(Report_Parameters$Current_Year, x))))
                  }

                  if (grades[1] == 7) {
                     year_span <- 4
                     temp.grades <- extend.grades(c(rev(head(grades, -1)), 8))
                     return (list(interp.df = data.frame(GRADE=temp.grades), 
				year_span=year_span, 
				years=sapply(seq(1-max(which(grades[1]==temp.grades)), length=length(temp.grades)), function(x) .year.increment(Report_Parameters$Current_Year, x))))
                  }

                  if (grades[1] == 6) {
                     year_span <- 4
                     temp.grades <- extend.grades(c(rev(head(grades, -1)), 7))
                     return (list(interp.df = data.frame(GRADE=temp.grades), 
				year_span=year_span, 
				years=sapply(seq(1-max(which(grades[1]==temp.grades)), length=length(temp.grades)), function(x) .year.increment(Report_Parameters$Current_Year, x))))
                  }

                  if (grades[1] == 5) {
                     year_span <- max(min(last.scale.score, 4), 3)
                     grades <- head(grades, year_span)
                     temp.grades <- extend.grades(c(rev(grades), head(6:7, data.year.span-year_span)))
                     return (list(interp.df = data.frame(GRADE=temp.grades), 
				year_span=year_span, 
				years=sapply(seq(1-max(which(grades[1]==temp.grades)), length=length(temp.grades)), function(x) .year.increment(Report_Parameters$Current_Year, x))))
                  }

                  if (grades[1] == 4) {
                     year_span <- max(min(last.scale.score, 4), 2)
                     grades <- head(grades, year_span)
                     temp.grades <- extend.grades(c(rev(grades), head(5:7, data.year.span-year_span)))
                     return (list(interp.df = data.frame(GRADE=temp.grades), 
				year_span=year_span, 
				years=sapply(seq(1-max(which(grades[1]==temp.grades)), length=length(temp.grades)), function(x) .year.increment(Report_Parameters$Current_Year, x))))
                  }

                  if (grades[1] == 3) {
                     year_span <- max(min(last.scale.score, 4), 1)
                     grades <- head(grades, year_span)
                     temp.grades <- extend.grades(c(rev(grades), head(4:7, data.year.span-year_span)))
                     return (list(interp.df = data.frame(GRADE=temp.grades), 
				year_span=year_span, 
				years=sapply(seq(1-max(which(grades[1]==temp.grades)), length=length(temp.grades)), function(x) .year.increment(Report_Parameters$Current_Year, x))))
                  }
              } 
       }
} 

year.function <- function(year, add.sub, vec.length, output.type="numeric") {
        if (is.numeric(year)) {
		return(seq(from=year+add.sub, length=vec.length))
	} else { 
                tmp <- as.numeric(unlist(strsplit(as.character(year), "_")))+add.sub
		if (output.type=="numeric") {
			return(seq(from=tmp[2], length=vec.length))
		} else {
			return(paste(seq(from=tmp[1], length=vec.length), "-", seq(from=tmp[2], length=vec.length), sep=""))
		}
        }
}


grade.values <- interpolate.grades(Grades, 5)

if (grade.values$year_span == 5) {
                    low.year <- year.function(Report_Parameters$Current_Year, -4, 1)
                    high.year <- year.function(Report_Parameters$Current_Year, 0, 1)
                    year.text <- year.function(Report_Parameters$Current_Year, -4, 5, "character")

                    grades.text <- paste("Grade", Grades[5:1])
                    grades.text[which(is.na(Grades[5:1]))] <- missing.data.symbol 
    
                    scale.scores.values <- Plotting_Scale_Scores[5:1]
                    scale.scores.text <- as.character(Scale_Scores[5:1])
                    scale.scores.text[which(is.na(Scale_Scores[5:1]))] <- missing.data.symbol

                    ach.levels.text <- ach.level.labels(Achievement_Levels[5:1])

                    gp.values <- SGP[4:1]
                    gp.text <- SGP[4:1]
                    gp.text[which(is.na(SGP[4:1]))] <- missing.data.symbol

                    gp.levels.text <- sgp.level.labels(SGP_Levels[4:1])

                    cuts.ny1.text <- rep(NA, number.growth.levels)
}

if (grade.values$year_span == 4) {
                    low.year <- year.function(Report_Parameters$Current_Year, -3, 1)
                    high.year <- year.function(Report_Parameters$Current_Year, 1, 1) 
                    year.text <- c(year.function(Report_Parameters$Current_Year, -3, 4, "character"), " ")

                    grades.text <- c(paste("Grade", Grades[4:1]), "Next Year")
                    grades.text[which(is.na(Grades[4:1]))] <- missing.data.symbol

                    scale.scores.values <- c(Plotting_Scale_Scores[4:1], NA)
                    scale.scores.text <- c(Scale_Scores[4:1], " ")
                    scale.scores.text[which(is.na(Scale_Scores[4:1]))] <- missing.data.symbol

                    ach.levels.text <- c(ach.level.labels(Achievement_Levels[4:1]), " ")

                    gp.values <- c(SGP[3:1], NA)
                    gp.text <- c(SGP[3:1], " ")
                    gp.text[which(is.na(SGP[3:1]))] <- missing.data.symbol

                    gp.levels.text <- c(sgp.level.labels(SGP_Levels[3:1]), " ")

                    cuts.ny1.text <- Cuts_NY1
}

if (grade.values$year_span == 3) {
                    low.year <- year.function(Report_Parameters$Current_Year, -2, 1)
                    high.year <- year.function(Report_Parameters$Current_Year, 2, 1) 
                    year.text <- c(year.function(Report_Parameters$Current_Year, -2, 3, "character"), rep(" ", 2))

                    grades.text <- c(paste("Grade", Grades[3:1]), "Next Year", " ")
                    grades.text[which(is.na(Grades[3:1]))] <- missing.data.symbol
                     
                    scale.scores.values <- c(Plotting_Scale_Scores[3:1], rep(NA, 2))
                    scale.scores.text <- c(Scale_Scores[3:1], rep(" ", 2)) 
                    scale.scores.text[which(is.na(Scale_Scores[3:1]))] <- missing.data.symbol
         
                    ach.levels.text <- c(ach.level.labels(Achievement_Levels[3:1]), rep(" ", 2))

                    gp.values <- c(SGP[2:1], rep(NA, 2))
                    gp.text <- c(SGP[2:1], rep(" ", 2))
                    gp.text[which(is.na(SGP[2:1]))] <- missing.data.symbol

                    gp.levels.text <- c(sgp.level.labels(SGP_Levels[2:1]), rep(" ", 2))

                    cuts.ny1.text <- Cuts_NY1
}

if (grade.values$year_span == 2) {
                    low.year <- year.function(Report_Parameters$Current_Year, -1, 1)
                    high.year <- year.function(Report_Parameters$Current_Year, 3, 1) 
                    year.text <- c(year.function(Report_Parameters$Current_Year, -1, 2, "character"), rep(" ", 3))

                    grades.text <- c(paste("Grade", Grades[2:1]), "Next Year", rep(" ", 2))
                    grades.text[which(is.na(Grades[2:1]))] <- missing.data.symbol                     

                    scale.scores.values <- c(Plotting_Scale_Scores[2:1], rep(NA, 3))
                    scale.scores.text <- c(Scale_Scores[2:1], rep(" ", 3)) 
                    scale.scores.text[which(is.na(Scale_Scores[2:1]))] <- missing.data.symbol

                    ach.levels.text <- c(ach.level.labels(Achievement_Levels[2:1]), rep(" ", 3))

                    gp.values <- c(SGP[1], rep(NA, 3))
                    gp.text <- c(SGP[1], rep(" ", 3))
                    gp.text[which(is.na(SGP[1]))] <- missing.data.symbol

                    gp.levels.text <- c(sgp.level.labels(SGP_Levels[1]), rep(" ", 3))

                    cuts.ny1.text <- Cuts_NY1
}

if (grade.values$year_span == 1) {
                    low.year <- year.function(Report_Parameters$Current_Year, 0, 1)
                    high.year <- year.function(Report_Parameters$Current_Year, 4, 1) 
                    year.text <- c(year.function(Report_Parameters$Current_Year, 0, 1, "character"), rep(" ", 4))

                    grades.text <- c(paste("Grade", Grades[1]), "Next Year", rep(" ", 3))

                    scale.scores.values <- c(Plotting_Scale_Scores[1], rep(NA, 4))
                    scale.scores.text <- c(Scale_Scores[1], rep(" ", 4)) 
                    scale.scores.text[which(is.na(Scale_Scores[1]))] <- missing.data.symbol

                    ach.levels.text <- c(ach.level.labels(Achievement_Levels[1]), rep(" ", 4))

                    gp.values <- rep(NA, 4)
                    gp.text <- rep(" ", 4)

                    gp.levels.text <- rep(" ", 4)

                    cuts.ny1.text <- Cuts_NY1
}

if (grade.values$year_span == 0) {
                    low.year <- year.function(Report_Parameters$Current_Year, 0, 1)
                    high.year <- year.function(Report_Parameters$Current_Year, 4, 1) 
                    year.text <- rep(" ", 5)

                    grades.text <- rep(" ", 5)

                    scale.scores.values <- rep(NA, 5)
                    scale.scores.text <- rep(" ", 5) 

                    ach.levels.text <- rep(" ", 5)

                    gp.values <- rep(NA, 4)
                    gp.text <- rep(" ", 4)

                    gp.levels.text <- rep(" ", 4)

                    cuts.ny1.text <- rep(NA, number.growth.levels)
}

current.year <- year.function(Report_Parameters$Current_Year, 0, 1)
xscale.range <- range(low.year,high.year) + c(-0.075, 0.1)*diff(range(low.year,high.year))
if (Report_Parameters$Content_Area %in% names(SGPstateData[[Report_Parameters$State]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]])) {
   tmp.range <- range(SGPstateData[[Report_Parameters$State]][["Student_Report_Information"]][["Transformed_Achievement_Level_Cutscores"]][[Report_Parameters$Content_Area]], na.rm=TRUE)
   low.score <- min(cuts.ny1.text,
                    Plotting_Scale_Scores,
                    tmp.range,
                    na.rm=TRUE)
   high.score <- max(cuts.ny1.text,
                     Plotting_Scale_Scores,
                     tmp.range,
                     na.rm=TRUE)
   tmp.extend.range <- extendrange(c(low.score,high.score), f=0.1)
   yscale.range <- extendrange(c(min(tmp.range[1], tmp.extend.range[1]), max(tmp.range[2], tmp.extend.range[2])), f=0.05)
} else {
   low.score <- min(cuts.ny1.text, 
                    Plotting_Scale_Scores, 
                    Cutscores$CUTSCORE[Cutscores$GRADE==max(grade.values$interp.df$GRADE[2], 3) & Cutscores$CUTLEVEL==1], 
                    na.rm=TRUE)
   high.score <- max(cuts.ny1.text, 
                     Plotting_Scale_Scores, 
                     Cutscores$CUTSCORE[Cutscores$GRADE==min(tail(grade.values$interp.df$GRADE, 2)[1], 10) & Cutscores$CUTLEVEL==number.achievement.level.regions-1], 
                     na.rm=TRUE)
   yscale.range <- extendrange(c(low.score,high.score), f=0.15)
}

subject.report.vp <- viewport(layout = grid.layout(2, 3, widths = unit(c(1.15, 5.4, 1.5)/8.05, rep("npc", 3)), 
                                              heights = unit(c(2.45, 0.9)/3.35, rep("npc", 2))))

growth.chart.vp <- viewport(name="growth.chart.vp",
                    layout.pos.row=1, layout.pos.col=2,
                    xscale=xscale.range, 
                    yscale=yscale.range,
                    gp=gpar(fill="transparent"))

right.vp <- viewport(name="right.vp",
                    layout.pos.row=1, layout.pos.col=3,
                    xscale=c(0,1),
                    yscale=c(0,1),
                    gp=gpar(fill="transparent"))

left.vp <- viewport(name="left.vp",
                    layout.pos.row=1, layout.pos.col=1,
                    xscale=c(0, 1), 
                    yscale=yscale.range,
                    gp=gpar(fill="transparent"))

growth.and.left.vp <- viewport(name="growth.and.left.vp",
                    layout.pos.row=1, layout.pos.col=1:2)

bottom.vp <- viewport(name="bottom.vp",
                    layout.pos.row=2, layout.pos.col=2,
                    xscale=xscale.range,
                    yscale=c(0,3),
                    gp=gpar(fill="transparent"))

bottom.left.right.vp <- viewport(name="bottom.left.right.vp",
                    layout.pos.row=2, layout.pos.col=1:3,
                    xscale=c(0,1),
                    yscale=c(0,3),
                    gp=gpar(fill="transparent"))

bottom.right.vp <- viewport(name="bottom.right.vp",
                    layout.pos.row=2, layout.pos.col=3,
                    xscale=c(0,1),
                    yscale=c(0,3),
                    gp=gpar(fill="transparent"))

bottom.left.vp <- viewport(name="bottom.left.vp",
                    layout.pos.row=2, layout.pos.col=1,
                    xscale=c(0,1),
                    yscale=c(0,3),
                    gp=gpar(fill="transparent"))


### Growth.Chart Viewport

pushViewport(subject.report.vp)
pushViewport(growth.chart.vp)

for (i in seq(number.achievement.level.regions-1)){
    temp <- cbind(temp_id=seq_len(nrow(grade.values$interp.df)), grade.values$interp.df, YEAR=grade.values$years)
    temp$YEAR <- sapply(temp$YEAR, function(x) get.my.cutscore.year(Report_Parameters$State, Report_Parameters$Content_Area, as.character(x)))
    temp <- merge(temp, subset(Cutscores, CUTLEVEL==i), all.x=TRUE)
    temp <- temp[order(temp$temp_id),]$CUTSCORE
    temp[which(is.na(temp))] <- approx(temp, xout=which(is.na(temp)))$y
    assign(paste("level_", i, "_curve", sep=""), splinefun((low.year-1):(high.year+1), temp, method="mono"))
}

tmp.x.points <- seq(xscale.range[1], xscale.range[2], length=40)
x.boundary.values.1 <- c(xscale.range[1], tmp.x.points, xscale.range[2])
y.boundary.values.1 <- c(yscale.range[1], eval(parse(text="level_1_curve(tmp.x.points)")), yscale.range[1])
assign(paste("x.boundary.values.", number.achievement.level.regions, sep=""), 
       c(xscale.range[1], tmp.x.points, xscale.range[2]))
assign(paste("y.boundary.values.", number.achievement.level.regions, sep=""), 
       c(yscale.range[2], eval(parse(text=paste("level_", number.achievement.level.regions-1, "_curve(tmp.x.points)", sep=""))), yscale.range[2]))

if (number.achievement.level.regions > 2) {
    for (i in 2:(number.achievement.level.regions-1)) { 
        assign(paste("x.boundary.values.", i, sep=""), 
               c(tmp.x.points, rev(tmp.x.points)))

        assign(paste("y.boundary.values.", i, sep=""), 
               eval(parse(text=paste("c(level_", i, "_curve(tmp.x.points), level_", i-1, "_curve(rev(tmp.x.points)))", sep=""))))
    }
}

for (i in seq(number.achievement.level.regions)){
grid.polygon(x=get(paste("x.boundary.values.", i, sep="")),
             y=get(paste("y.boundary.values.", i, sep="")),
             default.units="native",
             gp=gpar(fill=achievement.level.region.colors[i], lwd=0.8, col="white"))
}


if (grade.values$year_span == 0) {
grid.text(x=0.5, y=0.5, paste("No", test.abbreviation, "Data"), gp=gpar(col=border.color, cex=2))
}


if (Connect_Points=="Arrows") {
   growth.arrow.coors.x <- c(.05, .85, .8, 1, .8, .85, .05, .053, .0555, .0575, .0585, .059, .0585, .0575, .0555, .053, .05)
   growth.arrow.coors.y <- c(-.2, -.2, -.5, 0, .5, .2,  seq(.2, -.2, length=11))

   for (i in 1:length(gp.values)){
     if (!is.na(gp.values[i])){
          arrow.rise <- convertY(unit(scale.scores.values[i+1], "native") - unit(scale.scores.values[i], "native"), "inches")
          arrow.run <- convertX(unit(1, "native") - unit(0, "native"), "inches")
          arrow.angle <- atan2(as.numeric(arrow.rise),as.numeric(arrow.run))*180/pi

     ## Arrows connecting achievement scores

     pushViewport(viewport(x=unit(low.year+i-1, "native"), y=unit(scale.scores.values[i], "native"), 
                  width=unit(0.92/cos(arrow.angle*pi/180), "native"), height=unit(0.05, "npc"), angle=arrow.angle, just=c("left", "center"),
                  xscale=c(0, 1), yscale=c(-0.5, 0.5)))
     grid.polygon(x=growth.arrow.coors.x, y=growth.arrow.coors.y, default.units="native", gp=gpar(lwd=0.3, col=border.color, fill=arrow.color(as.numeric(gp.values[i]))))
     grid.curve(0.05, -0.2, 0.05, 0.2, curvature=0.3, ncp=11, square=FALSE, default.units="native", gp=gpar(lwd=0.3, col=border.color))
     popViewport()

     ## Horizonal Arrows

#     pushViewport(viewport(x=unit(low.year+i-1, "native"), y=unit((scale.scores.values[i]+scale.scores.values[i+1])/2, "native"),
#                  width=unit(0.92, "native"), height=unit(0.05, "npc"), just=c("left", "center"), xscale=c(0, 1), yscale=c(-0.5, 0.5)))
#     grid.polygon(x=growth.arrow.coors.x, y=growth.arrow.coors.y, default.units="native", gp=gpar(lwd=0.3, col=border.color, fill=arrow.color(as.numeric(gp.values[i]))))
#     grid.curve(0.05, -0.2, 0.05, 0.2, curvature=0.3, ncp=11, square=FALSE, default.units="native", gp=gpar(lwd=0.3, col=border.color))
#     popViewport()
     }
   }
} ## END Connect_Points=="Arrows"

if (Grades[1] != max(grades.reported.in.state) & !is.na(cuts.ny1.text[1])){

for (i in seq(number.growth.levels)) {
   grid.polygon(x=c(current.year, rep(current.year+1, 2), current.year), 
                y=c(scale.scores.values[which(current.year==low.year:high.year)], max(yscale.range[1], cuts.ny1.text[i]), 
			min(yscale.range[2], cuts.ny1.text[i+1]), scale.scores.values[which(current.year==low.year:high.year)]),
			default.units="native", gp=gpar(col=NA, lwd=0, fill=arrow.legend.color[i], alpha=0.45))
   grid.roundrect(x=unit(current.year+1, "native"), y=unit((max(yscale.range[1], cuts.ny1.text[i])+min(yscale.range[2], cuts.ny1.text[i+1]))/2, "native"), 
               height=unit(min(yscale.range[2], as.numeric(cuts.ny1.text[i+1])) - max(yscale.range[1], as.numeric(cuts.ny1.text[i])), "native"), width=unit(0.04, "native"), 
               r=unit(0.45, "snpc"), gp=gpar(lwd=0.3, col=border.color, fill=arrow.legend.color[i]))

   grid.text(x=current.year+1+.05, y=(max(yscale.range[1], cuts.ny1.text[i])+min(yscale.range[2], cuts.ny1.text[i+1]))/2, growth.level.labels[i],
             default.units="native", just="left", gp=gpar(cex=.4, col=border.color))
   }
}

grid.circle(x=low.year:high.year, y=scale.scores.values, r=unit(0.04, "inches"),
               gp=gpar(col=border.color, lwd=0.7, fill="white"), default.units="native") 

popViewport()


### Left Viewport

pushViewport(left.vp)

y.boundary.legend.1 <- c(yscale.range[1], yscale.range[1], rep(level_1_curve(xscale.range[1]), 2))
assign(paste("y.boundary.legend.", number.achievement.level.regions, sep=""),
       c(yscale.range[2], yscale.range[2], rep(eval(parse(text=paste("level_", number.achievement.level.regions-1, "_curve(xscale.range[1])", sep=""))), 2))) 

if (number.achievement.level.regions > 2) {
    for (i in 2:(number.achievement.level.regions-1)) { 
       assign(paste("y.boundary.legend.", i, sep=""),
              eval(parse(text=paste("c(rep(level_", i-1, "_curve(xscale.range[1]), 2), rep(level_", i, "_curve(xscale.range[1]), 2))", sep=""))))
    }
}

for (i in seq(number.achievement.level.regions)){
grid.polygon(x=c(0,1,1,0), 
             y=get(paste("y.boundary.legend.", i, sep="")),
             default.units="native",
             gp=gpar(fill=achievement.level.region.colors[i], lwd=0.5, col=border.color, alpha=0.7))
}

grid.text(x=.9, y=(level_1_curve(xscale.range[1]) + yscale.range[1])/2, names(achievement.level.labels)[1], 
          gp=gpar(col=border.color, fontface=2, fontfamily="Helvetica-Narrow", cex=.75), default.units="native", just="right")
grid.text(x=.9, y=(eval(parse(text=paste("level_", number.achievement.level.regions-1, "_curve(xscale.range[1])", sep=""))) + yscale.range[2])/2, 
          names(achievement.level.labels)[number.achievement.level.regions], 
          gp=gpar(col=border.color, fontface=2, fontfamily="Helvetica-Narrow", cex=.85), default.units="native", just="right")

if (number.achievement.level.regions > 2) {
    for (i in 2:(number.achievement.level.regions-1)) {
         grid.text(x=.9, y=(eval(parse(text=paste("(level_", i-1, "_curve(xscale.range[1]) + level_", i, "_curve(xscale.range[1]))/2", sep="")))),
                   names(achievement.level.labels)[i], 
                   gp=gpar(col=border.color, fontface=2, fontfamily="Helvetica-Narrow", cex=.85), default.units="native", just="right")
    }
}

grid.lines(0, c(yscale.range[1], yscale.range[2]), gp=gpar(lwd=.8, col=border.color), default.units="native")

popViewport()

pushViewport(growth.and.left.vp)
if (grade.values$year_span == 0) {grid.roundrect(r=unit(.01, "snpc"), gp=gpar(lwd=1.8, col=border.color, clip=TRUE, fill=rgb(1, 1, 1, 0.5)))}
else {grid.roundrect(r=unit(.01, "snpc"), gp=gpar(lwd=1.8, col=border.color, clip=TRUE))}
popViewport()


### Bottom Viewport

pushViewport(bottom.vp)


grid.text(x=low.year:high.year, y=2.67, grades.text, gp=gpar(col=border.color, cex=.75), default.units="native")
grid.text(x=low.year:high.year, y=2.3, year.text, gp=gpar(col=border.color, cex=.6), default.units="native")


grid.text(x=low.year:high.year, y=1.7, scale.scores.text, gp=gpar(col=border.color, cex=.65), default.units="native")

grid.text(x=low.year:high.year, y=1.3, ach.levels.text, gp=gpar(col=border.color, cex=.6), default.units="native")


grid.text(x=(low.year+1):high.year-0.5, y=0.7, gp.text, gp=gpar(col=border.color, cex=.65), default.units="native")


grid.text(x=(low.year+1):high.year-0.5, y=0.3, gp.levels.text, gp=gpar(col=border.color, cex=.6), default.units="native")
popViewport()


pushViewport(bottom.left.right.vp)
grid.lines(x=c(0,1), y=2, gp=gpar(lwd=1.8, col=border.color), default.units="native")
grid.lines(x=c(0,1), y=1, gp=gpar(lwd=1, col=border.color), default.units="native")
grid.lines(x=c(0,1), y=0, gp=gpar(lwd=1.8, col=border.color), default.units="native")
popViewport()

pushViewport(bottom.right.vp)
grid.text(x=0.1, y=1.5, "Achievement", gp=gpar(col=border.color, cex=1.2), just="left", default.units="native")
grid.text(x=0.1, y=.5, "Growth", gp=gpar(col=border.color, cex=1.2), just="left", default.units="native")
popViewport()


### Bottom Left Viewport

pushViewport(bottom.left.vp)

grid.text(x=0.9, y=1.7, "Scale Score", gp=gpar(col=border.color, cex=.7), just="right", default.units="native")
grid.text(x=0.9, y=1.3, "Achievement Level", gp=gpar(col=border.color, cex=.7), just="right", default.units="native")

grid.text(x=0.9, y=0.7, "Growth Percentile", gp=gpar(col=border.color, cex=.7), just="right", default.units="native")
grid.text(x=0.9, y=0.3, "Growth Level", gp=gpar(col=border.color, cex=.7), just="right", default.units="native")

popViewport()


### Right Viewport

pushViewport(right.vp)

grid.roundrect(width=unit(0.95, "native"), r=unit(.02, "snpc"), gp=gpar(lwd=1.8, col=border.color), just="center")

grid.text(x=.5, y=.875, content.area.label, gp=gpar(col=border.color, cex=1.8, fontface=2, fontfamily="Helvetica-Narrow"), 
          default.units="native") ## For PDF Versions
grid.text(x=0.08, y=0.75, "Achievement", gp=gpar(col=border.color, cex=.85, fontface=2, fontfamily="Helvetica-Narrow"), default.units="native", just="left")
grid.text(x=0.08, y=0.525, "Growth", gp=gpar(col=border.color, cex=.75, fontface=2, fontfamily="Helvetica-Narrow"), default.units="native", just="left")
grid.text(x=0.275, y=0.455, "Level", gp=gpar(col=border.color, cex=.6), default.units="native", just="center")
grid.text(x=0.75, y=0.455, "Percentiles", gp=gpar(col=border.color, cex=.6), default.units="native", just="center")

grid.roundrect(x=unit(0.3, "native"), y=unit(0.64, "native"), width=unit(0.3, "native"), height=unit(0.1, "native"), r=unit(0.06, "char"), 
               gp=gpar(col="grey72", lwd=0.4, fill="grey72"))
grid.circle(x=0.3, y=0.64, r=unit(0.04, "inches"), gp=gpar(col=border.color, lwd=0.7, fill="white"), default.units="native") 
grid.text(x=0.7, y=0.658, paste(test.abbreviation, content.area.label), gp=gpar(col=border.color, cex=.5), default.units="native")
grid.text(x=0.7, y=0.622, "Scale Score", gp=gpar(col=border.color, cex=.5), default.units="native")

y.center <- seq(0.05, 0.4, length=number.growth.levels+1)
arrow.legend.coors.x <- c(.25, .75, .75, 1, .5, 0, .25)
arrow.legend.coors.y <- c(0, 0, 1.3, 1.1, 2, 1.1, 1.3)

for (i in seq(number.growth.levels)) {
   pushViewport(viewport(x=unit(0.3, "native"), y=unit(y.center[i], "native"), 
                width=unit(0.07, "native"), height=unit(y.center[2]-y.center[1], "npc"), just=c("center", "bottom"),
                xscale=c(0, 1), yscale=c(0, 2)))
   grid.polygon(x=arrow.legend.coors.x, y=arrow.legend.coors.y, default.units="native", 
                gp=gpar(lwd=0.3, col=border.color, fill=arrow.legend.color[i]))

   popViewport()

   pushViewport(viewport(x=unit(0.2, "native"), y=unit(y.center[i], "native"),
                width=unit(0.04, "native"), height=unit(y.center[2]-y.center[1], "npc"), just=c("center", "bottom")))
   grid.roundrect(x=0.5, y=0.5, width=1, height=1, r=unit(.45, "snpc"), 
                  gp=gpar(lwd=0.3, col=border.color, fill=arrow.legend.color[i]))
   popViewport()

   grid.polygon(x=c(0.05, rep(0.1875, 2)),
                y=c((head(y.center,1)+tail(y.center,1))/2, y.center[i], y.center[i]+y.center[2]-y.center[1]), default.units="native", 
                gp=gpar(col=NA, lwd=0, fill=arrow.legend.color[i], alpha=0.45)) 

   grid.text(x=0.375, y=((y.center[1]+y.center[2])/2)+(i-1)*(y.center[2]-y.center[1]), growth.level.labels[i], default.units="native", 
             gp=gpar(col=border.color, cex=.5), just="left")
   grid.text(x=0.925, y=((y.center[1]+y.center[2])/2)+(i-1)*(y.center[2]-y.center[1]), growth.level.cutscores.text[i], default.units="native", 
             gp=gpar(col=border.color, cex=.5), just="right")
}

popViewport(2)

} ## END studentGrowthPlot Function
