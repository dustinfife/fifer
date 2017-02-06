##' Create a color-coded table in latex
##'
##' Create a color-coded table in latex
##'	
##' @param data the dataset in "long" format, where the conditions are on columns.
##' @param file the location where the file should be exported. 
##' @param dep.var A string naming the dependent variable
##' @param row.factor A vector of one or two elements, each of which is a string indicating the name of the row factor(s)
##' @param col.factor A vector of one or two elements, each of which is a string indicating the name of the col factor(s)
##' @param breaks The number of breaks in color. Defaults to 4.
##' @param round How many digits should the matrix be rounded to? Defaults to 2. 
##' @param rng The range of values used for coloring the plot. 
##' @param ... Other arguments passed to Hmisc
##' @aliases color.table col.table
##' @seealso \code{\link{Hmisc:latex}}
##' @author Dustin Fife
##' @importFrom Hmisc latex
##' @export
##' @examples
##' ## do this later
colored.table = function(data, file, dep.var, row.factors, col.factors, breaks=4, round=2, rng=NULL, FUN=mean, row.prefix=NULL, col.prefix=NULL, custom.breaks=NULL, return.table=F, ...){

	#### average across conditions
	f = make.formula(dep.var, c(row.factors, col.factors))
	agg = aggregate(f, data=data, FUN=FUN)

	if (length(row.factors)>2 | length(col.factors)>2){
		stop("Only two factors are allowed in either the rows or columns of the matrix")
	}

	row.vals = unique(agg[,row.factors]); 
		if (length(row.factors)>1){
			row.vals = row.vals[order(row.vals[,1], row.vals[,2]),]
			nr = nrow(row.vals)
		} else {
			nr = length(row.vals)
		}
	col.vals = unique(agg[,col.factors]); 
		if (length(col.factors)>1){
			col.vals = col.vals[order(col.vals[,1], col.vals[,2]),]	
			nc = nrow(col.vals)
		} else {
			nc = length(col.vals)
		}

	results = matrix(nrow=nr, ncol=nc)
	for (i in 1:nr){
		for (j in 1:nc){
	
			ag.row = matrix(data=unlist(c(agg[,row.factors])), nrow=nrow(agg))
			ag.col = matrix(data=unlist(c(agg[,col.factors])), nrow=nrow(agg))		
			if (length(row.factors)>1){
				row.match =(apply(ag.row, 1, identical, as.vector(unlist(c(row.vals[i,])))))
			} else {
				row.match =(apply(ag.row, 1, identical, as.vector(unlist(c(row.vals[i])))))
			}
			if (length(col.factors)>1){
				col.match =(apply(ag.col, 1, identical, as.vector(unlist(c(col.vals[j,]))))) 		
			} else {
				col.match =(apply(ag.col, 1, identical, as.vector(unlist(c(col.vals[j]))))) 		
			}
	
			
			if (length(which(row.match & col.match))>0){
				k = agg[row.match & col.match,]
				
				
				results[i,j] =(k[,dep.var])
			}
		}
	}

	#### set colors
	if (!is.null(rng)){
		max.r = max(abs(results), na.rm=T)/max(rng,na.rm=T)
	} else {
		max.r = 1
		rng = range(abs(results),na.rm=T)
	}


	#### color code results
	if (!is.null(custom.breaks)){
		custom.breaks = c(-Inf, custom.breaks, Inf)
		cols =(seq(from=1, to = .4+ ((1-max.r)*.4), length.out=length(custom.breaks)-1)	)
		cols = paste0("cellcolor[gray]{", round(cols, digits=2), "}")	
		col.mat = matrix(
				cut(abs(results), breaks=(custom.breaks), labels=cols, right=F),
				nrow=nrow(results))		
	} else {
		cols =(seq(from=1, to = .2+ ((1-max.r)*.2), length.out=breaks)	)
		cols = paste0("cellcolor[gray]{", round(cols, digits=2), "}")	
		col.mat = matrix(
				cut(abs(results), breaks=seq(from=rng[1], to=rng[2], length.out=breaks+1), labels=cols, include.lowest=T), 
				nrow=nrow(results))
	}			
	results = round(results, digits=round)
	
	if (is.null(row.prefix)){
		row.prefix = paste0(row.factors,"=")
	}
	
	if (is.null(col.prefix)){
		col.prefix= paste0(col.factors, "=")
	}

	if (length(row.factors)==2){
		row.major = paste0(row.prefix[1], sprintf("%.2f", unique(row.vals[,1])))
		row.minor = paste0(row.prefix[2], sprintf("%.2f", unique(row.vals[,2])))
		row.minor = rep(row.minor, times=length(row.major))
	} else {
		row.major = paste0(row.prefix[1], unique(row.vals))
		row.minor=NA
	}
	
	if (length(col.factors)==2){
		if (is.numeric(col.vals[,1])){
			col.major = paste0(col.prefix[1], sprintf("%.2f", unique(col.vals[,1])))
		} else {
			col.major = col.vals[,1]
		}
		if (is.numeric(col.vals[,2])){
			col.minor = paste0(col.prefix[2], sprintf("%.2f", unique(col.vals[,2])))
		} else {
			col.minor = col.vals[,2]
		}
		col.minor = rep(col.minor, times=length(col.major))
	} else {
		col.major = paste0(col.prefix[1], unique(col.vals))	
		col.minor=NA
	}
	
	if (length(col.factors)==2 & length(row.factors)==2){
		latex(results, file=file,rgroup = row.major, title='', n.rgroup = rep(nrow(results)/length(row.major), times=length(row.major)), 
					cgroup = col.major, n.cgroup = rep(ncol(results)/length(col.major), times=length(col.major)),
					rowname=row.minor, colheads=col.minor, cellTexCmds = latexTranslate(col.mat),...) 
	} else if (length(col.factors)==2){
		latex(results, file=file,title='',  
					cgroup = col.major, n.cgroup = rep(ncol(results)/length(col.major), times=length(col.major)),
					rowname=row.major, colheads=col.minor, cellTexCmds = latexTranslate(col.mat),...) 
	} else if (length(row.factors)==2){
		latex(results, file=file,rgroup = row.major, title='', n.rgroup = rep(nrow(results)/length(row.major), times=length(row.major)), 
					rowname=row.minor, colheads=col.major, cellTexCmds = latexTranslate(col.mat),...) 
	} else {
		latex(results, file=file, title='', rowname=row.major, colheads=col.major, cellTexCmds = latexTranslate(col.mat), ...) 		
	}
	list(results=results, colors=col.mat)
}


latex = function (object, title = first.word(deparse(substitute(object))), 
    file = paste(title, ".tex", sep = ""), append = FALSE, label = title, 
    rowlabel = title, rowlabel.just = "l", cgroup = NULL, n.cgroup = NULL, 
    rgroup = NULL, n.rgroup = NULL, cgroupTexCmd = "bfseries", 
    rgroupTexCmd = "bfseries", rownamesTexCmd = NULL, colnamesTexCmd = NULL, 
    cellTexCmds = NULL, rowname, cgroup.just = rep("c", length(n.cgroup)), 
    colheads = NULL, extracolheads = NULL, extracolsize = "scriptsize", 
    dcolumn = FALSE, numeric.dollar = !dcolumn, cdot = FALSE, 
    longtable = FALSE, draft.longtable = TRUE, ctable = FALSE, 
    booktabs = FALSE, table.env = TRUE, here = FALSE, lines.page = 40, 
    caption = NULL, caption.lot = NULL, caption.loc = c("top", 
        "bottom"), star = FALSE, double.slash = FALSE, vbar = FALSE, 
    collabel.just = rep("c", nc), na.blank = TRUE, insert.bottom = NULL, 
    insert.bottom.width = NULL, insert.top = NULL, first.hline.double = !(booktabs | 
        ctable), where = "!tbp", size = NULL, center = c("center", 
        "centering", "centerline", "none"), landscape = FALSE, 
    multicol = TRUE, math.row.names = FALSE, math.col.names = FALSE, 
    hyperref = NULL, ...) 
{
    if (length(hyperref)) 
        hyperref <- sprintf("\\hyperref[%s]{", hyperref)
    center <- match.arg(center)
    caption.loc <- match.arg(caption.loc)
    cx <- format.df(object, dcolumn = dcolumn, na.blank = na.blank, 
        numeric.dollar = numeric.dollar, cdot = cdot, math.row.names = math.row.names, 
        math.col.names = math.col.names, double.slash = double.slash, 
        ...)
    if (missing(rowname)) 
        rowname <- dimnames(cx)[[1]]
    nocolheads <- length(colheads) == 1 && is.logical(colheads) && 
        !colheads
    if (!length(colheads)) 
        colheads <- dimnames(cx)[[2]]
    col.just <- attr(cx, "col.just")
    nc <- ncol(cx)
    nr <- nrow(cx)
    if (length(cgroup)) {
        k <- length(cgroup)
        if (!length(n.cgroup)) 
            n.cgroup <- rep(nc/k, k)
        if (sum(n.cgroup) != nc) 
            stop("sum of n.cgroup must equal number of columns")
        if (length(n.cgroup) != length(cgroup)) 
            stop("cgroup and n.cgroup must have same lengths")
    }
    if (!length(rowname)) 
        rgroup <- NULL
    if (!length(n.rgroup) && length(rgroup)) 
        n.rgroup <- rep(nr/length(rgroup), length(rgroup))
    if (length(n.rgroup) && sum(n.rgroup) != nr) 
        stop("sum of n.rgroup must equal number of rows in object")
    if (length(rgroup) && length(n.rgroup) && (length(rgroup) != 
        length(n.rgroup))) 
        stop("lengths of rgroup and n.rgroup must match")
    if (length(rgroup) && rowlabel.just == "l") 
        rowname <- paste("~~", rowname, sep = "")
    sl <- ifelse(double.slash, "\\\\", "\\")
    if (ctable) {
        eol <- paste(sl, "NN\n", sep = "")
        eog <- ""
    }
    else if (longtable && length(n.rgroup)) {
        eol <- paste(sl, "tabularnewline*\n", sep = "")
        eog <- paste(sl, "tabularnewline\n", sep = "")
    }
    else {
        eol <- paste(sl, "tabularnewline\n", sep = "")
        eog <- paste(sl, "tabularnewline\n", sep = "")
    }
    if (booktabs) {
        toprule <- paste(sl, "toprule\n", sep = "")
        midrule <- paste(sl, "midrule\n", sep = "")
        bottomrule <- paste(sl, "bottomrule\n", sep = "")
    }
    else if (ctable) {
        toprule <- paste(sl, "FL\n", sep = "")
        midrule <- paste(sl, "ML\n", sep = "")
        bottomrule <- paste(sl, "LL\n", sep = "")
    }
    else {
        toprule <- if (first.hline.double) 
            paste(sl, "hline", sl, "hline\n", sep = "")
        else paste(sl, "hline\n", sep = "")
        midrule <- bottomrule <- paste(sl, "hline\n", sep = "")
    }
    if (length(cellTexCmds) & !(all(dim(cx) == dim(cellTexCmds)) & 
        length(dim(cx)) == length(dim(cellTexCmds)))) {
        msg <- "The dimensions of cellTexCmds must be:"
        msg1 <- paste(dim(cx), collapse = " x ")
        msg <- paste(msg, msg1)
        msg <- paste(msg, ", but you gave me: ")
        msg1 <- paste(dim(cellTexCmds), collapse = " x ")
        msg <- paste(msg, msg1, sep = "")
        stop(msg)
    }
    if (length(cellTexCmds) | length(rownamesTexCmd)) {
        if (!length(rownamesTexCmd) & length(rowname)) 
            rownamesTexCmd <- rep("", nr)
        if (!length(cellTexCmds)) 
            cellTexCmds <- array("", dim = dim(cx))
        rcellTexCmds <- cbind(rownamesTexCmd, cellTexCmds)
        thisDim <- dim(rcellTexCmds)
        rcellTexCmds <- paste(sl, rcellTexCmds, sep = "")
        rcellTexCmds[rcellTexCmds == sl] <- ""
        dim(rcellTexCmds) <- thisDim
    }
    else rcellTexCmds <- NULL
    if (length(cgroup)) {
        last.col <- cumsum(n.cgroup)
        first.col <- c(1, 1 + last.col[-length(last.col)])
        cgroup.cols <- cbind(first.col, last.col)
        col.subs <- split(seq(length.out = nc), rep.int(seq_along(n.cgroup), 
            times = n.cgroup))
        cxi <- rctci <- list()
        rctcx <- if (length(rcellTexCmds)) 
            rcellTexCmds[, 1]
        for (i in seq(along = col.subs)) {
            cxi[[i]] <- cx[, col.subs[[i]], drop = FALSE]
            if (length(rctcx)) 
                rctcx <- cbind(rctcx, rcellTexCmds[, 1 + col.subs[[i]], 
                  drop = FALSE], if (i < length(col.subs)) 
                  "")
        }
        if (length(rctcx)) 
            rcellTexCmds <- rctcx
        cxx <- cxi[[1]]
        col.justxx <- col.just[col.subs[[1]]]
        collabel.justxx <- collabel.just[col.subs[[1]]]
        colheadsxx <- colheads[col.subs[[1]]]
        extracolheadsxx <- extracolheads[col.subs[[1]]]
        cgroupxx <- cgroup[1]
        n.cgroupxx <- n.cgroup[1]
        for (i in seq(along = col.subs)[-1]) {
            cxx <- cbind(cxx, "", cxi[[i]])
            col.justxx <- c(col.justxx, "c", col.just[col.subs[[i]]])
            collabel.justxx <- c(collabel.justxx, "c", collabel.just[col.subs[[i]]])
            cgroupxx <- c(cgroupxx, "", cgroup[i])
            n.cgroupxx <- c(n.cgroupxx, 1, n.cgroup[i])
            colheadsxx <- c(colheadsxx, "", colheads[col.subs[[i]]])
            if (length(extracolheads)) 
                extracolheadsxx <- c(extracolheadsxx, "", extracolheads[col.subs[[i]]])
        }
        cgroup.colsxx <- cgroup.cols + 0:(nrow(cgroup.cols) - 
            1)
        cx <- cxx
        col.just <- col.justxx
        collabel.just <- collabel.justxx
        n.cgroup <- n.cgroupxx
        cgroup.cols <- cgroup.colsxx[cgroup != "", , drop = FALSE]
        cgroup <- cgroupxx
        colheads <- colheadsxx
        extracolheads <- extracolheadsxx
        nc <- ncol(cx)
    }
    cline <- NULL
    if (length(rowname)) {
        cx <- cbind(rowname, cx)
        col.just <- c(rowlabel.just, col.just)
        if (length(extracolheads)) 
            extracolheads <- c("", extracolheads)
        collabel.just <- c(rowlabel.just, collabel.just)
        if (length(cgroup) == 0L) 
            colheads <- c(rowlabel, colheads)
        else {
            colheads <- c("", colheads)
            cgroup <- c(rowlabel, cgroup)
            rlj <- ifelse(rowlabel.just == "l", "l", "c")
            cgroup.just <- c(rlj, cgroup.just)
            n.cgroup <- c(1, n.cgroup)
            cgroup.cols <- 1 + cgroup.cols
            cline <- paste(sl, "cline{", cgroup.cols[, 1], "-", 
                cgroup.cols[, 2], "}", sep = "", collapse = " ")
        }
        nc <- 1 + nc
    }
    vbar <- ifelse(vbar, "|", "")
    if (!append) 
        cat("", file = file)
    cat("%", deparse(sys.call()), "%\n", file = file, append = file != 
        "", sep = "")
    if (dcolumn) {
        decimal.point <- ifelse(cdot, paste(sl, "cdot", sep = ""), 
            ".")
        cat(sl, "newcolumntype{.}{D{.}{", decimal.point, "}{-1}}\n", 
            sep = "", file = file, append = file != "")
    }
    {
        tabular.cols <- paste(vbar, col.just, sep = "")
        if (!length(n.cgroup)) 
            tabular.cols <- c(tabular.cols, vbar)
        else {
            vv2 <- cumsum(n.cgroup)
            tabular.cols[vv2] <- paste(tabular.cols[vv2], vbar, 
                sep = "")
        }
        tabular.cols <- paste(tabular.cols, collapse = "")
    }
    intop <- function() {
        if (!length(insert.top)) 
            return(NULL)
        paste(if (center == "none") 
            "\n\\vspace{1ex}\n\n", paste("\\textbf{", insert.top, 
            "}", sep = ""), if (center != "center") 
            "\n\\vspace{1ex}\n\n", sep = "")
    }
    if (length(caption) && !ctable) {
        caption <- paste(sl, "caption", if (length(caption.lot)) 
            paste("[", caption.lot, "]", sep = ""), "{", caption, 
            if (!longtable) 
                paste(sl, "label{", label, "}", sep = ""), "}", 
            sep = "")
        table.env <- TRUE
    }
    if (ctable) {
        latex.begin <- latexBuild(if (length(size)) 
            paste("{", sl, size, sep = ""), "{", intop(), "", 
            paste(sl, "ctable[", sep = ""), "", if (length(caption) && 
                caption.loc == "bottom") 
                "botcap,", "", if (length(caption)) 
                paste("caption={", caption, "},", sep = ""), 
            "", if (length(caption.lot)) 
                paste("cap={", caption.lot, "},", sep = ""), 
            "", if (length(caption)) 
                paste("label=", label, ",", sep = ""), "", if (star) 
                "star, ", "", if (!landscape) 
                paste("pos=", where, ",", sep = ""), "", if (landscape) 
                "sideways", "", paste("]{", tabular.cols, "}", 
                sep = ""), "", if (length(insert.bottom)) 
                paste("{", paste(sl, "tnote[]{", sedit(insert.bottom, 
                  "\\\\", " "), "}", sep = "", collapse = ""), 
                  "}", sep = ""), "", if (!length(insert.bottom)) 
                "{}", "", paste("{", toprule, sep = ""), "{")
        latex.end <- attr(latex.begin, "close")
    }
    else if (!longtable) {
        latex.begin <- latexBuild(if (landscape) 
            paste(sl, "begin{landscape}", sep = ""), "landscape", 
            if (table.env) 
                paste(sl, "begin{table}", if (here) 
                  "[H]"
                else paste("[", where, "]", sep = ""), "\n", 
                  sep = ""), "table", if (length(size)) 
                paste("{", sl, size, "\n", sep = ""), "{", if (caption.loc == 
                "top" && length(caption)) 
                paste(caption, "\n"), "", intop(), "", if (center == 
                "center") 
                paste(sl, "begin{center}\n", sep = ""), "center", 
            if (center == "centering") 
                paste("{", sl, "centering\n", sep = ""), "{", 
            if (center == "centerline") 
                paste(sl, "centerline{", sep = ""), "{", hyperref, 
            "{", paste(sl, "begin{tabular}{", tabular.cols, "}\n", 
                toprule, sep = ""), "tabular", insert = list(if (!table.env && 
                length(insert.bottom)) list("tabular", "after", 
                insert.bottom), if (table.env) list("table", 
                "before", insert.bottom), if (caption.loc == 
                "bottom" && length(caption)) list("tabular", 
                "after", caption)))
        latex.end <- attr(latex.begin, "close")
    }
    else {
        latex.begin <- latexBuild(if (!draft.longtable) 
            paste(sl, "let", sl, "LTmulticolumn=", sl, "multicolumn", 
                sep = ""), "", paste(sl, "setlongtables", sep = ""), 
            "", if (landscape) 
                paste(sl, "begin{landscape}", sep = ""), "landscape", 
            if (length(size)) 
                paste("{", sl, size, "\n", sep = ""), "{", intop(), 
            "", paste(sl, "begin{longtable}{", tabular.cols, 
                "}", sep = ""), "longtable", if (caption.loc == 
                "top" && length(caption)) 
                paste(caption, eog), "", toprule, "", insert = list(if (caption.loc == 
                "bottom" && length(caption)) list("longtable", 
                "after", caption)))
        latex.end <- attr(latex.begin, "close")
        if (!length(caption)) 
            latex.end <- paste(latex.end, "\\addtocounter{table}{-1}", 
                sep = "\n")
    }
    cat(latex.begin, file = file, append = file != "")
    cgroupheader <- NULL
    if (length(cgroup)) {
        cvbar <- paste(cgroup.just, vbar, sep = "")
        cvbar[1] <- paste(vbar, cvbar[1], sep = "")
        cvbar[-length(cvbar)] <- paste(cvbar[-length(cvbar)], 
            vbar, sep = "")
        slmc <- paste(sl, "multicolumn{", sep = "")
        if (length(cgroupTexCmd)) 
            labs <- paste(sl, cgroupTexCmd, " ", cgroup, sep = "")
        else labs <- cgroup
        if (multicol) 
            labs <- paste(slmc, n.cgroup, "}{", cvbar, "}{", 
                labs, "}", sep = "")
        cgroupheader <- paste(labs, collapse = "&")
        if (!length(cline)) {
            inr <- as.numeric(length(rowname))
            cline <- paste(sl, "cline{", 1 + inr, "-", nc, "}", 
                sep = "")
        }
        cgroupheader <- paste(cgroupheader, eol, cline, "\n", 
            sep = "")
        cat(cgroupheader, file = file, append = file != "")
    }
    {
        cvbar <- paste(collabel.just, vbar, sep = "")
        cvbar[1] <- paste(vbar, cvbar[1], sep = "")
        if (length(n.cgroup)) {
            vv2 <- cumsum(n.cgroup[-length(n.cgroup)])
            cvbar[vv2] <- paste(cvbar[vv2], vbar, sep = "")
        }
        slmc1 <- paste(sl, "multicolumn{1}{", sep = "")
        labs <- colheads
        if (length(colnamesTexCmd)) 
            labs <- paste(sl, colnamesTexCmd, " ", labs, sep = "")
        if (nocolheads) 
            colheads <- labs <- NULL
        header <- NULL
        if (length(labs)) {
            if (!length(extracolheads)) {
                heads <- get2rowHeads(labs)
                colheads <- heads[[1]]
                if (any(heads[[2]] != "")) 
                  extracolheads <- heads[[2]]
            }
            if (multicol) 
                colheads <- paste(slmc1, cvbar, "}{", colheads, 
                  "}", sep = "")
            header <- if (length(colheads)) 
                paste(colheads, collapse = "&")
            if (length(extracolheads)) {
                extracolheads <- ifelse(extracolheads == "" | 
                  extracolsize == "", extracolheads, paste("{", 
                  sl, extracolsize, " ", extracolheads, "}", 
                  sep = ""))
                if (multicol) 
                  extracolheads <- ifelse(extracolheads == "", 
                    extracolheads, paste(slmc1, cvbar, "}{", 
                      extracolheads, "}", sep = ""))
                else extracolheads <- ifelse(extracolheads == 
                  "", extracolheads, paste(extracolheads, sep = ""))
                header <- if (length(header)) 
                  paste(header, eol, paste(extracolheads, collapse = "&"), 
                    sep = "")
            }
            if (length(header)) 
                cat(header, eog, file = file, sep = "", append = file != 
                  "")
            if (ctable) 
                cat(midrule, file = file, append = file != "")
            else cat(midrule, file = file, append = file != "")
        }
    }
    if (longtable) {
        if (!length(caption)) 
            cat(sl, "endhead\n", midrule, sl, "endfoot\n", sep = "", 
                file = file, append = file != "")
        else {
            cat(sl, "endfirsthead", sep = "", file = file, append = file != 
                "")
            cat(sl, "caption[]{\\em (continued)} ", eol, sep = "", 
                file = file, append = file != "")
            cat(midrule, sep = "", file = file, append = file != 
                "")
            if (length(cgroupheader)) 
                cat(cgroupheader, file = file, append = file != 
                  "")
            if (length(header)) 
                cat(header, file = file, sep = "&", append = file != 
                  "")
            cat(eog, midrule, sl, "endhead", "\n", midrule, sep = "", 
                file = file, append = file != "")
            if (length(insert.bottom)) {
                if (length(insert.bottom.width) == 0) {
                  insert.bottom.width = paste0(sl, "linewidth")
                }
                cat(paste(sl, "multicolumn{", nc, "}{", "p{", 
                  insert.bottom.width, "}}{", insert.bottom, 
                  "}", eol, sep = "", collapse = "\n"), sep = "", 
                  file = file, append = file != "")
            }
            cat(sl, "endfoot\n", sep = "", file = file, append = file != 
                "")
            cat(sl, "label{", label, "}\n", sep = "", file = file, 
                append = file != "")
        }
    }
    {
        if (length(n.rgroup)) {
            rg.end <- cumsum(n.rgroup)
            rg.start <- rg.end - n.rgroup + 1
            if (!length(rgroup)) {
                rgroup <- rep("", length(n.rgroup))
            }
            else {
                if (length(rgroupTexCmd)) {
                  rgroup <- paste("{", sl, rgroupTexCmd, " ", 
                    rgroup, "}", sep = "")
                }
                else rgroup <- paste("{", rgroup, "}", sep = "")
            }
            seq.rgroup <- seq(along = n.rgroup)
        }
        else {
            seq.rgroup <- 1
            rg.end <- nr
            rg.start <- 1
        }
        linecnt <- 0
        for (j in seq.rgroup) {
            if (length(n.rgroup)) {
                if (longtable && linecnt > 0 && (linecnt + n.rgroup[j] + 
                  (n.rgroup[j] > 1)) > lines.page) {
                  cat(sl, "newpage\n", sep = "", file = file, 
                    append = file != "")
                  linecnt <- 0
                }
                cat(rgroup[j], rep("", nc - 1), sep = "&", file = file, 
                  append = file != "")
                cat(eol, sep = "", file = file, append = file != 
                  "")
                linecnt <- linecnt + 1
            }
            for (i in rg.start[j]:rg.end[j]) {
                if (!length(n.rgroup)) {
                  if (longtable && linecnt > 0 && (linecnt + 
                    1 > lines.page)) {
                    cat(sl, "newpage\n", sep = "", file = file, 
                      append = file != "")
                    linecnt <- 0
                  }
                }
                if (length(rcellTexCmds)) {
                  num.cols <- ncol(cx)
                  for (colNum in 1:num.cols) {
                    cat(rcellTexCmds[i, colNum], " ", cx[i, colNum], 
                      file = file, append = file != "")
                    if (colNum < num.cols) 
                      cat("&", file = file, append = file != 
                        "")
                  }
                }
                else {
                  cat(cx[i, ], file = file, sep = "&", append = file != 
                    "")
                }
                cat(if (i == rg.end[j] || (!ctable && !length(n.rgroup))) 
                  eog
                else if (i < rg.end[j]) 
                  eol, sep = "", file = file, append = file != 
                  "")
                linecnt <- linecnt + 1
            }
            if (length(n.rgroup) > j) 
                cat(midrule, sep = "", file = file, append = file != 
                  "")
            else cat(bottomrule, sep = "", file = file, append = file != 
                "")
        }
    }
    cat(latex.end, file = file, sep = "\n", append = file != 
        "")
    sty <- c("longtable"[longtable], "here"[here], "dcolumn"[dcolumn], 
        "ctable"[ctable], "booktabs"[booktabs], if (landscape && 
            !ctable) "lscape")
    structure(list(file = file, style = sty), class = "latex")
}

