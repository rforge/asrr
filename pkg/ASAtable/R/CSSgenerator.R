CSSgenerator <-  function(
                          fontsize,
                          indent,
                          tablewidth,
                          lwidth,
                          firstline,
                          ...)
  ##the meaning of the argument,see HTMLtable()
{
  css.def <- "
<STYLE TYPE='text/css'>
   TABLE {
   font-size: fontsizept;
   }
   TD.BODYCELL{
     text-align:center;
    }
   TD.BODYCELLBOTTOM{
     text-align:center;
     border-top:none ;
     border-left:none;
     border-bottom:solid windowtext lwidth;
     border-right:'none';
    }
   TD.STUBCOLMAIN{
     text-align:left;
     text-indent:ptindentpt;
    }
   TD.STUBCOLRGROUP{
     text-align:left;
    }
   TD.CGROUP{
      border-top:none;
      border-left:none;
      border-bottom:solid windowtext lwidth;
      border-right:none;
      text-align:center;
    }
  TD.SEPCOL{
     border-top:none
     border-left:none;
     border-bottom:none;
     border-right:none;
     text-align:center;
    }
   TD.SEPCOLLEFT{
      border-top:none;
      border-left:none;
      border-bottom:none;
      border-right:none;
      text-align:left;
    }
  TD.FOOTNOTE{
      border-top:solid windowtext lwidth;
      text-align:left;
    }
  </STYLE>"
    css.def <- gsub("fontsize",fontsize,css.def,fixed=TRUE)
    css.def <- gsub("ptindent",indent*fontsize,css.def,fixed=TRUE)
    css.def <- gsub("tablewidth",tablewidth,css.def,fixed=TRUE)
    css.def <- gsub("lwidth",lwidth,css.def)
    css.def <- gsub("firstline",firstline,css.def)
    css.def
  }
