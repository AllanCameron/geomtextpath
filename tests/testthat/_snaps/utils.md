# No changes occurred in autodocumentation of aesthetics

    Code
      txt
    Output
       [1] "@section Aesthetics:"                                                                           
       [2] "\\code{geom_textpath()} understands the following aesthetics (required aesthetics are in bold):"
       [3] "\\itemize{"                                                                                     
       [4] "  \\item \\strong{\\code{x}}"                                                                   
       [5] "  \\item \\strong{\\code{y}}"                                                                   
       [6] "  \\item \\strong{\\code{label}}"                                                               
       [7] "  \\item \\code{alpha}"                                                                         
       [8] "  \\item \\code{angle}"                                                                         
       [9] "  \\item \\code{colour}"                                                                        
      [10] "  \\item \\code{family}"                                                                        
      [11] "  \\item \\code{fontface}"                                                                      
      [12] "  \\item \\code{group}"                                                                         
      [13] "  \\item \\code{hjust}"                                                                         
      [14] "  \\item \\code{linecolour}"                                                                    
      [15] "  \\item \\code{lineheight}"                                                                    
      [16] "  \\item \\code{linetype}"                                                                      
      [17] "  \\item \\code{linewidth}"                                                                     
      [18] "  \\item \\code{size}"                                                                          
      [19] "  \\item \\code{spacing}"                                                                       
      [20] "  \\item \\code{textcolour}"                                                                    
      [21] "  \\item \\code{vjust}"                                                                         
      [22] "}"                                                                                              
      [23] "In addition to aforementioned aesthetics, \\code{geom_labelpath()} also understands:"           
      [24] "\\itemize{"                                                                                     
      [25] "  \\item \\code{boxcolour}"                                                                     
      [26] "  \\item \\code{boxlinetype}"                                                                   
      [27] "  \\item \\code{boxlinewidth}"                                                                  
      [28] "  \\item \\code{fill}"                                                                          
      [29] "}"                                                                                              
      [30] "The \\code{spacing} aesthetic allows fine control of spacing"                                   
      [31] " of text, which is called 'tracking' in typography."                                            
      [32] "The default is 0 and units are measured in 1/1000 em."                                          
      [33] "Numbers greater than zero increase the spacing,"                                                
      [34] "whereas negative numbers decrease the spacing."                                                 
      [35] "\n\nLearn more about setting these aesthetics "                                                 
      [36] "in \\code{vignette(\"ggplot2-specs\")}."                                                        

# rd_dots works as before

    Code
      txt
    Output
      [1] "@param ... Other arguments passed on to \\code{\\link[ggplot2:layer]{layer}}. These are often aesthetics, used to set an aesthetic to a fixed value, like \\code{colour = \"red\"} or \\code{size = 3}. These can also be the following text-path parameters:\\describe{\\item{\\code{text_only}}{A \\code{logical(1)} indicating whether the path part should be plotted along with the text (\\code{FALSE}, the default). If \\code{TRUE}, any parameters or aesthetics relating to the drawing of the path will be ignored.}\\item{\\code{gap}}{A \\code{logical(1)} which if \\code{TRUE}, breaks the path into two sections with a gap on either side of the label. If \\code{FALSE}, the path is plotted as a whole. Alternatively, if \\code{NA}, the path will be broken if the string has a \\code{vjust} between 0 and 1, and not otherwise. The default for the label variant is \\code{FALSE} and for the text variant is \\code{NA}.}\\item{\\code{upright}}{A \\code{logical(1)} which if \\code{TRUE} (default), inverts any text where the majority of letters would upside down along the path, to improve legibility. If \\code{FALSE}, the path decides the orientation of text.}\\item{\\code{halign}}{A \\code{character(1)} describing how multi-line text should be justified. Can either be \\code{\"center\"} (default), \\code{\"left\"} or \\code{\"right\"}.}\\item{\\code{offset}}{A \\code{\\link[grid:unit]{unit}} object of length 1 to determine the offset of the text from the path. If this is \\code{NULL} (default), the \\code{vjust} parameter decides the offset. If not \\code{NULL}, the \\code{offset} argument overrules the \\code{vjust} setting.}\\item{\\code{parse}}{A \\code{logical(1)} which if \\code{TRUE}, will coerce the labels into expressions, allowing for plotmath syntax to be used.}\\item{\\code{padding}}{A \\code{\\link[grid:unit]{unit}} object of length 1 to determine the padding between the text and the path when the \\code{gap} parameter trims the path.}\\item{\\code{text_smoothing}}{a \\code{numeric(1)} value between 0 and 100 that smooths the text without affecting the line portion of the geom. The default value of \\code{0} means no smoothing is applied.}\\item{\\code{rich}}{A \\code{logical(1)} whether to interpret the text as html/markdown formatted rich text. Default: \\code{FALSE}. See also the rich text section of the details in \\code{\\link[=geom_textpath]{geom_textpath()}}.}\\item{\\code{remove_long}}{if TRUE, labels that are longer than their associated path will be removed.}}"

