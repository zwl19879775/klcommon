#!/bin/sh
# Generate html file name based on rst file.
FILE_RST=$1
FILE_HTML=`echo $FILE_RST | sed 's/\.rst$//g'`".html"
eval rst2html.py $FILE_RST $FILE_HTML
 
# Remove redundant meta, css, tag code.
CUR_LN=1
TITLE_LN=`sed -n '/<title>/'= $FILE_HTML`
sed -i "$CUR_LN,`expr $TITLE_LN - 1`"d $FILE_HTML
 
CUR_LN=`expr $CUR_LN + 1`
HEAD_LN=`sed -n '/<h1 class="title"/'= $FILE_HTML`
sed -i "$CUR_LN, $HEAD_LN"d $FILE_HTML
 
LAST_LN=`sed -n "$"= $FILE_HTML`
sed -i "`expr $LAST_LN - 2`,$LAST_LN"d $FILE_HTML
 
sed -i "/<\/\?div/"d $FILE_HTML
 
# Update individual tags.
sed -i 's/ class="[^"]*"//g' $FILE_HTML
sed -i 's/ class="[^"]*"//g' $FILE_HTML
sed -i 's/\(href="http.*"\)/\1 target="_blank"/g' $FILE_HTML
sed -i 's/h4>/h5>/g' $FILE_HTML
sed -i 's/h3>/h5>/g' $FILE_HTML
sed -i 's/h2>/h4>/g' $FILE_HTML
sed -i 's/h1>/h3>/g' $FILE_HTML
sed -i 's/<pre/<pre class="brush:bash"/g' $FILE_HTML

# Make cl-writer happy
sed -i '1i\<body>' $FILE_HTML
echo '</body>' >> $FILE_HTML

