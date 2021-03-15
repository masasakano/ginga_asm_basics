#ALL     = 

docmd   = DESCRIPTION.ja.md
outhtml = doc/DESCRIPTION.ja.html

#.SUFFIXES:      .so .o .c .f
#.o.so:
#       ${LD} ${LFLAGS} -o $@ $< ${LINK_LIB}

all: ${ALL}

.PHONY: clean test doc

doc::
	( [ ${outhtml} -nt ${docmd} ] && echo 'Nothing to be done for "doc"' ) || ( pandoc --css=http://www.wisebabel.com/sites/default/files/styles/pandoc_wb.css -s -t html5 --section-divs -N --toc -o ${outhtml} ${docmd} && echo "${outhtml} updated." )

clean::
	$(RM) doc/DESCRIPTION.ja.html 
# $(RM) ${ALL}

