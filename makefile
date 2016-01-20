#####################################################################################################################
##                                                                                                                 ##
## makefile -- Used to control the build for the "pascal-cc" compiler.                                             ##
##                                                                                                                 ##
##    Date     Tracker  Version  Pgmr  Modification                                                                ##
## ----/--/--  -------  -------  ----  --------------------------------------------------------------------------- ##
## 2016/02/07  Initial   v0.0    ADCL  Initial version -- leveraged from the pascal compiler, but made C++         ##
##                                                                                                                 ##
#####################################################################################################################

.SILENT:


VER=0.0


#
# -- Some strings needed to complete the dependency analysis
#    -------------------------------------------------------
BLD-SRC=util/build-num.txt
BLD-CC=$(subst .txt,.cc,$(subst util,src,$(BLD-SRC)))
BLD-OBJ=$(subst .txt,.o,$(subst util,obj,$(BLD-SRC)))

YY-SRC=$(sort $(wildcard src/*.yy))
YY-CC=$(subst .yy,.cc,$(YY-SRC))
YY-HH=$(subst .yy,.hh,$(subst src/,include/,$(YY-SRC)))

AST-SRC=$(sort $(wildcard src/*.ast))
AST-HH=$(subst src/,include/,$(subst .ast,.hh,$(AST-SRC)))

HH-SRC=$(sort $(wildcard include/*.hh) $(wildcard include/*.h) $(YY-HH) $(AST-HH))

LL-SRC=$(sort $(wildcard src/*.ll))
LL-CC=$(subst .ll,.cc,$(LL-SRC))

CC-SRC=$(sort $(wildcard src/*.cc) $(YY-CC) $(LL-CC) $(BLD-CC))

ALL-SRC=$(sort $(YY-SRC) $(AST-SRC) $(LL-SRC) $(filter-out $(BLD-CC),$(wildcard src/*.cc)) $(wildcard include/*.hh) \
		$(wildcard include/*.h))

OBJ=$(subst src/,obj/,$(subst .cc,.o,$(CC-SRC)))
DEP=$(subst .o,.d,$(filter-out $(BLD-OBJ),$(OBJ)))

RPT=util/pas-cc.rpt

TGT=pas-cc

#
# -- These are the build commands
#    ----------------------------
CFLAGS=-Wno-write-strings -Wno-unused-function
LIBS=-lstdc++

LEX=flex --yylineno
YAC=bison --defines=$(YY-HH) --debug --report-file=$(RPT) --report=all
AST=ast-cc
GCC=gcc -c -Wall -Wextra -Werror -g -Iinclude $(CFLAGS)
LD=gcc

#
# -- Finally, these are the recipes needed to build the actual system
#    ----------------------------------------------------------------
all: $(TGT) makefile
#	echo "Build # " $(BLD-SRC)
#	echo "Headers " $(HH-SRC)
#	echo "Scanner " $(LL-SRC)
#	echo "Grammar " $(YY-SRC)
#	echo "C++ Src " $(CC-SRC)
#	echo "AST Src " $(AST-SRC)
#	echo "AST Hdr " $(AST-HH)
#	echo "Objects " $(OBJ)
#	echo "Depends " $(DEP)
#	echo "All Src " $(ALL-SRC)

ifneq ($(MAKECMDGOALS),clean)
-include $(DEP)
endif

$(TGT): $(OBJ) makefile
	echo "LINK  " $@
	$(LD) -o $@ $(OBJ) $(LIBS)

$(BLD-SRC): $(ALL-SRC) makefile
	echo "BUILD  Incrementing Build Number..."
	mkdir -p $(dir $@)
	if ! test -f $@; then echo 0 > $@; fi
	echo $$(($$(cat $@) + 1)) > $@

$(BLD-CC): $(BLD-SRC) makefile
	echo "BUILD# The build number is:" $$(($$(cat $<)))
	echo "unsigned long BLD_NUM = " $$(cat $<) ";" > $@
	echo "char *BLD_DATE = \"$$(date +'%m-%d-%Y')\";" \ >> $@
	echo "char *VERSION = \"$(VER)\";" \ >> $@

obj/%.o: src/%.cc
	echo "CC    " $<
	mkdir -p obj
	$(GCC) -o $@ $<

src/%.cc: src/%.yy
	echo "YACC  " $<
	$(YAC) -o $@ $<

src/%.cc: src/%.ll
	echo "LEX   " $<
	$(LEX) -o $@ $<

include/%.hh: src/%.ast
	echo "AST   " $<
	$(AST) $< > /dev/null
	mv ast-nodes.h $@

obj/%.d: src/%.cc $(AST-HH)
	echo "DEPEND" $(notdir $<)
	mkdir -p obj
	$(GCC) -MM -MG $< | sed -e 's@^\(.*\)\.o:@obj/\1.d obj/\1.o:@' \
			| sed -e 's/ grammar.hh/ include\/grammar.hh/' | sed -e 's/ pascal.hh/ include\/pascal.hh/' > $@

$(AST-HH): $(AST-SRC)
$(YY-HH) $(YY-CC): $(YY-SRC)
$(LL-CC): $(LL-SRC)

clean:
	rm -f $(TGT)
	rm -f $(YY-CC)
	rm -f $(YY-HH)
	rm -f $(LL-CC)
	rm -f $(AST-HH)
	rm -f $(BLD-CC)
	rm -f ast-nodes.h
	rm -fR obj
	rm -f *~
	echo "All cleaned up!"
