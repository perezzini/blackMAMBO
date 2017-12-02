# Unix makefile for tigermain example

# Cada vez que se agrega una dependencia, hacer "make depend"

HOME=/usr/local/bin
MOSMLHOME=${HOME}
MOSMLTOOLS=camlrunm /usr/local/share/mosml/tools
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

GCC=gcc
CFLAGS= -g
MOSMLC=${MOSMLHOME}/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
	tigernlin.uo tigerpp.uo tigerescap.uo tigertab.uo tigerseman.uo tigertemp.uo tigertree.uo \
	tigerframe.uo tigertrans.uo tigerit.uo tigerpila.uo tigerinterp.uo tigertopsort.uo tigermuestratipos.uo tigercanon.uo \
	tigerassem.uo tigercodegen.uo tigerliveness.uo utils.uo tigerregalloc.uo

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger $(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

### Agregar modulos acá también
depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml tigertopsort.sml tigermuestratipos.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigerliveness.ui: tigerassem.uo tigertemp.ui 
tigerassem.uo: tigertemp.ui 
tigerpila.uo: tigerpila.ui 
tigertree.uo: tigertab.ui tigertemp.ui 
tigertemp.uo: tigertemp.ui 
tigertopsort.uo: tigertopsort.ui tigertab.ui tigertips.uo tigerabs.uo \
    tigermuestratipos.ui 
tigermuestratipos.ui: tigertips.uo 
tigerescap.ui: tigerabs.uo 
tigerinterp.uo: tigertree.uo tigertab.ui tigerframe.ui tigerit.uo \
    tigertemp.ui 
tigertab.uo: tigertab.ui 
tigermain.uo: tigerseman.ui tigercodegen.ui tigertree.uo tigerescap.ui \
    tigergrm.ui tigerframe.ui tigerit.uo tigerregalloc.ui tigercanon.ui \
    tigerassem.uo tigerinterp.uo tigertemp.ui tigerlex.uo tigerliveness.ui \
    tigertrans.ui tigerpp.uo 
tigerseman.uo: tigerseman.ui tigersres.uo tigertab.ui tigerpila.ui \
    tigertopsort.ui tigertemp.ui tigerabs.uo tigertrans.ui 
tigertopsort.ui: tigertab.ui tigertips.uo tigerabs.uo 
tigerseman.ui: tigerabs.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigercanon.uo: tigercanon.ui tigertree.uo tigertab.ui tigertemp.ui 
tigertrans.uo: tigertrans.ui tigertree.uo tigerpila.ui tigerframe.ui \
    tigerit.uo tigertemp.ui tigerabs.uo 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo 
tigermuestratipos.uo: tigermuestratipos.ui tigertips.uo 
utils.uo: utils.ui 
tigerliveness.uo: tigerliveness.ui utils.ui tigerassem.uo tigertemp.ui 
tigerframe.uo: tigerframe.ui utils.ui tigertree.uo tigerit.uo tigerassem.uo \
    tigertemp.ui 
tigerregalloc.ui: tigerframe.ui tigerassem.uo tigertemp.ui 
tigerit.uo: tigertree.uo tigertab.ui 
tigergrm.ui: tigerabs.uo 
tigercodegen.uo: tigercodegen.ui utils.ui tigertree.uo tigerframe.ui \
    tigerassem.uo tigertemp.ui 
tigerregalloc.uo: tigerregalloc.ui utils.ui tigerframe.ui tigerassem.uo \
    tigertemp.ui tigerliveness.ui 
tigercodegen.ui: tigertree.uo tigerframe.ui tigerassem.uo 
tigersres.uo: tigertab.ui tigertips.uo tigertemp.ui tigerabs.uo \
    tigertrans.ui 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigertrans.ui: tigertree.uo tigerframe.ui tigertemp.ui tigerabs.uo 
tigerpp.uo: tigerabs.uo 
tigercanon.ui: tigertree.uo tigertemp.ui 
tigerframe.ui: tigertree.uo tigerassem.uo tigertemp.ui 
