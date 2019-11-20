NAME = atom

SOURCES = atom.ml main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

CFLAGS = -w @A

all: depend $(NAME)

$(NAME): opt byt
	ln -s $(NAME).byt $(NAME)

opt: $(NAME).opt
byt: $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(OBJS)
				$(CAMLC) -o $(NAME).byt $(OBJS)

$(NAME).opt: $(OPTOBJS)
				$(CAMLOPT) -o $(NAME).opt $(OPTOBJS)

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(CFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(CFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(CFLAGS) -c $<

clean:
	rm -f .depend
	rm -f *.cm[iox] *.opt *.o
	rm -f $(NAME).o

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME).opt
	rm -f $(NAME).byt

depend: .depend

.depend:
	$(CAMLDEP) $(SOURCES) > .depend

re: fclean all

-include .depend
