TEMPLATEDIR=templates
SRCDIR=src
OBJDIR=ebin
DEPDIR=.deps
ERLC=erlc
ERLYDTL=./bin/erlydtl_compile
ERLCFLAGS=-I include #+debug_info

SOURCES=$(wildcard $(SRCDIR)/*.erl)
OBJECTS=$(addprefix $(OBJDIR)/, $(notdir $(SOURCES:.erl=.beam)))

APPSRC=$(wildcard $(SRCDIR)/*.app.src)
APPS=$(addprefix $(OBJDIR)/, $(notdir $(APPSRC:.app.src=.app)))

TEMPLATES=$(wildcard $(TEMPLATEDIR)/*.dtl)
TEMPLATE_OBJECTS=$(addprefix $(OBJDIR)/, $(notdir $(TEMPLATES:.dtl=.beam)))

DEPS = $(addprefix $(DEPDIR)/, $(notdir $(SOURCES:.erl=.Pbeam))) $(addprefix $(DEPDIR)/, $(notdir $(TEMPLATES:.dtl=.Pbeam)))

#$(warning TEMPLATES = $(TEMPLATES))
#$(warning TEMPLATE_OBJECTS = $(TEMPLATE_OBJECTS))

#$(warning SOURCES = $(SOURCES))
#$(warning DEPS = $(DEPS))

# clear out all suffixes
.SUFFIXES:
# list only those we use
.SUFFIXES: .beam .erl .app

all: $(OBJECTS) $(TEMPLATE_OBJECTS) $(APPS)

-include $(DEPS)


$(OBJDIR)/%.beam: $(TEMPLATEDIR)/%.dtl | $(OBJDIR)
	$(ERLYDTL) $< $(OBJDIR)

# define a suffix rule for .erl -> .beam
$(OBJDIR)/%.beam: $(SRCDIR)/%.erl | $(OBJDIR)
	$(ERLC) $(ERLCFLAGS) -o $(OBJDIR) $<

$(OBJDIR)/%.app: $(SRCDIR)/%.app.src | $(OBJDIR)
	cp $< $@

#see this: http://www.gnu.org/software/make/manual/html_node/Pattern-Match.html
$(DEPDIR)/%.Pbeam: $(SRCDIR)/%.erl | $(DEPDIR)
	$(ERLC) -MF $@ -MT $(OBJDIR)/$*.beam $(ERLCFLAGS) $<

$(DEPDIR)/%.Pbeam: $(TEMPLATEDIR)/%.dtl | $(DEPDIR)
	./bin/template_deps.py $< > $@

#the | pipe operator, defining an order only prerequisite. Meaning
#that the $(OBJDIR) target should be existent (instead of more recent)
#in order to build the current target
$(OBJECTS): | $(OBJDIR)

$(OBJDIR):
	test -d $(OBJDIR) || mkdir $(OBJDIR)

$(DEPDIR):
	test -d $(DEPDIR) || mkdir $(DEPDIR)

TAGS: $(SOURCES)
	etags $(SOURCES)

clean:
	-$(RM) -rf $(OBJECTS) $(TEMPLATE_OBJECTS) $(DEPS)

distclean: clean
	-$(RM) -rf $(OBJDIR) $(DEPDIR)

.PHONY: clean distclean all
