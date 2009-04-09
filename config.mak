FIRM_HOME   = $(HOME)/develop/firm
FIRM_BUILD  = $(FIRM_HOME)/build/i686-apple-darwin9.5.0/debug/
FIRM_CFLAGS = -I$(FIRM_HOME)/libfirm/include -I$(FIRM_HOME)/obstack -I$(FIRM_HOME)/libcore -I$(FIRM_HOME)/libcore/libcore -I$(FIRM_HOME)
FIRM_LIBS   = -L$(FIRM_BUILD) -L$(FIRM_BUILD)/libfirm -lfirm -llpp -lcore -lm -lz -ldl
LIBFIRM_FILE = $(FIRM_BUILD)/libfirm/libfirm.a

