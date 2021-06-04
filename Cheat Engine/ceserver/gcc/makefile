COMPILER  = gcc
CFLAGS    =  -g -MMD -MP -Wall -Wextra -Winit-self -Wno-missing-field-initializers -Doff64_t=__off64_t 
ifeq "$(shell getconf LONG_BIT)" "64"
  LDFLAGS = -lz -ldl -pthread
else
  LDFLAGS = -lz -ldl -pthread
endif
LIBS      =
INCLUDE   = -I../
TARGET    = ./ceserver
SRCDIR    = ../
ifeq "$(strip $(SRCDIR))" ""
  SRCDIR  = .
endif
SOURCES   = $(wildcard $(SRCDIR)/*.c)
OBJDIR    = ./obj
ifeq "$(strip $(OBJDIR))" ""
  OBJDIR  = .
endif
OBJECTS   = $(addprefix $(OBJDIR)/, $(notdir $(SOURCES:.c=.o)))
DEPENDS   = $(OBJECTS:.o=.d)

$(TARGET): $(OBJECTS) $(LIBS)
	$(COMPILER) -o $@ $^ $(LDFLAGS)

$(OBJDIR)/%.o: $(SRCDIR)/%.c
	-mkdir -p $(OBJDIR)
	$(COMPILER) $(CFLAGS) $(INCLUDE) -o $@ -c $<

all: clean $(TARGET)

clean:
	-rm -f $(OBJECTS) $(DEPENDS) $(TARGET)

-include $(DEPENDS)
