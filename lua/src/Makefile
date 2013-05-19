all: lualib lua luac bin2c

lualib:
	@$(MAKE) --no-print-directory -f tecmake_compact.mak

lua:
	@$(MAKE) --no-print-directory -f tecmake_compact.mak MF=lua

luac:
	@$(MAKE) --no-print-directory -f tecmake_compact.mak MF=luac
  
bin2c:
	@$(MAKE) --no-print-directory -f tecmake_compact.mak MF=bin2c
  