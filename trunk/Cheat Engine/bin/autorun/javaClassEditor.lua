--Java class editor
--[[
This will show an userinterface for editing java classes and will return a list of "patch" commands
that can be used with the runtime java class edit commands

e.g:
InsertBytecode(spot, command)
ModifyBytecode(spot, command)
DeleteBytecode(spot)  (could be ModifyBytecode(spot,"nop") )


The user should not have to know about exceptions and how their positions change with each insert/delete

--]]

