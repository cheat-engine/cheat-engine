

--This lua script gets loaded when Cheat Engine loads
--You can use this to define some often used functions and libraries you'd like to use

require("defines")
require("class");

--
--List of CE specific functions:
--note: addresses can be strings, they will get interpreted by ce's symbolhandler
--readBytes(address,bytecount, ReturnAsTable ) : returns the bytes at the given address. If ReturnAsTable is true it will return a table instead of multiple bytes
--  Reads the bytes at the given address and returns a table containing the read out bytes

--writeBytes(address, x,x,x,x,...) : Write the given bytes to the given address from a table
--writeBytes(address, table, count) : Write the given bytes to the given address from a table 


--readBytesLocal(address,bytecount, ReturnAsTable) : See readBytes but then it's for Cheat engine's memory
--writeBytesLocal(address, x,x,x,x,...) : See writeBytes but then it's for Cheat Engine's memory
--writeBytesLocal(address, table, , count) : See writeBytes but then it's for Cheat Engine's memory
--readInteger(address) : Reads a integer from the specified address
--readFloat(address) : Reads a single precision floating point value from the specified address
--readDouble(address) : Reads a double precision floating point value from the specified address
--readString(address, maxlength) : maxlength is just so you won't freeze for too long, set to 6000 if you don't care too much
--writeInteger(address,value) : Writes an integer to the specified address. Returns true on success
--writeFloat(address,value) : Writes a single precision floating point to the specified address. Returns true on success
--writeDouble(address,value) : Writes a double precision floating point to the specified address. Returns true on success
--writeString(address,string) : Write a string to the specified address. Returns true on success
--getAddress(string): returns the address of a symbol. Can be a modulename or an export
--reinitializeSymbolhandler(): reinitializes the symbolhandler. E.g when new modules have been loaded

--generateAPIHookScript(address, addresstojumpto, addresstogetnewcalladdress OPT) : Generates an auto assembler script which will hook the given address when executed
--autoAssemble(text) : runs the auto assembler with the given text. Returns true on success
--showMessage(text) : shows a messagebox with the given text
--messageDialog(text, type, buttons...) : pops up a messagebox with a specific icon/sound with the specified buttons (mbok, mbyes, ....)
--sleep(milliseconds): pauses for the number of specified milliseconds (1000= 1 sec...)


--getOpenedProcessID() : Returns the currently opened process. If none is open, returns 0
--getProcessIDFromProcessName(name) : returns a processid
--openProcess(processid) : causes cheat engine to open the given processid
--openProcess(processname): causes cheat engine to find and open the given process
--pause : pauses the current opened process
--unpause: resumes the current opened process


--getPixel(x,y) : returns the rgb value of the pixel at the specific screen coordinate
--getMousePos: returns the x,y coordinates of the mouse
--setMousePos(x,y): sets the mouse position

--isKeyPressed(key) : returns true if the specified key is currently pressed
--keyDown(key) : causes the key to go into down state
--keyUp(key) :causes the key to go up
--doKeyPress(key) : simmulates a key press


--speedhack_setSpeed(speed)
--injectDll(filename): Injects a dll, and returns true on success



--Cheat table functions:

--Table related routines:
--If a cheat entry is about to get enabled or disabled it will check if a lua function named "_memrec_description_activating" or "_memrec_description_deactivating" is available, and if so call it.
--If a cheat entry is enabled or disabled it will check if a lua function named "_memrec_description_activated" or "_memrec_description_deactivated" is available, and if so call it.
--It passes the tableEntry pointer as parameter
--Example:
--If the cheat entry table with description "xxx" gets enabled it will call "_memrec_xxx_activating(te)" before it is activated and "_memrec_xxx_activated(te)" after it has been activated (check with isActive to see if it actually did get activated in case of errors in a script or unreadable memory)
--If the cheat entry table with description "xxx" gets disabled it will call "_memrec_xxx_deactivating(te)" before it is activated and "_memrec_xxx_deactivated(te)" after it has been deactivated

-----debugging------

--debug variables
--EFLAGS
--32-bit: EAX, EBX, ECX, EDX, EDI, ESP, EBP, ESP, EIP 
--64-bit: RAX, EBX, RBX, RDX, RDI, RSP, RBP, RSP, RIP, R8, R9, R10, R11, R12, R13, R14, R15 : The value of the register

--Debug related routines:
--function debugger_onBreakpoint():
--When a breaking breakpoint hits (that includes single stepping) and the lua function debugger_onBreakpoint() is defined it will be called and the global variables EAX, EBX, .... will be filled in
--Return 0 if you want the userinterface to be updated and anything else if not (e.g: You continued from the breakpoint in your script)



--createProcess(path, parameters OPTIONAL, debug OPTIONAL, breakonentrypoint OPTIONAL) : Creates a process. If debug is true it will be created using the windows debugger and if breakonentry is true it will cause a breakpoint to occur on entrypoint

--debugProcess(interface OPT): starts the debugger for the currently opened process (won't ask the user) Optional interface: 0=default, 1=windows debug, 2=VEHDebug, 3=Kerneldebug
--debug_setBreakpoint(address, size OPTIONAL, trigger OPTIONAL) : sets a breakpoint of a specific size at the given address. if trigger is bptExecute then size is ignored. If trigger is ignored then it will be of type bptExecute, which obviously also ignores the size then as well
--debug_removeBreakpoint(address) : if the given address is a part of a breakpoint it will be removed
--debug_continueFromBreakpoint(continueMethod) : if the debugger is currently waiting to continue you can continue with this. Valid parameters are :co_run (just continue), co_stepinto(when on top of a call, follow it), co_stepover (when on top of a call run till after the call)



--Changing registers:
--Different from ce 6.0
--When the debugger is waiting to continue you can change the register variables. When you continue those register values will be set in the thread's context



------gui------
--closeCE() : just closes ce
--hideAllCEWindows() : makes all normal ce windows invisible (e.g trainer table)
--unhideMainCEwindow() : shows the main cheat engine window

------class-objects-----
--The following objects are descendent from the object named "control", therefore the returned pointer when they are created can also be used for functions that start with control_

--createForm(visible OPT): creates a form (window) and returns the pointer for it. Visible is default true but can be changed


--The following create routines take an owner as parameter. Owner can be a Form, Panel or Groupbox
--The x,y position will be based on the client array of the owner
--createPanel(owner)
--createGroupBox(owner)
--createButton(owner)
--createImage(owner)
--createLabel(owner)
--createEdit(owner)
--createMemo(owner)
--createTimer(owner)


--getAutoAttachList(): returns the AutoAttach StringList object. It can be controlled with the stringlist_ routines (it's not recommended to destroy this list object)


--AOBScan(x,x,x,x,...):
--scans the currently opened process and returns a stringlist containing all the results. don't forget to free this list when done
--Bytevalue of higher than 255 or anything not an integer will be seen as a wildcard
--AOBScan(aobstring): see above but here you just input one string





--function onOpenProcess(processid) : When this function is defined it will be called each time a process has been opened (note that a process can be opened multiple times in a row, e.g when attaching the debugger it might first open it and then attach the debugger which opens it again...)

--function debugger_onModuleLoad(modulename, baseaddress) : 
--this routine is called when a module is loaded. Only works for the windows debugger
--return 1 if you want to cause the debugger to break



------------------------------CE 6.1------------------------------
--Regarding eventhandlers. You can initialize them using both a string of a functionname or the function itself.
--If initialized using a function itself it won't be able to get saved in the table


--undefined property functions
--getPropertyList(class) : Returns a stringlist object containing all the published properties of the specified class (free the list when done) (Note, not all classed with properties have 'published' properties. E.g: stringlist)
--setProperty(class, propertyname, propertyvalue) : Sets the value of a published property of a class (Won't work for method properties)
--getProperty(class, propertyname) : Gets the value of a published property of a class (Won't work for method properties)

--getMainForm() : Returns the main form class object which can be accessed using the Form_ class methods and the methods of the classes it inherits from
--getAddressList() : Returns the cheat table addresslist object
--getFreezeTimer()
--getUpdateTimer()

--GenericHotkey Class : (Inheritance:  Object)
--createHotkey(function, key, ...) : returns an initialized GenericHotkey class object. Maximum of 5 keys
--generichotkey_setKeys(hotkey, key, ....)
--generichotkey_onHotkey(hotkey, function)

--beep() : Plays the fabulous beep/ping sound!


--class helper functions
--inheritsFromObject(class): Returns true if given any class
--inheritsFromComponent(class): Returns true if the given object inherits from the Component class
--inheritsFromControl(class): Returns true if the given object inherits from the Control class
--inheritsFromWinControl(class): Returns true if the given object inherits from the WinControl class


----Class definitions----
--Object class: (Inheritance: )
--object_getClassName(object): Returns the classname of the given object
--object_destroy(object) : Destroys the object (basically everything inherits from this class)


--Component Class: (Inheritance: Object)
--component_getComponentCount(Component) : Returns the number of components attached to his component
--component_getComponent(Component, index) : Returns the specific component
--component_getName(Component) : Return the name
--component_setName(Component, newname) : Changes the name
--component_getTag(Component) : Sets an integer value. You can use this for ID's
--component_setTag(Component, tagvalue) : Get the tag value
--component_getOwner(Component) : Returns the owner of this component



--Control Class: (Inheritance: Component->Object)
--control_setCaption(control, caption) : sets the text on a control. All the gui objects fall in this category
--control_getCaption(control) : Returns the text of the control
--control_setPosition(control, x,y): sets the x and y position of the object base don the top left position (relative to the client array of the owner object)
--control_getPosition(contron): returns the x and y position of the object (relative to the client array of the owner object)
--control_setSize(control, width,height) : Sets the width and height of the control
--control_getSize(control) : Sets the size of the control
--control_setAlign(control, alignmentoption): sets the alignment of the control
--control_getAlign(control, alignmentoption): gets the alignment of the control
--control_getEnabled(control) : gets the enabled state of the control
--control_setEnabled(control, boolean) : Sets the enabled state of the control
--control_getVisible(control) : Sets the visible state of the control
--control_setVisible(control, boolean) : Gets the visible state of the control
--control_getColor(control) : gets the color
--control_setColor(control, rgb) : Sets the color
--control_getParent(control) : Returns nil or an object that inherits from the Wincontrol class
--control_setParent(control) : Sets the parent for this control
--control_onClick(control, functionnameorstring) : Sets the onclick routine


--WinControl Class: (Inheritance: Control->Component->Object)
--wincontrol_getControlCount(control)  Returns the number of Controls attached to this class
--wincontrol_getControl(control,index) : Returns a WinControl class object
--wincontrol_canFocus(control): returns true if the object can be focused
--wincontrol_focused(control): returns boolean true when focused
--wincontrol_setFocus(control): tries to set keyboard focus the object
--wincontrol_OnEnter(control, function) : Sets an onEnter event. (Triggered on focus enter)
--wincontrol_onExit(control, function) : Sets an onExit event. (Triggered on lost focus)


--Strings Class: (Inheritance : Object) (Mostly an abstract class)
--strings_clear(list) : Deletes all strings in the list
--strings_add(list, string) : adds a string to the list
--strings_delete(list, index) : Deletes a string from the list
--strings_getText(strings) : Returns all the strings as one big string
--strings_indexOf(list, string): Returns the index of the specified string. Returns -1 if not found
--strings_insert(list, index, string): Inserts a string at a specific spot moving the items after it

--strings_getCount(list): Returns the number is strings in the list
--strings_remove(list, string); Removes the given string from the list
--strings_loadFromFile(list, filename) : Load the strings from a textfile
--strings_saveToFile(list, filename) : Save the strings to a textfile

--strings_getString(list, index) : gets the string at the given index
--strings_setString(list, index, string) : Replaces the string at the given index



--Stringlist Class: (Inheritance : Strings->Object)
--stringlist_getDuplicates(list) : returns the duplicates property
--stringlist_setDuplicates(list, Duplicates) : Sets the duplicates property
--stringlist_getSorted(list) : returns true if the list has the sorted property
--stringlist_setSorted(list, boolean) : Sets the sorted property
--stringlist_getCaseSensitive(list) : Returns true if the case sensitive property is set
--stringlist_setCaseSensitive(list, boolean): Sets the case sensitive property


--Form Class: (Inheritance: ScrollingWinControl->CustomControl->WinControl->Control->Component->Object)
--form_centerScreen(form); : Places the form at the center of the screen
--form_hide(form) : Hide the form
--form_show(form) : show the form
--form_showModal(form) : show the form and wait for it to close and get the close result
--form_isForegroundWindow(form): returns true if the specified form has focus
--form_onClose(form, function)  : function (sender) : Return a CloseAction to determine how to close the window


--GraphicControl Class: (Inheritance: Control->Component->Object)
--Nothing for now, perhaps in the future the Canvas


--Label Class: (Inheritance: GraphicControl->Control->Component->Object)


--Splitter Class: (Inheritance: CustomControl->WinControl->Control->Component->Object)


--Panel Class: (Inheritance: CustomControl->WinControl->Control->Component->Object)
--panel_getAlignment(panel) : gets the alignment property
--panel_setAlignment(panel, alignment) : sets the alignment property
--panel_getBevelInner(panel) 
--panel_setBevelInner(panel, PanelBevel)
--panel_getBevelOuter(panel)
--panel_setBevelOuter(panel, PanelBevel) 
--panel_getBevelWidth(panel)
--panel_setBevelWidth(panel, BevelWidth)
--panel_getFullRepaint(panel)
--panel_setFullRepaint(panel, boolean)



--Image Class: (Inheritance: GraphicControl->Control->Component->Object)
--image_loadImageFromFile(filename)
--image_stretch(boolean)
--image_transparent(boolean)

--Edit Class: (Inheritance: WinControl->Control->Component->Object)
--edit_clear(edit)
--edit_selectAll(edit)
--edit_clearSelection(edit)
--edit_copyToClipboard(edit)
--edit_cutToClipboard(edit)
--edit_pasteFromClipboard(edit)
--edit_onChange(edit, function)


--Memo Class: (Inheritance: Edit->WinControl->Control->Component->Object)
--memo_append(memo,string)
--memo_getLines(memo) : returns a Strings class
--memo_getWordWrap(memo)
--memo_setWordWrap(memo, boolean)
--memo_getWantTabs(memo)
--memo_setWantTabs(memo, boolean)
--memo_getWantReturns(memo)
--memo_setWantReturns(memo, boolean)
--memo_getScrollbars(memo)
--memo_setScrollbars(memo, boolean)




--ButtonControl Class: (Inheritance: WinControl->Control->Component->Object)


--Button Class: (Inheritance: ButtonControl->WinControl->Control->Component->Object)
--button_getModalResult(button)
--button_setModalResult(button, mr)

--CheckBox Class: (Inheritance: ButtonControl->WinControl->Control->Component->Object)
--checkbox_getAllowGrayed(CheckBox)
--checkbox_setAllowGrayed(CheckBox, boolean)
--checkbox_getState(checkbox)
--checkbox_setState(checkbox, boolean)
--checkbox_onChange(checkbox, function)

--ToggleBox Class: (Inheritance: CheckBox->ButtonControl->WinControl->Control->Component->Object)

--GroupBox Class: (Inheritance: WinControl->Control->Component->Object)

--RadioGroup class: (Inheritance: GroupBox->WinControl->Control->Component->Object)
--radiogroup_getRows(radiogroup): Returns the number of rows
--radiogroup_getItems(radiogroup): Returns a Strings object
--radiogroup_getColumns(radiogroup): Returns the nuber of columns
--radiogroup_setColumns(radiogroup, count)
--radiogroup_onClick(radiogroup, function)


--ListBox Class: (Inheritance: WinControl->Control->Component->Object) 
--listbox_clear(listbox)
--listbox_getItems(listbox): Returns a strings object
--listbox_getItemIndex(listbox)
--listbox_setItemIndex(listbox,index)


--ComboBox Class: (Inheritance: WinControl->Control->Component->Object)
--combobox_clear(combobox)
--combobox_getItems(combobox)
--combobox_getItemIndex(combobox)
--combobox_setItemIndex(combobox)



--ProgressBar Class: (Inheritance: WinControl->Control->Component->Object)
--progressbar_stepIt(progressbar)
--progressbar_stepBy(progressbar, delta)
--progressbar_getMax(progressbar)
--progressbar_setMax(progressbar, integer)
--progressbar_getMin(progressbar)
--progressbar_setMin(progressbar, integer)
--progressbar_getPosition(progressbar)
--progressbar_setPosition(progressbar, integer)



--TrackBar Class : (Inheritance: WinControl->Control->Component->Object)
--trackbar_getMax(trackbar)
--trackbar_setMax(trackbar, integer)
--trackbar_getMin(trackbar)
--trackbar_setMin(trackbar, integer)
--trackbar_getPosition(progressbar)
--trackbar_setPosition(progressbar, integer)
--trackbar_onChange(trackbar, function)


--CollectionItem Class: (Inheritance: Object)
--usually not used by lua users but just defining it here for future usage



--ListColumn class: (Inheritance: CollectionItem->Object)
--listcolumn_setAutosize(listcolumns, boolean)
--listcolumn_getCaption(listcolumns)
--listcolumn_setCaption(listcolumns, caption)
--listcolumn_getMaxWidth(listcolumns)
--listcolumn_setMaxWidth(listcolumns, width)
--listcolumn_getMinWidth(listcolumns)
--listcolumn_setMinWidth(listcolumns, width)
--listcolumn_getWidth(listcolumns)
--listcolumn_setWidth(listcolumns, width)




--Collection Class: (Inheritance: TObject)
--collection_clear(collection)
--collection_getCount(collection)
--collection_delete(collection, index)


--ListColumns class : (Inheritance: Collection->Object)
--listcolumns_add(listcolumns): Returns a new ListColumn object
--listcolumns_getColumn(listcolumns, index): Returns a ListColum object;

--ListItem Class : (Inheritance: TObject)
--listitem_delete(listitem)
--listitem_getCaption(listitem)
--listitem_setCaption(listitem, string)
--listitem_getSubItems(listitem): Returns a Strings object


--ListItems class : (Inheritance: TObject)
--listitems_clear(listitems)
--listitems_getCount(listitems)
--listitems_add(listitems): Returns a new ListItem object



--Listview Class : (Inheritance: WinControl->Control->Component->Object)
--listview_clear(listview)
--listview_getColumns(listview): Returns a ListColumns object
--listview_getItems(listview) : Returns a ListItems object
--listview_getItemIndex(listview)
--listview_setItemIndex(listview, index)


--Timer Class : (Inheritance: Component->object)
--timer_setInterval(timer, interval)
--timer_onInterval(timer, function)

--OpenDialog Class: (Inheritance: FileDialog->CommonDialog->Component->Object)
--opendialog_execute(openDialog): Shows the dialog and returns the string to the selected file

--SaveDialog Class: (Inheritance: OpenDialog->FileDialog->CommonDialog->Component->Object)

 
--MemoryStream Class (Inheritance: Stream->Object)
--FileStream Class (Inheritance: HandleStream->Stream->Object)


--TableFile class (Inheritance: Object)
--tablefile_saveToFile(tablefile, filename)
--tablefile_getData(tablefile, filename) : Gets a MemoryStream object 


--findTableFile(filename): Returns the TableFile class object for the saved file

--xmplayer_playXM(filename)
--xmplayer_playXM(Stream)
--xmplayer_pause()
--xmplayer_resume()
--xmplayer_stop()
--xmplayer_isPlaying()

--writeRegionToFile(filename, sourceaddress,size) : Writes the given region to a file. Returns the number of bytes written
--readRegionFromFile(filename, destinationaddress)

--resetLuaState(): This will create a new lua state that will be used. (Does not destroy the old one, so memory leak)


--CheatComponent Class: (Inheritance: WinControl->Control->Component->Object)
--The cheatcomponent class is the component used in Cheat Engine 5.x trainers
--Most people will probably want to design their own components but for those that don't know much coding and use the autogenerated trainer this will be used

--cheatcomponent_setActive(cheatcomponent, boolean, deactivatetime OPTIONAL) : sets the cheat object's Active color to enabled or disabled. The deactivatetime parameter turns the active color off automatically after the given time in milliseconds (e.g one hit cheats like increase by)
--cheatcomponent_getActive(cheatcomponent)
--cheatcomponent_setDescription(cheatcomponent, string)
--cheatcomponent_getDescription(cheatcomponent)
--cheatcomponent_setHotkey(cheatcomponent, string)
--cheatcomponent_getHotkey(cheatcomponent)
--cheatcomponent_setDescriptionLeft(cheatcomponent, integer)
--cheatcomponent_getDescriptionLeft(cheatcomponent)
--cheatcomponent_setHotkeyLeft(cheatcomponent, integer)
--cheatcomponent_getHotkeyLeft(cheatcomponent)


--MemoryRecordHotkey Class: (Inheritance: object)
--memoryrecordhotkey_getDescription(hotkey)
--memoryrecordhotkey_getHotkeyString(hotkey)
--memoryrecordhotkey_getID(hotkey)
--memoryrecordhotkey_onHotkey(hotkey, hotkeyfunction): function (hotkey)
--memoryrecordhotkey_onPostHotkey(hotkey, hotkeyfunction): function (hotkey)
--memoryrecordhotkey_getOwner(hotkey): Returns the MemoryRecord this hotkey belongs to
--memoryrecordhotkey_doHotkey(hotkey): Executes the hotkey as if it got triggered by the keyboard


--MemoryRecord Class:
--memoryrecord_getID(memoryrecord)
--memoryrecord_getHotkeyCount(memoryrecord)
--memoryrecord_getHotkey(memoryrecord, index): Returns a memoryrecordhotkey class
--memoryrecord_getHotkeyByID(memoryrecord, ID): Every hotkey in a memoryrecord gets an unique ID. This way you can always find the hotkey even if the order of hotkeys has changed (or deleted)
--memoryrecord_onActivate(memoryrecord, function): function (memoryrecord, before): boolean; If before is true returning false will cause the activation to stop
--memoryrecord_onDeactivate(memoryrecord, function): function (memoryrecord, before): boolean; If before is true returning false will cause the deactivation to stop
--memoryrecord_setDescription(te, description): sets the specified description for this entry
--memoryrecord_getDescription(te): gets the current description of this entry
--memoryrecord_getAddress(te): returns the address and optional offsets for a pointer (note that in 64-bit kernelmode addresses will be rounded down...)
--memoryrecord_setAddress(te,address,offsets OPTIONAL) : Sets the address of a entry. You can give as many offsets as you need
--memoryrecord_getType(te) : returns the Variable type. (vtByte to vtCustom)
--memoryrecord_setType(te, vartype) : sets the type of the entry
--memoryrecord_getValue(te): returns the current value of the cheat table entry as a string
--memoryrecord_setValue(te, value): sets the value of a cheat table entry
--memoryrecord_getScript(te) : If the entry is of type vtAutoAssembler then you can get the script with this routine
--memoryrecord_setScript(te, script)
--memoryrecord_isActive(te)
--memoryrecord_freeze(te, updownfreeze OPTIONAL): sets the entry to frozen state. updownfreeze is optional. 0=freeze, 1=allow increase, 2=allow decrease
--memoryrecord_unfreeze(te) :unfreezes an entry
--memoryrecord_setColor(te, colorrgb): Sets the color of the entry
--memoryrecord_appendToEntry(te,te) : Adds the entry to another entry
--memoryrecord_delete(te) : It's unknown what this function does, all that is known is that after using this command other memrec routines with this table entry value don't work anymore...


--Addresslist Class:
--addresslist_getCount(addresslist)
--addresslist_getMemoryRecord(addresslist, index)
--addresslist_getMemoryRecordByDescription(addresslist, description): --getTableEntry(descriptionname): returns a tableEntry pointer for use with memrec functions
--addresslist_getMemoryRecordByID(addresslist, ID)

--addresslist_createMemoryRecord(addresslist) : --createTableEntry: creates an generic cheat table entry and add it to the list. Returns a tableentry pointer you can use with memrec routines


--registerSymbol(symbolname, address)
--unregisterSymbol(symbolname)

--getNameFromAddress(address)
--inModule(address) : returns true if the given address is inside a module
--inSystemModule(address) : returns true if the given address is inside a system module
--getCommonModuleList: Returns the commonModuleList stringlist. (Do not free this one)



--aobScan("aobstring", protectionflags OPTIONAL, alignmenttype OPTIONAL, alignmentparam HALFOPTIONAL):
--protectionflags is a string. 
--  X=Executable W=Writable memory C=Copy On Write. Add a + to indicate that flag MUST be set and a - to indicate that that flag MUST NOT be set. (* sets it to don't care)
--  Examples: 
--    +W-C = Writable memory exluding copy on write and doesn't care about the Executable flag
--    +X-C-W = Find readonly executable memory
--    +W = Finds all writable memory and don't care about copy on write or execute
--    "" = Find everything (is the same as "*X*C*W" )
--
--
--alignmenttype is an integer: 
--  0=No alignment check
--  1=Address must be dividable by alignmentparam 
--  2=Address must end with alignmentparam
--alignmentparam is a string which either holds the value the addresses must be dividable by or what the last digits of the address must be



--createMemScan(progressbar OPTIONAL) : Returns a new MemScan class object
--MemScan Class (Inheritance: Object)
--memscan_firstScan(memscan, scanoption, vartype, roundingtype, input1, input2 ,startAddress ,stopAddress ,protectionflags ,alignmenttype ,"alignmentparam" ,isHexadecimalInput ,isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan);
--memscan_nextScan(memscan, scanoption, roundingtype, input1,input2, isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan, savedresultname OPTIONAL);
--memscan_newscan(memscan);
--memscan_waitTillDone(memscan)
--memscan_saveCurrentResults(memscan, name)


--createFoundList(memscan)
--foundlist_initialize(foundlist)
--foundlist_deinitialize(foundlist)
--foundlist_getCount(foundlist)
--foundlist_getAddress(foundlist, index) : Returns the address as a string
--foundlist_getValue(foundlist, index)



--supportCheatEngine(attachwindow, hasclosebutton, width, height, position ,yoururl OPTIONAL, extraparameters OPTIONAL, percentageshown OPTIONAL): 
--  Will show an advertising window which will help keep the development of Cheat Engine going.
--  If you provide your own url it will be shown Up to 75% of the time. 
--
--  attachwindow: Type=Form : The form that the ad is attached to
--  hasclosebutton: Type=boolean : If true the window will have a border an a close button at top
--  width, height: Type=integer : 
--    The client width and height of the window.
--    Prefered formats are : 120x600 , 160x600, 300x250, 468x60, 728x90  ,But you are free to use different formats
--
--  Position: Type=integer/enum: The place of the window
--    0=Top, 1=Right, 2=Bottom, 3=left
--
--  Yoururl: Type=string: The url you want to show. When given instead of showing CE's ads 100% it will show your url up to 75%.
--    You can use it for your own income, or for updating users about new versions of your trainer or whatever you feel like
--
--  Extraparameters: Type=String :  are url request parameters you can add to the default parameters (e.g trainername=mytrainer for tracking purposes)  
--
--  PercentageShown: You can change the default of 75% to a smaller value like 50%
--
--
--fuckCheatEngine() : Removes the ad window if it was showing




--dbk_initialize() : Returns true if the dbk driver is loaded in memory. False if it failed for whatever reason (e.g 64-bit and not booted with unsigned driver support)
--dbk_useKernelmodeOpenProcess() : Switches the internal pointer of the OpenProcess api to dbk_OpenProcess
--dbk_useKernelmodeProcessMemoryAccess() : Switches the internal pointer to the ReadProcessMemory and WriteProcessMemory apis to dbk_ReadProcessMemory and dbk_WriteProcessMemory
--dbk_useKernelmodeQueryMemoryRegions() : Switches the internal pointer to the QueryVirtualMemory api to dbk_QueryVirtualMemory
--dbk_getPEProcess(processid) : Returns the pointer of the EProcess structure of the selected processid
--dbk_getPEThread(threadid) : Gets the pointer to the EThread  structure
--dbk_executeKernelMemory(address, parameter) : 
  --Executes a routine from kernelmode (e.g a routine written there with auto assembler)
  --parameter can be a value or an address. It's up to your code how it's handled



