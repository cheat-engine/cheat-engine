--This lua script gets loaded when Cheat Engine loads
--You can use this to define some often used functions and libraries you'd like to use

require("defines")

--[[

List of CE specific functions:

getCEVersion():6.2: Returns a floating point value


fullAccess(address,size):6.2: Changes the protection of a block of memory to writable and executable



note: addresses can be strings, they will get interpreted by ce's symbolhandler

readBytes(address,bytecount, ReturnAsTable ) : returns the bytes at the given address. If ReturnAsTable is true it will return a table instead of multiple bytes
  Reads the bytes at the given address and returns a table containing the read out bytes

writeBytes(address, x,x,x,x,...) : Write the given bytes to the given address from a table
writeBytes(address, table) : Write the given bytes to the given address from a table 


readInteger(address) : Reads an integer from the specified address
readFloat(address) : Reads a single precision floating point value from the specified address
readDouble(address) : Reads a double precision floating point value from the specified address
readString(address, maxlength) : maxlength is just so you won't freeze for too long, set to 6000 if you don't care too much
writeInteger(address,value) : Writes an integer to the specified address. Returns true on success
writeFloat(address,value) : Writes a single precision floating point to the specified address. Returns true on success
writeDouble(address,value) : Writes a double precision floating point to the specified address. Returns true on success
writeString(address,maxlength) : Write a string to the specified address. Returns true on success

readBytesLocal(address,bytecount, ReturnAsTable) : See readBytes but then it's for Cheat engine's memory
readIntegerLocal(address) : Reads a integer from the specified address in CE's memory
readFloatLocal(address) : Reads a single precision floating point value from the specified address in CE's memory
readDoubleLocal(address) : Reads a double precision floating point value from the specified address in CE's memory
readStringLocal(address)
writeIntegerLocal(address,value) : Writes an integer to the specified address in CE's memory. Returns true on success
writeFloatLocal(address,value) : Writes a single precision floating point to the specified address in CE's memory. Returns true on success
writeDoubleLocal(address,value) : Writes a double precision floating point to the specified address in CE's memory. Returns true on success
writeStringLocal(address,string)
writeBytesLocal(address, x,x,x,x,...) : See writeBytes but then it's for Cheat Engine's memory
writeBytesLocal(address, table, , count) : See writeBytes but then it's for Cheat Engine's memory


ansiToUtf8(string): Converts a string in Ansi encoding to UTF8
utf8ToAnsi(string): Converts a string in UTF8 encoding to Ansi
Note: GUI components mainly show in UTF8, some other functions use Ansi, try to find out which ones...


getAddress(string, local OPTIONAL): returns the address of a symbol. Can be a modulename or an export. set Local to true if you wish to querry the symboltable of the ce process
getModuleSize(modulename): Returns the size of a given module (Use getAddress to get the base address)
reinitializeSymbolhandler(): reinitializes the symbolhandler. E.g when new modules have been loaded

errorOnLookupFailure(state): If set to true (default) address lookups in stringform will raise an error if it can not be looked up. This includes symbolnames that are not defined and pointers that are bad. If set to false it will return 0 in those cases

generateAPIHookScript(address, addresstojumpto, addresstogetnewcalladdress OPT) : Generates an auto assembler script which will hook the given address when executed
autoAssemble(text, targetself OPTIONAL) : runs the auto assembler with the given text. Returns true on success (if targetself is set it will assemble into Cheat Engine itself)
showMessage(text) : shows a messagebox with the given text
messageDialog(text, type, buttons...) : pops up a messagebox with a specific icon/sound with the specified buttons (mbok, mbyes, ....)
sleep(milliseconds): pauses for the number of specified milliseconds (1000= 1 sec...)


getOpenedProcessID() : Returns the currently opened process. If none is open, returns 0
getProcessIDFromProcessName(name) : returns a processid
openProcess(processid) : causes cheat engine to open the given processid
openProcess(processname): causes cheat engine to find and open the given process
pause() : pauses the current opened process
unpause(): resumes the current opened process


getPixel(x,y) : returns the rgb value of the pixel at the specific screen coordinate
getMousePos: returns the x,y coordinates of the mouse
setMousePos(x,y): sets the mouse position

isKeyPressed(key) : returns true if the specified key is currently pressed
keyDown(key) : causes the key to go into down state
keyUp(key) :causes the key to go up
doKeyPress(key) : simmulates a key press

shellExecute(command, parameters OPTIONAL, folder OPTIONAL, showcommand OPTIONAL): Executes a given command

getTickCount() : 6.2: Returns the current tickcount since windows was started. Each tick is one millisecond
processMessages() : 6.2: Lets the main eventhandler process the new messages (allows for new button clicks)
integerToUserData(int): 6.2: Converts a given integer to a userdata variable
userDataToInteger(UserDataVar): 6.2: Converts a given userdata variable to an integer


writeToClipboard(text): 6.2: Writes the given text to the clipboard
readFromClipboard(): 6.2: Reads the text from the clipboard

speedhack_setSpeed(speed)
injectDLL(filename): Injects a dll, and returns true on success

loadPlugin(dllnameorpath): Loads the given plugin. Returns nil on failure. On success returns a value of 0 or greater



registerCustomTypeLua(typename, bytecount, bytestovaluefunction, valuetobytesfunction)
  Registers a Custom type based on lua functions
  The bytes to value function should be defined as "function bytestovalue (b1,b2,b3,b4)" and return an integer as result
  The value to bytes function should be defined as "function valuetobytes (integer)" and return the bytes it should write


registerCustomTypeAutoAssembler(typename, bytecount, script)
  Registers a custom type based on an auto assembler script. The script must allocate an "ConvertRoutine" and "ConvertBackRoutine"

onAutoGuess(function) : 
  Registers an function to be called whenever autoguess is used to predict a variable type
  function override (address, ceguess): Return the variable type you want it to be. If no change, just return ceguess





Cheat table functions:

Table related routines: (deprecated, It's recommended to use the memoryrecord class event registration instead)
If a cheat entry is about to get enabled or disabled it will check if a lua function named "_memrec_description_activating" or "_memrec_description_deactivating" is available, and if so call it.
If a cheat entry is enabled or disabled it will check if a lua function named "_memrec_description_activated" or "_memrec_description_deactivated" is available, and if so call it.
It passes the tableEntry pointer as parameter
Example:
If the cheat entry table with description "xxx" gets enabled it will call "_memrec_xxx_activating(te)" before it is activated and "_memrec_xxx_activated(te)" after it has been activated (check with isActive to see if it actually did get activated in case of errors in a script or unreadable memory)
If the cheat entry table with description "xxx" gets disabled it will call "_memrec_xxx_deactivating(te)" before it is activated and "_memrec_xxx_deactivated(te)" after it has been deactivated

-debugging

debug variables
EFLAGS
32-bit: EAX, EBX, ECX, EDX, EDI, ESP, EBP, ESP, EIP 
64-bit: RAX, EBX, RBX, RDX, RDI, RSP, RBP, RSP, RIP, R8, R9, R10, R11, R12, R13, R14, R15 : The value of the register

Debug related routines:
function debugger_onBreakpoint():
When a breaking breakpoint hits (that includes single stepping) and the lua function debugger_onBreakpoint() is defined it will be called and the global variables EAX, EBX, .... will be filled in
Return 0 if you want the userinterface to be updated and anything else if not (e.g: You continued from the breakpoint in your script)



createProcess(path, parameters OPTIONAL, debug OPTIONAL, breakonentrypoint OPTIONAL) : Creates a process. If debug is true it will be created using the windows debugger and if breakonentry is true it will cause a breakpoint to occur on entrypoint

debugProcess(interface OPT): starts the debugger for the currently opened process (won't ask the user) Optional interface: 0=default, 1=windows debug, 2=VEHDebug, 3=Kerneldebug
debug_setBreakpoint(address, size OPTIONAL, trigger OPTIONAL) : sets a breakpoint of a specific size at the given address. if trigger is bptExecute then size is ignored. If trigger is ignored then it will be of type bptExecute, which obviously also ignores the size then as well
debug_removeBreakpoint(address) : if the given address is a part of a breakpoint it will be removed
debug_continueFromBreakpoint(continueMethod) : if the debugger is currently waiting to continue you can continue with this. Valid parameters are :co_run (just continue), co_stepinto(when on top of a call, follow it), co_stepover (when on top of a call run till after the call)
debug_getXMMPointer(xmmregnr) : 
  Returns the address of the specified xmm register of the thread that is currently broken
  This is a LOCAL Cheat Engine address. Use Local memory access functions to read and modify
  xmmregnr can be 0 to 15 (0 to 7 on 32-bit)


The following routines describe last branch recording. These functions only work when kernelmode debugging is used and using windows XP (vista and later work less effective or not at all because the operating system interferes.  Might also be intel specific. A dbvm upgrade in the future might make this work for windows vista and later)
debug_setLastBranchRecording(boolean): When set the Kernel debugger will try to record the last branch(es) taken before a breakpoint happens
debug_getMaxLastBranchRecord() : Returns the maximum branch record your cpu can store (-1 if none)
debug_getLastBranchRecord(index): Returns the value of the Last Branch Record at the given index (when handling a breakpoint)



Changing registers:
Different from ce 6.0
When the debugger is waiting to continue you can change the register variables. When you continue those register values will be set in the thread's context



gui
closeCE() : just closes ce
hideAllCEWindows() : makes all normal ce windows invisible (e.g trainer table)
unhideMainCEwindow() : shows the main cheat engine window


interval
getAutoAttachList(): returns the AutoAttach StringList object. It can be controlled with the stringlist_ routines (it's not recommended to destroy this list object)


AOBScan(x,x,x,x,...):
scans the currently opened process and returns a StringList object containing all the results. don't forget to free this list when done
Bytevalue of higher than 255 or anything not an integer will be seen as a wildcard
AOBScan(aobstring): see above but here you just input one string





function onOpenProcess(processid) : When this function is defined it will be called each time a process has been opened (note that a process can be opened multiple times in a row, e.g when attaching the debugger it might first open it and then attach the debugger which opens it again...)

function debugger_onModuleLoad(modulename, baseaddress) : 
this routine is called when a module is loaded. Only works for the windows debugger
return 1 if you want to cause the debugger to break



CE 6.1
Regarding eventhandlers. You can initialize them using both a string of a functionname or the function itself.
If initialized using a function itself it won't be able to get saved in the table

allocateSharedMemory(name, size):
  Creates a shared memory object of the given size if it doesn't exist yet. If size is not given and there is no shared region with this name then the default size of 4096 is used
  It then maps this shared memory block into the currently targeted process. It returns the address of mapped region in the target process


getForegroundProcess() : Returns the processID of the process that is currently on top 
 
cheatEngineIs64Bit(): Returns true if CE is 64-bit, false if 32-bit
targetIs64Bit(): Returns true if the target process is 64-bit, false if 32-bit


getCheatEngineDir(): Returns the folder Cheat Engine is located at

disassemble(address): Disassembles the given address and returns a string in the format of "address - bytes - opcode : extra"
splitDisassembledString(disassembledstring): Returns 4 strings. The address, bytes, opcode and extra field

getInstructionSize(address): Returns the size of an instruction (basically it disassembles the instruction and returns the number of bytes for you)
getPreviousOpcode(address): Returns the address of the previous opcode (this is just an estimated guess)




undefined property functions. Not all properties of all classes have been explicitly exposed to lua, but if you know the name of a property of a specific class you can still access them (assuming they are declared as published in the pascal class declaration)
getPropertyList(class) : Returns a stringlist object containing all the published properties of the specified class (free the list when done) (Note, not all classed with properties have 'published' properties. E.g: stringlist)
setProperty(class, propertyname, propertyvalue) : Sets the value of a published property of a class (Won't work for method properties)
getProperty(class, propertyname) : Gets the value of a published property of a class (Won't work for method properties)
setMethodProperty(class, propertyname, function):6.2: Sets the method property to the specific function
    



getFormCount() : Returns the total number of forms assigned to the main CE application
getForm(index): Returns the form at the specific index

getMemoryViewForm() : Returns the main memoryview form class object which can be accessed using the Form_ class methods and the methods of the classes it inherits from. There can be multiple memory views, but this will only find the original/base
getMainForm() : Returns the main form class object which can be accessed using the Form_ class methods and the methods of the classes it inherits from
getAddressList() : Returns the cheat table addresslist object
getFreezeTimer()
getUpdateTimer()

GenericHotkey Class : (Inheritance:  Object)
createHotkey(function, key, ...) : returns an initialized GenericHotkey class object. Maximum of 5 keys
generichotkey_setKeys(hotkey, key, ....)
generichotkey_onHotkey(hotkey, function)

beep() : Plays the fabulous beep/ping sound!


class helper functions
inheritsFromObject(class): Returns true if given any class
inheritsFromComponent(class): Returns true if the given object inherits from the Component class
inheritsFromControl(class): Returns true if the given object inherits from the Control class
inheritsFromWinControl(class): Returns true if the given object inherits from the WinControl class


Class definitions
Object class: (Inheritance: )
object_getClassName(object): Returns the classname of the given object
object_destroy(object) : Destroys the object (basically everything inherits from this class)


Component Class: (Inheritance: Object)
component_getComponentCount(Component) : Returns the number of components attached to his component
component_getComponent(Component, index) : Returns the specific component
component_findComponentByName(component, name) : Returns the component with this name
component_getName(Component) : Return the name
component_setName(Component, newname) : Changes the name
component_getTag(Component) : Sets an integer value. You can use this for ID's
component_setTag(Component, tagvalue) : Get the tag value
component_getOwner(Component) : Returns the owner of this component



Control Class: (Inheritance: Component->Object)
control_setCaption(control, caption) : sets the text on a control. All the gui objects fall in this category
control_getCaption(control) : Returns the text of the control
control_setPosition(control, x,y): sets the x and y position of the object base don the top left position (relative to the client array of the owner object)
control_getPosition(contron): returns the x and y position of the object (relative to the client array of the owner object)
control_setSize(control, width,height) : Sets the width and height of the control
control_getSize(control) : Sets the size of the control
control_setAlign(control, alignmentoption): sets the alignment of the control
control_getAlign(control, alignmentoption): gets the alignment of the control
control_getEnabled(control) : gets the enabled state of the control
control_setEnabled(control, boolean) : Sets the enabled state of the control
control_getVisible(control) : gets the visible state of the control
control_setVisible(control, boolean) : sets the visible state of the control
control_getColor(control) : gets the color
control_setColor(control, rgb) : Sets the color
control_getParent(control) : Returns nil or an object that inherits from the Wincontrol class
control_setParent(control, wincontrol) : Sets the parent for this control
control_getPopupMenu(control)
control_setPopupMenu(control)
control_getFont(label): 6.2+: Returns the Font object of this object
control_onClick(control, functionnameorstring) : Sets the onclick routine
control_doClick(control): 6.2+: Executes the current function under onClick



Region Class (6.2+): (Region->GraphicsObject->Object)
createRegion(): Created an empty region
region_addRectangle(region, x1, y1, x2, y2): Adds a rectangle to the region
region_addPolygon(region, tablewithcoordinates): Adds an array of 2D locations. (example : {{0,0},{100,100}, {0,100}} for a triangle )



WinControl Class: (Inheritance: Control->Component->Object)
wincontrol_getControlCount(wincontrol)  Returns the number of Controls attached to this class
wincontrol_getControl(wincontrol,index) : Returns a WinControl class object
wincontrol_getControlAtPos(wincontrol, x,y): 6.2: Gets the control at the given x,y position relative to the wincontrol's position 
wincontrol_canFocus(wincontrol): returns true if the object can be focused
wincontrol_focused(wincontrol): returns boolean true when focused
wincontrol_setFocus(wincontrol): tries to set keyboard focus the object
wincontrol_setShape(wincontrol, Region):6.2+: Sets the region object as the new shape for this wincontrol
wincontrol_setShape(wincontrol, Bitmap):6.2+
wincontrol_onEnter(wincontrol, function) : Sets an onEnter event. (Triggered on focus enter)
wincontrol_onExit(wincontrol, function) : Sets an onExit event. (Triggered on lost focus)


MenuItem class(Inheritance: Component->Object)
createMenuItem(ownermenu) : Creates a menu item that gets added to the owner menu
menuItem_getCaption(menuitem) : Gets the caption of the menu item
menuItem_setCaption(menuitem, caption) : Sets the caption of the menu item
menuItem_getShortcut(menuitem): Returns the shortcut for this menu item
menuItem_setShortcut(menuitem, shortcut): Sets the shortcut for this menuitem. A shortcut is a string in the form of ("ctrl+x")
menuItem_getCount(menuitem)
menuItem_getItem(menuitem, index) : Returns the menuitem object at the given index
menuItem_add(menuitem, menuitem) : Adds a menuItem as a submenu item
menuItem_insert(menuitem, index, menuitem): Adds a menuItem as a submenu item at the given index
menuItem_delete(menuitem, index)
menuItem_onClick(menuitem, function) : Sets an onClick event


Menu Class: (Inheritance: Component->Object)
menu_getItems(menu) : Returns the MenuItem of this Menu

MainMenu Class: (Inheritance: Menu->Component->Object)
createMainMenu(form)
  The mainmenu is the menu at the top of a window

PopupMenu Class: (Inheritance: Menu->Component->Object)
createPopupMenu(owner)
  The popup menu is the menu that popus up when showing the (rightclick) context of an control


Strings Class: (Inheritance : Object) (Mostly an abstract class)
strings_clear(list) : Deletes all strings in the list
strings_add(list, string) : adds a string to the list
strings_delete(list, index) : Deletes a string from the list
strings_getText(strings) : Returns all the strings as one big string
strings_setText(strings) : Sets the strings of the given strings object to the given text (can be multiline)
strings_indexOf(list, string): Returns the index of the specified string. Returns -1 if not found
strings_insert(list, index, string): Inserts a string at a specific spot moving the items after it

strings_getCount(list): Returns the number is strings in the list
strings_remove(list, string); Removes the given string from the list
strings_loadFromFile(list, filename) : Load the strings from a textfile
strings_saveToFile(list, filename) : Save the strings to a textfile

strings_getString(list, index) : gets the string at the given index
strings_setString(list, index, string) : Replaces the string at the given index



Stringlist Class: (Inheritance : Strings->Object)
createStringlist() : Creates a stringlist class object (for whatever reason, lua strings are probably easier to use)
stringlist_getDuplicates(list) : returns the duplicates property
stringlist_setDuplicates(list, Duplicates) : Sets the duplicates property (dupIgnore, dupAccept, dupError)
stringlist_getSorted(list) : returns true if the list has the sorted property
stringlist_setSorted(list, boolean) : Sets the sorted property
stringlist_getCaseSensitive(list) : Returns true if the case sensitive property is set
stringlist_setCaseSensitive(list, boolean): Sets the case sensitive property




Form Class: (Inheritance: ScrollingWinControl->CustomControl->WinControl->Control->Component->Object)
createForm(visible OPT): creates a Form class object(window) and returns the pointer for it. Visible is default true but can be changed
createFormFromFile(filename): Returns the generated form
form_saveToFile(form, filename): Saves a userdefined form. (DOES NOT WORK ON NORMAL FORMS LIKE MAINFORM)
form_centerScreen(form); : Places the form at the center of the screen
form_hide(form) : Hide the form
form_show(form) : show the form
form_close(form): 6.2: Closes the form. Without an onClose this will be the same as hide 
form_showModal(form) : show the form and wait for it to close and get the close result
form_isForegroundWindow(form): returns true if the specified form has focus
form_onClose(form, function)  : function (sender) : Return a CloseAction to determine how to close the window
form_getMenu(form) : Returns the mainmenu object of this form
form_setMenu(form, mainmenu)

form_setBorderStyle(form, borderstyle): 6.2+: Sets the borderstyle of the window
form_getBorderStyle

form_getDoNotSaveInTable(form): Returns the DoNotSaveInTable property
form_setDoNotSaveInTable(form, boolean): Sets the DoNotSaveInTable property

form_printToRasterImage(form, rasterimage):6.2+: Draws the contents of the form to a rasterimage class object
form_dragNow(form): 6.2+: Call this on mousedown on any object if you wish that the mousemove will drag the whole form arround. Useful for borderless windows (Dragging will stop when the mouse button is released)


GraphicControl Class: (Inheritance: Control->Component->Object)
graphicControl_getCanvas(graphiccontrol) : Returns the Canvas object for the given object that has inherited from customControl



Label Class: (Inheritance: GraphicControl->Control->Component->Object)
createLabel(owner): Creates a Label class object which belongs to the given owner. Owner can be any object inherited from WinControl



Splitter Class: (Inheritance: CustomControl->WinControl->Control->Component->Object)
createSplitter(owner): Creates a Splitter class object which belongs to the given owner. Owner can be any object inherited from WinControl

Panel Class: (Inheritance: CustomControl->WinControl->Control->Component->Object)
createPanel(owner): Creates a Panel class object which belongs to the given owner. Owner can be any object inherited from WinControl
panel_getAlignment(panel) : gets the alignment property
panel_setAlignment(panel, alignment) : sets the alignment property
panel_getBevelInner(panel) 
panel_setBevelInner(panel, PanelBevel)
panel_getBevelOuter(panel)
panel_setBevelOuter(panel, PanelBevel) 
panel_getBevelWidth(panel)
panel_setBevelWidth(panel, BevelWidth)
panel_getFullRepaint(panel)
panel_setFullRepaint(panel, boolean)



Image Class: (Inheritance: GraphicControl->Control->Component->Object)
createImage(owner): Creates an Image class object which belongs to the given owner. Owner can be any object inherited from WinControl
image_loadImageFromFile(image, filename)
image_stretch(image, boolean)
image_transparent(image, boolean)
image_getCanvas(image)
image_getPicture(image) : Returns the Picture object of this image


Edit Class: (Inheritance: WinControl->Control->Component->Object)
createEdit(owner): Creates an Edit class object which belongs to the given owner. Owner can be any object inherited from WinControl
edit_clear(edit)
edit_selectAll(edit)
edit_clearSelection(edit)
edit_copyToClipboard(edit)
edit_cutToClipboard(edit)
edit_pasteFromClipboard(edit)
edit_onChange(edit, function)


Memo Class: (Inheritance: Edit->WinControl->Control->Component->Object)
createMemo(owner): Creates a Memo class object which belongs to the given owner. Owner can be any object inherited from WinControl
memo_append(memo,string)
memo_getLines(memo) : returns a Strings class
memo_getWordWrap(memo)
memo_setWordWrap(memo, boolean)
memo_getWantTabs(memo)
memo_setWantTabs(memo, boolean)
memo_getWantReturns(memo)
memo_setWantReturns(memo, boolean)
memo_getScrollbars(memo)
memo_setScrollbars(memo, scrollbarenumtype) : 
  Sets the scrollbars. Horizontal only takes affect when wordwrap is disabled
  valid enum types: 
    ssNone : No scrollbars
    ssHorizontal: Has a horizontal scrollbar
    ssVertical: Has a vertical scrollbar
    ssBoth: Has both scrollbars
    ssAutoHorizontal: Same as above but only shows when there actually is something to scroll for
    ssAutoVertical: " " " " ...
    ssAutoBoth: " " " " ...





ButtonControl Class: (Inheritance: WinControl->Control->Component->Object)


Button Class: (Inheritance: ButtonControl->WinControl->Control->Component->Object)
createButton(owner): Creates a Button class object which belongs to the given owner. Owner can be any object inherited from WinControl
button_getModalResult(button)
button_setModalResult(button, mr)

CheckBox Class: (Inheritance: ButtonControl->WinControl->Control->Component->Object)
createCheckBox(owner): Creates a CheckBox class object which belongs to the given owner. Owner can be any object inherited from WinControl
checkbox_getAllowGrayed(CheckBox)
checkbox_setAllowGrayed(CheckBox, boolean)
checkbox_getState(checkbox): Returns a state for the checkbox. (cbUnchecked, cbChecked, cbGrayed)
checkbox_setState(checkbox, boolean): Sets the state of the checkbox
checkbox_onChange(checkbox, function)

ToggleBox Class: (Inheritance: CheckBox->ButtonControl->WinControl->Control->Component->Object)
createToggleBox(owner): Creates a ToggleBox class object which belongs to the given owner. Owner can be any object inherited from WinControl

GroupBox Class: (Inheritance: WinControl->Control->Component->Object)
createGroupBox(owner): Creates a GroupBox class object which belongs to the given owner. Owner can be any object inherited from WinControl


RadioGroup class: (Inheritance: GroupBox->WinControl->Control->Component->Object)
createRadioGroup(owner): Creates a RadioGroup class object which belongs to the given owner. Owner can be any object inherited from WinControl
radiogroup_getRows(radiogroup): Returns the number of rows
radiogroup_getItems(radiogroup): Returns a Strings object
radiogroup_getColumns(radiogroup): Returns the nuber of columns
radiogroup_setColumns(radiogroup, count)
radiogroup_getItemIndex(radiogroup)
radiogroup_setItemIndex(radiogroup)

radiogroup_onClick(radiogroup, function)


ListBox Class: (Inheritance: WinControl->Control->Component->Object) 
createListBox(owner): Creates a ListBox class object which belongs to the given owner. Owner can be any object inherited from WinControl
listbox_clear(listbox)
listbox_getItems(listbox): Returns a strings object
listbox_getItemIndex(listbox)
listbox_setItemIndex(listbox,index)
listbox_getCanvas(listbox)


ComboBox Class: (Inheritance: WinControl->Control->Component->Object)
createComboBox(owner): Creates a ComboBox class object which belongs to the given owner. Owner can be any object inherited from WinControl
combobox_clear(combobox)
combobox_getItems(combobox)
combobox_getItemIndex(combobox)
combobox_setItemIndex(combobox)
combobox_getCanvas(combobox)



ProgressBar Class: (Inheritance: WinControl->Control->Component->Object)
createProgressBar(owner): Creates a ProgressBar class object which belongs to the given owner. Owner can be any object inherited from WinControl
progressbar_stepIt(progressbar)
progressbar_stepBy(progressbar, delta)
progressbar_getMax(progressbar)
progressbar_setMax(progressbar, integer)
progressbar_getMin(progressbar)
progressbar_setMin(progressbar, integer)
progressbar_getPosition(progressbar)
progressbar_setPosition(progressbar, integer)



TrackBar Class : (Inheritance: WinControl->Control->Component->Object)
createTrackBar(owner): Creates a TrackBar class object which belongs to the given owner. Owner can be any object inherited from WinControl
trackbar_getMax(trackbar)
trackbar_setMax(trackbar, integer)
trackbar_getMin(trackbar)
trackbar_setMin(trackbar, integer)
trackbar_getPosition(progressbar)
trackbar_setPosition(progressbar, integer)
trackbar_onChange(trackbar, function)


CollectionItem Class: (Inheritance: Object)
usually not used by lua users but just defining it here for future usage



ListColumn class: (Inheritance: CollectionItem->Object)
listcolumn_setAutosize(listcolumns, boolean)
listcolumn_getCaption(listcolumns)
listcolumn_setCaption(listcolumns, caption)
listcolumn_getMaxWidth(listcolumns)
listcolumn_setMaxWidth(listcolumns, width)
listcolumn_getMinWidth(listcolumns)
listcolumn_setMinWidth(listcolumns, width)
listcolumn_getWidth(listcolumns)
listcolumn_setWidth(listcolumns, width)




Collection Class: (Inheritance: TObject)
collection_clear(collection)
collection_getCount(collection)
collection_delete(collection, index)


ListColumns class : (Inheritance: Collection->Object)
listcolumns_add(listcolumns): Returns a new ListColumn object
listcolumns_getColumn(listcolumns, index): Returns a ListColum object;

ListItem Class : (Inheritance: TObject)
listitem_delete(listitem)
listitem_getCaption(listitem) : Returns the first columns string of the listitem
listitem_setCaption(listitem, string) : Sets the first column string of the listitem
listitem_getChecked(listitem) : Returns true if the listitem is checked
listitem_setChecked(listitem, state): Sets the checkbox of the listbox to the given state
listitem_getSubItems(listitem): Returns a Strings object


ListItems class : (Inheritance: TObject)
listitems_clear(listitems)
listitems_getCount(listitems)
listitems_getItem(li, index) : 6.2:Return the listitem object at the given index
listitems_add(listitems): Returns a new ListItem object



Listview Class : (Inheritance: WinControl->Control->Component->Object)
createListView(owner): Creates a ListView class object which belongs to the given owner. Owner can be any object inherited from WinControl
listview_clear(listview)
listview_getColumns(listview): Returns a ListColumns object
listview_getItems(listview) : Returns a ListItems object
listview_getItemIndex(listview)
listview_setItemIndex(listview, index)
listview_getCanvas(listview)


Timer Class : (Inheritance: Component->object)
createTimer(owner, enabled OPT): 
  Creates a timer object. If enabled is not given it will be enabled by default (will start as soon as an onTimer event has been assigned)
  Owner may be nil, but you will be responsible for destroying it instead of being the responsibility of the owner object)

timer_setInterval(timer, interval) : Sets the speed on how often the timer should trigger. In milliseconds (1000=1 second)
timer_onTimer(timer, function)
timer_getEnabled(timer)
timer_setEnabled(timer, boolean)

Ce 6.1 Alpha 8+

CustomControl class (CustomControl->WinControl->Control->Component->Object)
customControl_getCanvas(customcontrol) : Returns the Canvas object for the given object that has inherited from customControl


Canvas Class : (Inheritance: CustomCanvas->Object)
canvas_getBrush(canvas): Returns the brush object of this canvas
canvas_getPen(canvas): Returns the pen object of this canvas
canvas_getFont(canvas): Returns the font object of this canvas
canvas_getWidth(canvas)
canvas_getHeight(canvas)
canvas_getPenPosition(canvas)
canvas_setPenPosition(canvas, x,y)


canvas_line(canvas, sourcex, sourcey, destinationx, destinationy)
canvas_lineTo(canvas, destinationx, destinationy)
canvas_rect(canvas, x1,y1,x2,y2)
canvas_fillRect(canvas, x1,y1,x2,y2)
canvas_textOut(canvas, x,y, text)
canvas_getTextWidth(canvas, text)
canvas_getTextHeight(canvas, text)
canvas_getPixel(canvas, x,y)
canvas_setPixel(canvas, x,y,color)
canvas_floodFill(canvas, x,y)
canvas_ellipse(canvas, x1,y1,x2,y2)
canvas_gradientFill(canvas, x1,y1,x2,y2, startcolor, stopcolor, direction) : Gradient fills a rectangle. Direction can be 0 or 1. 0=Vertical 1=Horizontal
canvas_copyRect(dest_canvas, dest_x1,dest_y1,dest_x2,dest_y2, sourceCanvas, source_x1,source_y1,source_x2,source_y2)
canvas_draw(canvas, x,y, graphic) : Draw the image of a specific Graphic class

Pen Class : (Inheritance: CustomPen->CanvasHelper->Object)
pen_getColor(pen)
pen_setColor(pen, color)
pen_getWidth(pen)
pen_setWidth(pen, width)


Brush Class : (Inheritance: CustomBrush->CanvasHelper->Object)
brush_getColor
brush_setColor

Font Class : (Inheritance: CustomFont->CanvasHelper->Object)
font_getName
font_setName
font_getSize
font_setSize
font_getColor
font_setColor


Graphic Class : (Inheritance: Object) : Abstract class
graphic_getWidth(graphic): Gets the current width in pixels of this graphics object
graphic_setWidth(graphic, width): Sets thw width in pixels
graphic_getHeight(graphic)
graphic_setHeight(graphic, height)

RasterImage class: (Inheritance: Graphic->Object) : Base class for some graphical controls
rasterimage_getCanvas(RasterImage): Returns the Canvas object for this image
rasterimage_getPixelFormat((rasterimage): 6.2: Returns the current pixelformat
rasterimage_setPixelFormat(rasterimage, pixelformat): 6.2: Sets the pixelformat for this image. Will clear the current image if it had one. Supported pixelformats: pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit (recommended)

rasterimage_setTransparent(rasterimage,state): 6.2: Will set the image to support transparency or not
rasterimage_getTransparent(rasterimage): 6.2: Returns true if the image supports transparency
rasterimage_setTransparentColor(rasterimage, color): 6.2:Sets the color that will be rendered as transparent when drawn
rasterimage_getTransparentColor(rasterimage): 6.2: Returns the color set to be transparent


Bitmap class: (Inheritance: CustomBitmap->RasterImage->Graphic->Object) : Bitmap based Graphic object
PortableNetworkGraphic Class: (Inheritence: TCustomBitmap->RasterImage->Graphic->Object)
JpegImage Class: (Inheritence: TCustomBitmap->RasterImage->Graphic->Object)
 


Picture Class : (Inheritance: Object) : Container for the Graphic class
createPicture() : Returns a empty picture object
picture_loadFromFile(picture, filename)
picture_loadFromStream(picture, stream, originalextension OPTIONAL)
picture_assign(picture, sourcepicture)
picture_getGraphic(picture) : Gets the Graphic object of this picture
picture_getPNG(picture): Returns a PortableNetworkGraphic Class object (Can be used from scratch)
picture_getBitmap(picture): Returns a Bitmap Class object (Can be used from scratch)
picture_getJpeg(picture): Returns a JpegImage Class object (Picture must be initialized with a jpeg file first)





OpenDialog Class: (Inheritance: FileDialog->CommonDialog->Component->Object)
openDialog_execute(openDialog): Shows the dialog and returns the string to the selected file

SaveDialog Class: (Inheritance: OpenDialog->FileDialog->CommonDialog->Component->Object)

 
MemoryStream Class (Inheritance: Stream->Object)
FileStream Class (Inheritance: HandleStream->Stream->Object)


TableFile class (Inheritance: Object)
tablefile_saveToFile(tablefile, filename)
tablefile_getData(tablefile, filename) : Gets a MemoryStream object 


findTableFile(filename): Returns the TableFile class object for the saved file

xmplayer_playXM(filename)
xmplayer_playXM(Stream)
xmplayer_pause()
xmplayer_resume()
xmplayer_stop()
xmplayer_isPlaying()

writeRegionToFile(filename, sourceaddress,size) : Writes the given region to a file. Returns the number of bytes written
readRegionFromFile(filename, destinationaddress)

resetLuaState(): This will create a new lua state that will be used. (Does not destroy the old one, so memory leak)


CheatComponent Class: (Inheritance: WinControl->Control->Component->Object)
The cheatcomponent class is the component used in Cheat Engine 5.x trainers
Most people will probably want to design their own components but for those that don't know much coding and use the autogenerated trainer this will be used

cheatcomponent_setActive(cheatcomponent, boolean, deactivatetime OPTIONAL) : sets the cheat object's Active color to enabled or disabled. The deactivatetime parameter turns the active color off automatically after the given time in milliseconds (e.g one hit cheats like increase by)
cheatcomponent_getActive(cheatcomponent)
cheatcomponent_setDescription(cheatcomponent, string)
cheatcomponent_getDescription(cheatcomponent)
cheatcomponent_setHotkey(cheatcomponent, string)
cheatcomponent_getHotkey(cheatcomponent)
cheatcomponent_setDescriptionLeft(cheatcomponent, integer)
cheatcomponent_getDescriptionLeft(cheatcomponent)
cheatcomponent_setHotkeyLeft(cheatcomponent, integer)
cheatcomponent_getHotkeyLeft(cheatcomponent)
cheatcomponent_getEditValue(cheatcomponent) : Returns the text from the editbox of this component
cheatcomponent_setEditValue(cheatcomponent, value): Sets the text on an editbox


MemoryRecordHotkey Class: (Inheritance: object)
memoryrecordhotkey_getDescription(hotkey)
memoryrecordhotkey_getHotkeyString(hotkey)
memoryrecordhotkey_getID(hotkey)
memoryrecordhotkey_onHotkey(hotkey, hotkeyfunction): function (hotkey)
memoryrecordhotkey_onPostHotkey(hotkey, hotkeyfunction): function (hotkey)
memoryrecordhotkey_getOwner(hotkey): Returns the MemoryRecord this hotkey belongs to
memoryrecordhotkey_doHotkey(hotkey): Executes the hotkey as if it got triggered by the keyboard


MemoryRecord Class:
memoryrecord_getID(memoryrecord)
memoryrecord_getHotkeyCount(memoryrecord)
memoryrecord_getHotkey(memoryrecord, index): Returns a memoryrecordhotkey class
memoryrecord_getHotkeyByID(memoryrecord, ID): Every hotkey in a memoryrecord gets an unique ID. This way you can always find the hotkey even if the order of hotkeys has changed (or deleted)
memoryrecord_onActivate(memoryrecord, function): function (memoryrecord, before, currentstate): boolean; If before is true returning false will cause the activation to stop
memoryrecord_onDeactivate(memoryrecord, function): function (memoryrecord, before, currentstate): boolean; If before is true returning false will cause the deactivation to stop
memoryrecord_onDestroy(memoryrecord, function): function (memoryrecord) : Called when a memory record is destroyed
memoryrecord_setDescription(te, description): sets the specified description for this entry
memoryrecord_getDescription(te): gets the current description of this entry
memoryrecord_getAddress(te): returns the address. If it is a pointer it returns a secondary return value which is a table which starts as base address followed by the offset)
memoryrecord_setAddress(te,address,offsets OPTIONAL) : Sets the address of a entry. You can give as many offsets as you need
memoryrecord_getType(te) : returns the Variable type. (vtByte to vtCustom)
memoryrecord_setType(te, vartype) : sets the type of the entry

memoryrecord_getValue(te): returns the current value of the cheat table entry as a string
memoryrecord_setValue(te, value): sets the value of a cheat table entry
memoryrecord_getScript(te) : If the entry is of type vtAutoAssembler then you can get the script with this routine
memoryrecord_setScript(te, script)
memoryrecord_isActive(te)
memoryrecord_freeze(te, updownfreeze OPTIONAL): sets the entry to frozen state. updownfreeze is optional. 0=freeze, 1=allow increase, 2=allow decrease
memoryrecord_unfreeze(te) :unfreezes an entry
memoryrecord_setColor(te, colorrgb): Sets the color of the entry
memoryrecord_appendToEntry(te,te) : Adds the entry to another entry


memoryrecord_string_getSize(te):(6.2+) 
memoryrecord_string_setSize(te, integer):(6.2+) 
memoryrecord_string_getUnicode(te):(6.2+) 
memoryrecord_string_setUnicode(te, boolean):(6.2+) 
memoryrecord_binary_getStartbit(te):(6.2+) 
memoryrecord_binary_setStartbit(te, integer):(6.2+) 
memoryrecord_binary_getSize(te):(6.2+) 
memoryrecord_binary_setSize(te, integer):(6.2+) 
memoryrecord_aob_getSize(te):(6.2+) 
memoryrecord_aob_setSize(te, integer):(6.2+) 


memoryrecord_isSelected(te): (6.2+) Returns true or false depending on if it's currently selected or not
memoryrecord_delete(te) : It's unknown what this function does, all that is known is that after using this command other memrec routines with this table entry value don't work anymore...


Addresslist Class:
addresslist_getCount(addresslist)
addresslist_getMemoryRecord(addresslist, index)
addresslist_getMemoryRecordByDescription(addresslist, description): getTableEntry(descriptionname): returns a tableEntry pointer for use with memrec functions
addresslist_getMemoryRecordByID(addresslist, ID)

addresslist_createMemoryRecord(addresslist) : createTableEntry: creates an generic cheat table entry and add it to the list. Returns a tableentry pointer you can use with memrec routines

addresslist_getSelectedRecords(Addresslist): 6.2+: Returns a table of all the selected records






registerSymbol(symbolname, address, OPTIONAL donotsave): Registers a userdefined symbol. If donotsave is true this symbol will not get saved when the table is saved
unregisterSymbol(symbolname)

getNameFromAddress(address)
inModule(address) : returns true if the given address is inside a module
inSystemModule(address) : returns true if the given address is inside a system module
getCommonModuleList: Returns the commonModuleList stringlist. (Do not free this one)



AOBScan("aobstring", protectionflags OPTIONAL, alignmenttype OPTIONAL, alignmentparam HALFOPTIONAL):
protectionflags is a string. 
  X=Executable W=Writable memory C=Copy On Write. Add a + to indicate that flag MUST be set and a - to indicate that that flag MUST NOT be set. (* sets it to don't care)
  Examples: 
    +W-C = Writable memory exluding copy on write and doesn't care about the Executable flag
    +X-C-W = Find readonly executable memory
    +W = Finds all writable memory and don't care about copy on write or execute
    "" = Find everything (is the same as "*X*C*W" )


alignmenttype is an integer: 
  0=No alignment check
  1=Address must be dividable by alignmentparam 
  2=Address must end with alignmentparam
alignmentparam is a string which either holds the value the addresses must be dividable by or what the last digits of the address must be



createMemScan(progressbar OPTIONAL) : Returns a new MemScan class object
getCurrentMemscan() : Returns the current memory scan object. If tabs are used the current tab's memscan object
MemScan Class (Inheritance: Object)
memscan_firstScan(memscan, scanoption, vartype, roundingtype, input1, input2 ,startAddress ,stopAddress ,protectionflags ,alignmenttype ,"alignmentparam" ,isHexadecimalInput ,isNotABinaryString, isunicodescan, iscasesensitive);
  Does an initial scan.
  memscan: The MemScan object created with createMemScan
  scanOption: Defines what type of scan is done. Valid values for firstscan are:
    soUnknownValue: Unknown initial value scan
    soExactValue: Exact Value scan
    soValueBetween: Value between scan
    soBiggerThan: Bigger than ... scan
    soSmallerThan: smaller than ... scan

  vartype: Defines the variable type. Valid variable types are:
    vtByte
    vtWord  2 bytes
    vtDword 4 bytes
    vtQword 8 bytes
    vtSingle float
    vtDouble
    vtString
    vtByteArray
    vtBinary
    vtAll

  roundingtype: Defined the way scans for exact value floating points are handled
    rtRounded : Normal rounded scans. If exact value = "3" then it includes 3.0 to 3.49999999. If exact value is "3.0" it includes 3.00 to 3.0499999999
    rtTruncated: Truncated algoritm. If exact value = "3" then it includes 3.0 to 3.99999999. If exact value is "3.0" it includes 3.00 to 3.099999999
    rtExtremerounded: Rounded Extreme. If exact value = "3" then it includes 2.0000001 to 3.99999999. If exact value is "3.0" it includes 2.900000001 to 3.099999999

  input1: If required by the scanoption this is a string of the given variable type
  input2: If requires by the scanoption this is the secondary input

  startAddress : The start address to scan from. You want to set this to 0
  stopAddress  : The address the scan should stop at. (You want to set this to 0xffffffffffffffff)

  protectionflags : See aobscan about protectionflags
  alignmenttype : Scan alignment type. Valid options are:
    fsmNotAligned : No alignment check
    fsmAligned    : The address must be dividable by the value in alignmentparam
    fsmLastDigits : The last digits of the address must end with the digits provided by alignmentparam

  alignmentparam : String that holds the alignment parameter.
 
  isHexadecimalInput: When true this will handle the input field as a hexadecimal string else decimal
  isNotABinaryString: When true and the varType is vtBinary this will handle the input field as a decimal instead of a binary string
  isunicodescan: When true and the vartype is vtString this will do a unicode (utf16) string scan else normal utf8 string
  iscasesensitive : When true and the vartype is vtString this check if the case matches

    


memscan_nextScan(memscan, scanoption, roundingtype, input1,input2, isHexadecimalInput, isNotABinaryString, isunicodescan, iscasesensitive, ispercentagescan, savedresultname OPTIONAL);
  Does a next scan based on the current addresslist and values of the previous scan or values of a saved scan
  memscan: The MemScan object that has previously done a first scan
  scanoption:
    soExactValue: Exact Value scan
    soValueBetween: Value between scan
    soBiggerThan: Bigger than ... scan
    soSmallerThan: smaller than ... scan
    soIncreasedValue: Increased value scan
    soIncreasedValueBy: Increased value by scan
    soDecreasedValue: Decreased value scan
    soDecreasedValueBy: Decreased value by scan
    soChanged: Changed value scan
    soUnchanged: Unchanged value scan
  
  roundingtype: Defined the way scans for exact value floating points are handled
    rtRounded : Normal rounded scans. If exact value = "3" then it includes 3.0 to 3.49999999. If exact value is "3.0" it includes 3.00 to 3.0499999999
    rtTruncated: Truncated algoritm. If exact value = "3" then it includes 3.0 to 3.99999999. If exact value is "3.0" it includes 3.00 to 3.099999999
    rtExtremerounded: Rounded Extreme. If exact value = "3" then it includes 2.0000001 to 3.99999999. If exact value is "3.0" it includes 2.900000001 to 3.099999999
  
  input1: If required by the scanoption this is a string of the given variable type
  input2: If requires by the scanoption this is the secondary input

  isHexadecimalInput: When true this will handle the input field as a hexadecimal string else decimal
  isNotABinaryString: When true and the varType is vtBinary this will handle the input field as a decimal instead of a binary string
  isunicodescan: When true and the vartype is vtString this will do a unicode (utf16) string scan else normal utf8 string
  iscasesensitive : When true and the vartype is vtString this check if the case matches
  ispercentage: When true and the scanoption is of type soValueBetween, soIncreasedValueBy or soDecreasedValueBy will cause CE to do a precentage scan instead of a normal value scan
  savedResultName: String that holds the name of a saved result list that should be compared against. First scan is called "FIRST"

memscan_newscan(memscan) : Clears the current results
memscan_waitTillDone(memscan)
memscan_saveCurrentResults(memscan, name)
memscan_getAttachedFoundlist(memscan) : Returns a FoundList object if one is attached to this scanresults. Returns nil otherwise

6.2+:
memscan_returnOnlyOneResult(memscan, state): If set to true before you start a scan, this will cause the scanner to only return one result. Note that it does not work with a foundlist
memscan_getOnlyResult(memscan): Only works if returnOnlyOneResult is true. Returns nil if not found, else returns the address that was found (integer)



FoundList
The foundlist is an object that opens the current memscan's result file and provides an interface for reading out the addresses

createFoundList(memscan)
foundlist_initialize(foundlist)
foundlist_deinitialize(foundlist)
foundlist_getCount(foundlist)
foundlist_getAddress(foundlist, index) : Returns the address as a string
foundlist_getValue(foundlist, index)


Memoryview class: (Inheritance: Form->ScrollingWinControl->CustomControl->WinControl->Control->Component->Object)
memoryview_getDisassemblerView(memoryView): Returns the visual disassembler object on the memoryview window
memoryview_getHexadecimalView(memoryView): Returns the visual hexadecimal object on the memoryview window

Disassemblerview class: (Inheritance: Panel->CustomControl->WinControl->Control->Component->Object) 
  The visual disassembler used on the memory view window

disassemblerview_getSelectedAddress(disassemblerview)
disassemblerview_setSelectedAddress(disassemblerview, address)
disassemblerview_onSelectionChange(disassemblerview, function): function(disassemblerview, address, address2)

Hexadecimal class: (Inheritance: Panel->CustomControl->WinControl->Control->Component->Object) 
  The visual hexadecimal object used on the memory view window

hexadecimalview_getTopAddress(hexadecimalview)
hexadecimalview_setTopAddress(hexadecimalview, address)
hexadecimalview_onAddressChange(hexadecimalview, function): function(hexadecimalview, address)
hexadecimalview_onByteSelect(hexadecimalview, function): function(hexadecimalview, address, address2)


Thread Class: (Inheritance: Object)
createNativeThread(function) : 
  Executes the given function in another thread using the systems thread mechanism
  The function returns the Thread class object
  function declaration: function (Thread)


thread_freeOnTerminate(thread, state) : 
  When set to true the thread object will free itself when the function ends (default=true)
  Note: Use this only from inside the thread function as the thread might have already terminated and freed itself when called

thread_synchronize(thread, function) :
  Called from inside the thread. This wil cause the tread to get the main thread to execute the given function and wait for it to finish.
  Usually for gui access
  function ()

thread_waitfor(thread) : Waits for the given thread to finish



Structure class: (Inheritance: Object)

getStructureCount(): Returns the number of Global structures. (Global structures are the visible structures)
getStructure(index): Returns the Structure object at the given index

createStructure(name): Returns an empty structure object (Not yet added to the Global list. Call structure_addToGlobalStructureList manually)

structure_getName(structure)
structure_setName(structure,name)
structure_getSize(structure): Calculates the size of the structure
structure_getElementCount(structure): Returns the number of elements in this structure





structure_getElement(structure, index): Returns a structureElement object (Changing offsets can change the index)
structure_getElementByOffset(structure, offset): Returns a structureElement object where the specified offset is at least the requested offset


structure_addElement(structure): Adds a new blank structureElement and returns it
structure_autoGuess(structure, baseaddresstoguessfrom, offset, size)

structure_beginUpdate(structure): Call this when you want to make multiple updates to a structure. It will speed up the update process
structure_endUpdate(structure): Call this when done
structure_addToGlobalStructureList(structure): Add this to the list of structures for the user to select from. (Global structures will get saved to the table)
structure_removeFromGlobalStructureList(structure): Remove from the list of structures. 


StructureElement class: (Inheritance: Object)
structureElement_getOwnerStructure(se): Returns the structure this element belongs to
structureElement_getOffset(se): Returns the offset of this element
structureElement_setOffset(se, offset): Sets the offset of this element
structureElement_getName(se): Returns the name of this element
structureElement_setName(se, name): Sets the name of this element (tip: Leave blank if you only want to set the name of the variable)
structureElement_getVartype(se): Returns the variable type of this element (check Variable types in defines.lua)
structureElement_setVartype(se, vartype)
structureElement_getChildStruct(se)
structureElement_setChildStruct(se, structure)
structureElement_getChildStructStart(se)
structureElement_setChildStructStart(se, offset)
structureElement_getBytesize(se): Gets the bytesize of the element. Usually returns the size of the type, except for string and aob
structureElement_setBytesize(se, size): sets the bytesize for types that are affected (string, aob)





supportCheatEngine(attachwindow, hasclosebutton, width, height, position ,yoururl OPTIONAL, extraparameters OPTIONAL, percentageshown OPTIONAL): 
  Will show an advertising window which will help keep the development of Cheat Engine going.
  If you provide your own url it will be shown Up to 75% of the time. 

  attachwindow: Type=Form : The form that the ad is attached to
  hasclosebutton: Type=boolean : If true the window will have a border an a close button at top
  width, height: Type=integer : 
    The client width and height of the window.
    Prefered formats are : 120x600 , 160x600, 300x250, 468x60, 728x90  ,But you are free to use different formats

  Position: Type=integer/enum: The place of the window
    0=Top, 1=Right, 2=Bottom, 3=left

  Yoururl: Type=string: The url you want to show. When given instead of showing CE's ads 100% it will show your url up to 75%.
    You can use it for your own income, or for updating users about new versions of your trainer or whatever you feel like

  Extraparameters: Type=String :  are url request parameters you can add to the default parameters (e.g trainername=mytrainer for tracking purposes)  

  PercentageShown: You can change the default of 75% to a smaller value like 50%


fuckCheatEngine() : Removes the ad window if it was showing


Following are some more internal functions for Cheat Engine

dbk_initialize() : Returns true if the dbk driver is loaded in memory. False if it failed for whatever reason (e.g 64-bit and not booted with unsigned driver support)
dbk_useKernelmodeOpenProcess() : Switches the internal pointer of the OpenProcess api to dbk_OpenProcess
dbk_useKernelmodeProcessMemoryAccess() : Switches the internal pointer to the ReadProcessMemory and WriteProcessMemory apis to dbk_ReadProcessMemory and dbk_WriteProcessMemory
dbk_useKernelmodeQueryMemoryRegions() : Switches the internal pointer to the QueryVirtualMemory api to dbk_QueryVirtualMemory
dbk_getPEProcess(processid) : Returns the pointer of the EProcess structure of the selected processid
dbk_getPEThread(threadid) : Gets the pointer to the EThread  structure
dbk_readMSR(msr): Reads the msr
dbk_writeMSR(msr, msrvalue): Writes the msr
dbk_executeKernelMemory(address, parameter) : 
  Executes a routine from kernelmode (e.g a routine written there with auto assembler)
  parameter can be a value or an address. It's up to your code how it's handled


dbvm_initialize(offloados OPTIONAL) : Initializes the dbvm functions (dbk_initialize also calls this) offloados is a boolean that when set will offload the system onto dbvm if it's not yet running (and only IF the dbk driver is loaded)
dbvm_readMSR(msr): See dbk_readMSR
dbvm_writeMSR(msr, value): See dbk_writeMSR



onAPIPointerChange(function): Registers a callback when an api pointer is changed (can happen when the user clicks ok in settings, or when dbk_use*** is used. Does NOT happen when setAPIPointer is called)


setAPIPointer(functionid, address): Sets the pointer of the given api to the given address. The address can be a predefined address set at initialization by Cheat Engine, or an address you got from an autoassembler script or injected dll (When Cheat Engine itself was targeted)

functionid:
  0: OpenProcess
    Known compatible address defines:
      windows_OpenProcess
      dbk_OpenProcess
      
  1: ReadProcessMemory
    Known compatible address defines:
      windows_ReadProcessMemory
      dbk_ReadProcessMemory
      dbk_ReadPhysicalMemory
      dbvm_ReadPhysicalMemory

  2: WriteProcessMemory
    Known compatible address defines:
      windows_WriteProcessMemory
      dbk_WriteProcessMemory
      dbk_WritePhysicalMemory
      dbvm_WritePhysicalMemory


  3: VirtualQueryEx
    Known compatible address defines:
      windows_VirtualQueryEx
      dbk_VirtualQueryEx
      VirtualQueryExPhysical

Extra variables defined:
dbk_NtOpenProcess : Address of the NtOpenProcess implementation in DBK32


The dbvm_ addresses should only be used with auto assembler scripts injected into Cheat Engine
dbvm_block_interrupts  : Address of function dbvm_block_interrupts : DWORD; stdcall;  
dbvm_raise_privilege   : Address of function dbvm_raise_privilege : DWORD; stdcall;  
dbvm_restore_interrupts: Address of function dbvm_restore_interrupts : DWORD; stdcall;
dbvm_changeselectors   : Address of function dbvm_changeselectors(cs,ss,ds,es,fs,gs: dword): DWORD; stdcall; 


D3DHOOK_ functions:
The d3dhook functions provide a method to render graphics and text inside the game, as long as it is running in directx9, 10 or 11

d3dhook_initializeHook(textureandcommandlistsize, hookmessages):
  Hooks direct3d and allocates a buffer with given size for storage of for the rendercommand list

  hookmessages defines if you want to hook the windows message handler for the direct3d window. The d3dhook_onClick function makes use of that
  

  If no size is provided 16MB is used and hookmessages is true

  Note: You can call this only once for a process


d3dhook_beginUpdate() : Use this function when you intent to update multiple sprites,textcontainers or textures. Otherwise artifacts may occur (sprite 1 might be drawn at the new location while sprite 2 might still be at the old location when a frame is rendered)

d3dhook_endUpdate() : When done updating, call this function to apply the changes


d3dhook_getWidth(): Returns the width of the direct3d window. Note: At least one frame must have been rendered in the game for this to return anything useful
d3dhook_getHeight(): Returns the height of the direct3d window.  ""


d3dhook_setDisabledZBuffer(state): When true will disable the Z-Buffer (Depth testing)
d3dhook_setWireframeMode(state): When true will show objects in wireframe mode


d3dhook_setMouseClip(state): Requires HookMessages to be true. When true will keep the mouse cursor inside the game. (Handy for certain strategy games, that don't support windowed mode or multiple displays )


d3dhook_onClick(function):
  Registers a function to be called when clicked on an sprite (excluding the mouse)
  function definition: function d3dclick(d3dhook_spite, x,y)
    x and y are coordinates in the sprite object. If sprites overlap the highest zorder sprite will be given. It does NOT care if a transparent part is clicked or not
  
  Note: This can cause a slowdown in the game if there are a lot of sprites and you press the left button a lot


--]]




--[[






D3DHook_Texture Class (Inheritance: Object)
This class controls the texture in memory. Without a sprite to use it, it won't show

d3dhook_texture_getHeight(d3dhook_texture)
d3dhook_texture_getWidth(d3dhook_texture)
d3dhook_texture_loadTextureByPicture(d3dhook_texture, picture)
d3dhook_texture_getHeight(d3dhook_texture)
d3dhook_texture_getWidth(d3dhook_texture)


D3DHook_FontMap Class (Inheritance: D3DHook_Texture->Object)
A fontmap is a texture that contains extra data regarding the characters. This class is used by the textcontainer
Current implementation only supports 96 characters (character 32 to 127)
d3dhook_fontmap_changeFont(d3dhook_fontmap, font): Changes the fontmap to the selected font


D3DHook_RenderObject Class (Inheritance: Object
The renderobject is the abstract class used to control in what manner objects are rendered.
The sprite and TextContainer classed inherit from this

d3dhook_renderobject_getX(d3dhook_renderobject)
d3dhook_renderobject_setX(d3dhook_renderobject, x)
d3dhook_renderobject_getY(d3dhook_renderobject)
d3dhook_renderobject_setY(d3dhook_renderobject, y)
d3dhook_renderobject_getAlphablend(d3dhook_renderobject)
d3dhook_renderobject_setAlphablend(d3dhook_renderobject, x)
d3dhook_renderobject_getVisible(d3dhook_renderobject)
d3dhook_renderobject_setVisible(d3dhook_renderobject, x)
d3dhook_renderobject_getZOrder(d3dhook_renderobject)
d3dhook_renderobject_setZOrder(d3dhook_renderobject, x)


D3DHook_Sprite Class (Inheritance: D3DHook_RenderObject->Object)
A d3dhook_sprite class is a visible texture on the screen.

d3dhook_sprite_getWidth(d3dhook_sprite)
d3dhook_sprite_setWidth(d3dhook_sprite, width)
d3dhook_sprite_getHeight(d3dhook_sprite)
d3dhook_sprite_setHeight(d3dhook_sprite, height)
d3dhook_sprite_getTexture(d3dhook_sprite)
d3dhook_sprite_setTexture(d3dhook_sprite, d3dhook_texture)

D3Dhook_TextContainer Class (Inheritance: D3DHook_RenderObject->Object)
A d3dhook_sprite class draws a piece of text on the screen based on the used fontmap.
While you could use a texture with the text, updating a texture in memory is slow. So if you wish to do a lot of text updates, use a textcontainer

d3dhook_textcontainer_getFontMap(d3dhook_textcontainer)
d3dhook_textcontainer_setFontMap(d3dhook_textcontainer, d3dhook_fontmap)
d3dhook_textcontainer_getText(d3dhook_textcontainer)
d3dhook_textcontainer_setText(d3dhook_textcontainer, string)



d3dhook_createTexture(filename) : Returns a d3dhook_texture object
d3dhook_createTexture(picture, hasTransparency OPTIONAL, transparentColor OPTIONAL): Returns a d3dhook_texture object
  if the picture is not a transparent image the transparentcolor parameter can be used to make it transparent


d3dhook_createSprite(d3dhook_texture): returns a d3dhook_sprite object
d3dhook_createFontMap(font): Returns a d3dhook_fontmap object
d3dhook_createTextContainer(d3dhook_fontmap, x, y, text): Returns a d3dhook_textContainer object


getThreadList(List): fills a List object with the threadlist of the currently opened process. Format: Hexadecimal threadid-string)

getProcessList(List): Fills a List object with the processlist of the system. Format: %x- 






--]]