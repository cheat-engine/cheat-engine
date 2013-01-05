--This lua script gets loaded when Cheat Engine loads
--You can use this to define some often used functions and libraries you'd like to use

require("defines")

--[[

List of CE specific functions and variables:

TrainerOrigin : A variable that contains the path of the trainer that launched cheat engine (Only set when launched as a trainer)

getCEVersion(): Returns a floating point value specifying the version of cheat engine

fullAccess(address,size): Changes the protection of a block of memory to writable and executable

loadTable(filename, merge OPTIONAL): Loads a .ct or .cetrainer. If merge is provided and set to true it will not clear the old table
saveTable(filename, protect OPTIONAL): Saves the current table. If protect is provided and set to true and the filename has the .CETRAINER extension, it will protect it from reading normally

note: addresses can be strings, they will get interpreted by ce's symbolhandler

readBytes(address,bytecount, ReturnAsTable ) : returns the bytes at the given address. If ReturnAsTable is true it will return a table instead of multiple bytes
  Reads the bytes at the given address and returns a table containing the read out bytes
 
writeBytes(address, x,x,x,x,...) : Write the given bytes to the given address from a table
writeBytes(address, table) : Write the given bytes to the given address from a table 


readInteger(address) : Reads an integer from the specified address
readQword(address): Reads a 64-bit integer from the specified address
readFloat(address) : Reads a single precision floating point value from the specified address
readDouble(address) : Reads a double precision floating point value from the specified address
readString(address, maxlength, widechar OPTIONAL) : Reads a string till it encounters a 0-terminator. Maxlength is just so you won't freeze for too long, set to 6000 if you don't care too much. Set WideChar to true if it is encoded using a widechar formatting
writeInteger(address,value) : Writes an integer to the specified address. Returns true on success
writeQword(address, value): Write a 64-bit integer to the specified address
writeFloat(address,value) : Writes a single precision floating point to the specified address. Returns true on success
writeDouble(address,value) : Writes a double precision floating point to the specified address. Returns true on success
writeString(address,text, widechar OPTIONAL) : Write a string to the specified address. Returns true on success

readBytesLocal(address,bytecount, ReturnAsTable) : See readBytes but then it's for Cheat engine's memory
readIntegerLocal(address) : Reads an integer from the specified address in CE's memory
readQwordLocal(address) : Reads a 64-bit integer from the specified address in CE's memory
readFloatLocal(address) : Reads a single precision floating point value from the specified address in CE's memory
readDoubleLocal(address) : Reads a double precision floating point value from the specified address in CE's memory
readStringLocal(address, maxlength, widechar OPTIONAL)
writeIntegerLocal(address,value) : Writes an integer to the specified address in CE's memory. Returns true on success
writeQwordLocal(address,value) : Writes a 64-bit integer to the specified address in CE's memory. Returns true on success
writeFloatLocal(address,value) : Writes a single precision floating point to the specified address in CE's memory. Returns true on success
writeDoubleLocal(address,value) : Writes a double precision floating point to the specified address in CE's memory. Returns true on success
writeStringLocal(address,string, widechar OPTIONAL)
writeBytesLocal(address, x,x,x,x,...) : See writeBytes but then it's for Cheat Engine's memory
writeBytesLocal(address, table, , count) : See writeBytes but then it's for Cheat Engine's memory


ansiToUtf8(string): Converts a string in Ansi encoding to UTF8
utf8ToAnsi(string): Converts a string in UTF8 encoding to Ansi
Note: GUI components mainly show in UTF8, some other functions use Ansi, try to find out which ones...


getAddress(string, local OPTIONAL): returns the address of a symbol. Can be a modulename or an export. set Local to true if you wish to querry the symboltable of the ce process
getModuleSize(modulename): Returns the size of a given module (Use getAddress to get the base address)
reinitializeSymbolhandler(): reinitializes the symbolhandler. E.g when new modules have been loaded

errorOnLookupFailure(state): If set to true (default) address lookups in stringform will raise an error if it can not be looked up. This includes symbolnames that are not defined and pointers that are bad. If set to false it will return 0 in those cases
  (Useful for pointers that don't work 100% of the time)

generateAPIHookScript(address, addresstojumpto, addresstogetnewcalladdress OPT) : Generates an auto assembler script which will hook the given address when executed
autoAssemble(text, targetself OPTIONAL) : runs the auto assembler with the given text. Returns true on success (if targetself is set it will assemble into Cheat Engine itself)
showMessage(text) : shows a messagebox with the given text
messageDialog(text, type, buttons...) : pops up a messagebox with a specific icon/sound with the specified buttons (mbok, mbyes, ....)
sleep(milliseconds): pauses for the number of specified milliseconds (1000= 1 sec...)

getProcesslist(Strings): Fills a Strings inherited object with the processlist of the system. Format: %x-pidname
getThreadList(List): fills a List object with the threadlist of the currently opened process. Format: %x


function onOpenProcess(processid):
  If this function is defined it will be called whenever cheat engine opens a process. Note that the same process might be opened multiple times in a row


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

getTickCount() :  Returns the current tickcount since windows was started. Each tick is one millisecond
processMessages() :  Lets the main eventhandler process the new messages (allows for new button clicks)
integerToUserData(int):  Converts a given integer to a userdata variable
userDataToInteger(UserDataVar):  Converts a given userdata variable to an integer


writeToClipboard(text):  Writes the given text to the clipboard
readFromClipboard():  Reads the text from the clipboard

speedhack_setSpeed(speed)
injectDLL(filename): Injects a dll, and returns true on success

loadPlugin(dllnameorpath): Loads the given plugin. Returns nil on failure. On success returns a value of 0 or greater



registerCustomTypeLua(typename, bytecount, bytestovaluefunction, valuetobytesfunction)
  Registers a Custom type based on lua functions
  The bytes to value function should be defined as "function bytestovalue (b1,b2,b3,b4)" and return an integer as result
  The value to bytes function should be defined as "function valuetobytes (integer)" and return the bytes it should write


registerCustomTypeAutoAssembler(script)
  Registers a custom type based on an auto assembler script. The script must allocate an "ConvertRoutine" and "ConvertBackRoutine"

onAutoGuess(function) : 
  Registers an function to be called whenever autoguess is used to predict a variable type
  function override (address, ceguess): Return the variable type you want it to be. If no change, just return ceguess



closeCE() : just closes ce
hideAllCEWindows() : makes all normal ce windows invisible (e.g trainer table)
unhideMainCEwindow() : shows the main cheat engine window

getAutoAttachList(): returns the AutoAttach StringList object. It can be controlled with the stringlist_ routines (it's not recommended to destroy this list object)


AOBScan(x,x,x,x,...):
scans the currently opened process and returns a StringList object containing all the results. don't forget to free this list when done
Bytevalue of higher than 255 or anything not an integer will be seen as a wildcard
AOBScan(aobstring): see above but here you just input one string




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


beep() : Plays the fabulous beep/ping sound!


getFormCount() : Returns the total number of forms assigned to the main CE application
getForm(index): Returns the form at the specific index

getMemoryViewForm() : Returns the main memoryview form class object which can be accessed using the Form_ class methods and the methods of the classes it inherits from. There can be multiple memory views, but this will only find the original/base
getMainForm() : Returns the main form class object which can be accessed using the Form_ class methods and the methods of the classes it inherits from
getAddressList() : Returns the cheat table addresslist object
getFreezeTimer()
getUpdateTimer()


undefined property functions. Not all properties of all classes have been explicitly exposed to lua, but if you know the name of a property of a specific class you can still access them (assuming they are declared as published in the pascal class declaration)
getPropertyList(class) : Returns a stringlist object containing all the published properties of the specified class (free the list when done) (Note, not all classed with properties have 'published' properties. E.g: stringlist)
setProperty(class, propertyname, propertyvalue) : Sets the value of a published property of a class (Won't work for method properties)
getProperty(class, propertyname) : Gets the value of a published property of a class (Won't work for method properties)
setMethodProperty(class, propertyname, function): Sets the method property to the specific function
getMethodProperty(Class, propertyname): Returns a function you can use to call the original function










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


function debugger_onModuleLoad(modulename, baseaddress) : 
this routine is called when a module is loaded. Only works for the windows debugger
return 1 if you want to cause the debugger to break


Changing registers:
When the debugger is waiting to continue you can change the register variables. When you continue those register values will be set in the thread's context


detachIfPossible() : Detaches the debugger from the target process (if it was attached)




class helper functions
inheritsFromObject(class): Returns true if given any class
inheritsFromComponent(class): Returns true if the given object inherits from the Component class
inheritsFromControl(class): Returns true if the given object inherits from the Control class
inheritsFromWinControl(class): Returns true if the given object inherits from the WinControl class


Class definitions
Object class: (Inheritance: )
Properties:
  ClassName: String - The name of the structure (Read only)
Methods:
  getClassName(): Returns the classname
  destroy(): Destroys the object



Component Class: (Inheritance: Object)
properties
  ComponentCount: Integer - Number of child components . Readonly
  Component[int]: Component - Array containing the child components. Starts at 0. Readonly
  ComponentByName[string]: Component - Returns a component based on the name. Readonly
  Name: string - The name of the component
  Tag: integer - Free to use storage space. (Usefull for id's)
  Owner: Component - Returns the owner of this object. Nil if it has none

methods
  getComponentCount() : Returns the number of components attached to his component
  getComponent(index) : Returns the specific component
  findComponentByName(name) : Returns the component with this name
  getName() : Return the name
  setName(newname) : Changes the name
  getTag() : Sets an integer value. You can use this for ID's
  setTag(tagvalue) : Get the tag value
  getOwner() : Returns the owner of this component



Control Class: (Inheritance: Component->Object)
properties:
  Caption: string - The text of a control
  Top : integer - The x position
  Left : integer - The y position
  Width : integer - The width of the control
  Height : integer - The height of the control
  Align: AlignmentOption - Alignment of the control
  Enabled: boolean - Determines if the object is usable or greyed out
  Visible: boolean - Determines if the object is visible or not
  Color: ColorDefinition/RGBInteger - The color of the object. Does not affect the caption
  Parent: WinControl - The owner of this control
  PopupMenu: PopupMenu - The popup menu that shows when rightclicking the control
  Font: Font - The font class associated with the control
  OnClick: function - The function to call when a button is pressed
  

methods:
  getLeft()
  setLeft(integer)
  getTop()
  setTop(integer)
  getWidth()
  setWidth(integer)
  getHeight()
  setHeight()
  setCaption(caption) : sets the text on a control. All the gui objects fall in this category
  getCaption() : Returns the text of the control
  setPosition(x,y): sets the x and y position of the object base don the top left position (relative to the client array of the owner object)
  getPosition(): returns the x and y position of the object (relative to the client array of the owner object)
  setSize(width,height) : Sets the width and height of the control
  getSize() : Gets the size of the control
  setAlign(alignmentoption): sets the alignment of the control
  getAlign(alignmentoption): gets the alignment of the control
  getEnabled() : gets the enabled state of the control
  setEnabled(boolean) : Sets the enabled state of the control
  getVisible() : gets the visible state of the control
  setVisible(boolean) : sets the visible state of the control
  getColor() : gets the color
  setColor(rgb) : Sets the color
  getParent() : Returns nil or an object that inherits from the Wincontrol class
  setParent(wincontrol) : Sets the parent for this control
  getPopupMenu()
  setPopupMenu()
  getFont():  Returns the Font object of this object
  setFont():  Assigns a new font object. (Not recommended to use. Change the font object that's already there if you wish to change fonts)
  setOnClick(functionnameorstring) : Sets the onclick routine
  getOnClick(): Gets the onclick function
  doClick():  Executes the current function under onClick

GraphicsObject : (GraphicsObject->Object)



Region Class : (Region->GraphicsObject->Object)
createRegion(): Created an empty region

properties
-
methods
  addRectangle(x1, y1, x2, y2): Adds a rectangle to the region
  addPolygon(tablewithcoordinates): Adds an array of 2D locations. (example : {{0,0},{100,100}, {0,100}} for a triangle )



WinControl Class: (Inheritance: Control->Component->Object)
properties
  ControlCount : integer - The number of child controls of this wincontrol
  Control[] : Control - Array to access a child control
  OnEnter : function - Function to be called when the WinControl gains focus
  OnExit : function - Function to be called when the WinControl loses focus

methods
  getControlCount()  Returns the number of Controls attached to this class
  getControl(index) : Returns a WinControl class object
  getControlAtPos(x,y):  Gets the control at the given x,y position relative to the wincontrol's position 
  canFocus(): returns true if the object can be focused
  focused(): returns boolean true when focused
  setFocus(): tries to set keyboard focus the object
  setShape(Region): Sets the region object as the new shape for this wincontrol
  setShape(Bitmap): 
  setOnEnter(function) : Sets an onEnter event. (Triggered on focus enter)
  getOnEnter()
  setOnExit(function) : Sets an onExit event. (Triggered on lost focus)
  getOnExit()


MenuItem class(Inheritance: Component->Object)
createMenuItem(ownermenu) : Creates a menu item that gets added to the owner menu

properties
  Caption : String - Text of the menu item
  Shortcut : string - Shortcut in textform to trigger the menuitem
  Count : integer - Number of children attached to this menuitem
  Item[] : Array to access each child menuitem
  OnClick: Function to call when the menu item is activated

methods
  getCaption(menuitem) : Gets the caption of the menu item
  setCaption(menuitem, caption) : Sets the caption of the menu item
  getShortcut(menuitem): Returns the shortcut for this menu item
  setShortcut(menuitem, shortcut): Sets the shortcut for this menuitem. A shortcut is a string in the form of ("ctrl+x")
  getCount(menuitem)
  getItem(index) : Returns the menuitem object at the given index
  add(menuitem) : Adds a menuItem as a submenu item
  insert(index, menuitem): Adds a menuItem as a submenu item at the given index
  delete(index)
  setOnClick(function) : Sets an onClick event
  getOnClick()
  doClick(): Executes the onClick method if one is assigned



Menu Class: (Inheritance: Component->Object)
properties
  Items : MenuItem - The base MenuItem class of this menu (readonly)
methods
  getItems() : Returns the main MenuItem of this Menu

MainMenu Class: (Inheritance: Menu->Component->Object)
createMainMenu(form)
  The mainmenu is the menu at the top of a window

PopupMenu Class: (Inheritance: Menu->Component->Object)
createPopupMenu(owner)
  The popup menu is the menu that pops up when showing the (rightclick) context of an control


Strings Class: (Inheritance : Object) (Mostly an abstract class)
properties
  Text : String - All the strings in one string
  String[]: String - Array to access one specific string in the list
  [] = String[]

methods
  clear() : Deletes all strings in the list
  add(string) : adds a string to the list
  delete(index) : Deletes a string from the list
  getText() : Returns all the strings as one big string
  setText() : Sets the strings of the given strings object to the given text (can be multiline)
  indexOf(string): Returns the index of the specified string. Returns -1 if not found
  insert(index, string): Inserts a string at a specific spot moving the items after it

  getCount(): Returns the number is strings in the list
  remove(string); Removes the given string from the list
  loadFromFile(filename) : Load the strings from a textfile
  saveToFile(filename) : Save the strings to a textfile

  getString(index) : gets the string at the given index
  setString(index, string) : Replaces the string at the given index



Stringlist Class: (Inheritance : Strings->Object)
createStringlist() : Creates a stringlist class object (for whatever reason, lua strings are probably easier to use)

properties
  Duplicates : DuplicatesType - Determines how duplicates should be handled
  Sorted : boolean - Determines if the list should be sorted
  CaseSensitive: boolean - Determines if the list is case sensitive or not.

methods
  getDuplicates() : returns the duplicates property
  setDuplicates(Duplicates) : Sets the duplicates property (dupIgnore, dupAccept, dupError)
  getSorted() : returns true if the list has the sorted property
  setSorted(boolean) : Sets the sorted property
  getCaseSensitive() : Returns true if the case sensitive property is set
  setCaseSensitive(boolean): Sets the case sensitive property


Form Class: (Inheritance: ScrollingWinControl->CustomControl->WinControl->Control->Component->Object)
properties
  OnClose: function - The function to call when the form gets closed
  Menu: MainMenu - The main menu of the form

methods
  centerScreen(); : Places the form at the center of the screen
  hide() : Hide the form
  show() : show the form
  close():  Closes the form. Without an onClose this will be the same as hide 
  showModal() : show the form and wait for it to close and get the close result
  isForegroundWindow(): returns true if the specified form has focus
  setOnClose(function)  : function (sender) : Return a CloseAction to determine how to close the window
  getOnClose() : Returns the function
  getMenu() : Returns the mainmenu object of this form
  setMenu(mainmenu)

  setBorderStyle( borderstyle):  Sets the borderstyle of the window
  getBorderStyle()

  printToRasterImage(rasterimage): Draws the contents of the form to a rasterimage class object
  dragNow():  Call this on mousedown on any object if you wish that the mousemove will drag the whole form arround. Useful for borderless windows (Dragging will stop when the mouse button is released)



CEForm Class: (Inheritance: Form->ScrollingWinControl->CustomControl->WinControl->Control->Component->Object)
createForm(visible OPT): creates a CEForm class object(window) and returns the pointer for it. Visible is default true but can be changed
createFormFromFile(filename): Returns the generated CEform

properties
  DoNotSaveInTable: boolean - Set this if you do not wish to save the forms in the table
methods
  saveToFile(form, filename): Saves a userdefined form
  getDoNotSaveInTable(form): Returns the DoNotSaveInTable property
  setDoNotSaveInTable(form, boolean): Sets the DoNotSaveInTable property


GraphicControl Class: (Inheritance: Control->Component->Object)
properties
  Canvas: Canvas - The canvas for rendering this control

methods
  getCanvas() : Returns the Canvas object for the given object that has inherited from customControl


Label Class: (Inheritance: GraphicControl->Control->Component->Object)
createLabel(owner): Creates a Label class object which belongs to the given owner. Owner can be any object inherited from WinControl


Splitter Class: (Inheritance: CustomControl->WinControl->Control->Component->Object)
createSplitter(owner): Creates a Splitter class object which belongs to the given owner. Owner can be any object inherited from WinControl


Panel Class: (Inheritance: CustomControl->WinControl->Control->Component->Object)
createPanel(owner): Creates a Panel class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Alignment: alignment
  BevelInner: panelBevel
  BevelOuter: panelBevel
  BevelWidth: Integer
  FullRepaint: boolean
methods
  getAlignment() : gets the alignment property
  setAlignment(alignment) : sets the alignment property
  getBevelInner() 
  setBevelInner(PanelBevel)
  getBevelOuter()
  setBevelOuter(PanelBevel) 
  getBevelWidth()
  setBevelWidth(BevelWidth)
  getFullRepaint()
  setFullRepaint(boolean)



Image Class: (Inheritance: GraphicControl->Control->Component->Object)
createImage(owner): Creates an Image class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Canvas: Canvas - The canvas object to access the picture of the image
  Transparent: boolean - Determines if some parts of the picture are see through (usually based on the bottomleft corner)
  Stretch: boolean - Determines if the picture gets stretched when rendered in the image component
  Picture: Picture - The picture to render

methods
  loadImageFromFile(filename)
  getStretch()
  setStretch(boolean)
  getTransparent()
  setTransparent(boolean)
  getCanvas()
  setPicture(picture)
  getPicture() : Returns the Picture object of this image


Edit Class: (Inheritance: WinControl->Control->Component->Object)
createEdit(owner): Creates an Edit class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Text: string - The current contents of the editfield
  OnChange: function - The function to call when the editfield is changed

methods
  clear()
  selectAll()
  clearSelection()
  copyToClipboard()
  cutToClipboard()
  pasteFromClipboard()
  onChange(function)


Memo Class: (Inheritance: Edit->WinControl->Control->Component->Object)
createMemo(owner): Creates a Memo class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Lines: Strings - Strings object for this memo
  WordWrap: boolean - Set if words at the end of the control should go to the next line
  WantTabs: Boolean - Set if tabs will add a tab to the memo. False if tab will go to the next control
  WantReturns: Boolean - Set if returns will send a event or not
  Scrollbars: Scrollstyle - Set the type of ascrollbars to show (ssNone, ssHorizontal, ssVertical, ssBoth,
    ssAutoHorizontal, ssAutoVertical, ssAutoBoth)


methods
  append(string)
  getLines() : returns a Strings class
  getWordWrap()
  setWordWrap(boolean)
  getWantTabs()
  setWantTabs(boolean)
  getWantReturns()
  setWantReturns(boolean)
  getScrollbars()
  setScrollbars(scrollbarenumtype) : 
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

properties
  ModalResult: ModalResult - The result this button will give the modalform when clicked

methods
  getModalResult(button)
  setModalResult(button, mr)

CheckBox Class: (Inheritance: ButtonControl->WinControl->Control->Component->Object)
createCheckBox(owner): Creates a CheckBox class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Checked: boolean - True if checked
  AllowGrayed: boolean - True if it can have 3 states. True/False/None
  State: boolean - The state. (cbUnchecked=0, cbChecked=1, cbGrayed=2)
  OnChange: function - Function to call when the state it changed

methods  
  getAllowGrayed(CheckBox)
  setAllowGrayed(CheckBox, boolean)
  getState(checkbox): Returns a state for the checkbox. (cbUnchecked, cbChecked, cbGrayed)
  setState(checkbox, boolean): Sets the state of the checkbox
  onChange(checkbox, function)

ToggleBox Class: (Inheritance: CheckBox->ButtonControl->WinControl->Control->Component->Object)
createToggleBox(owner): Creates a ToggleBox class object which belongs to the given owner. Owner can be any object inherited from WinControl

GroupBox Class: (Inheritance: WinControl->Control->Component->Object)
createGroupBox(owner): Creates a GroupBox class object which belongs to the given owner. Owner can be any object inherited from WinControl


RadioGroup class: (Inheritance: GroupBox->WinControl->Control->Component->Object)
createRadioGroup(owner): Creates a RadioGroup class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Items: Strings - Strings derived object containings all the items in the list
  Columns: Integer - The number of columns to split the items into
  ItemIndex: Integer - The currently selected item
  OnClick: Called when the control is clicked

methods
  getRows(): Returns the number of rows
  getItems(): Returns a Strings object
  getColumns(): Returns the nuber of columns
  setColumns(integer)
  getItemIndex()
  setItemIndex(integer)
  setOnClick(function)
  getOnClick()


ListBox Class: (Inheritance: WinControl->Control->Component->Object) 
createListBox(owner): Creates a ListBox class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Items: Strings - Strings derived object containings all the items in the list
  ItemIndex: integer - Get selected index. -1 is nothing selected
  Canvas: Canvas - The canvas object used to render on the object

methods
  clear()
  getItems(): Returns a strings object
  setItems(Strings): sets a strings object to the listbox
  getItemIndex()
  setItemIndex(integer)
  getCanvas()


ComboBox Class: (Inheritance: WinControl->Control->Component->Object)
createComboBox(owner): Creates a ComboBox class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Items: Strings - Strings derived object containings all the items in the list
  ItemIndex: integer - Get selected index. -1 is nothing selected
  Canvas: Canvas - The canvas object used to render on the object
  
methods
  clear()
  getItems()
  setItems()
  getItemIndex()
  setItemIndex(integer)
  getCanvas()



ProgressBar Class: (Inheritance: WinControl->Control->Component->Object)
createProgressBar(owner): Creates a ProgressBar class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Min: integer - The minimum positionvalue the progressbar can have (default 0)
  Max: integer - The maximum positionvalue the progressbar can have (default 100
  Position: integer - The position of the progressbar
  Step: integer- The stepsize to step by when stepIt() is called

methods
  stepIt() - Increase position with "Step" size
  stepBy(integer) - increase the position by the given integer value
  getMax() - returns the Max property
  setMax(integer) - sets the max property
  getMin() - returns the min property
  setMin(integer)- sets the min property
  getPosition() - returns the current position
  setPosition(integer) - sets the current position




TrackBar Class : (Inheritance: WinControl->Control->Component->Object)
createTrackBar(owner): Creates a TrackBar class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Min: integer - Minimal value for the trackbar
  Max: integer - Maximum value for the trackbar
  Position: integer - The current position
  OnChange: function - Function to call when 

methods
  getMax()
  setMax(integer)
  getMin(trackbar)
  setMin(trackbar, integer)
  getPosition(progressbar)
  setPosition(progressbar, integer)
  getOnChange(function)
  setOnChange()


CollectionItem Class: (Inheritance: Object)
Base class for some higher level classes. Often used for columns

properties
  ID: integer
  Index: integer - The index in the array this item belong to
  DisplayName: string

methods
  getID()
  getIndex()
  setIndex()
  getDisplayName()
  setDisplayName()





ListColumn class: (Inheritance: CollectionItem->Object)
properties
  AutoSize: boolean
  Caption: string
  MaxWidth: integer
  MinWidth: integer
  Width: integer
methods
  getAutosize()
  setAutosize(boolean)
  getCaption()
  setCaption(caption)
  getMaxWidth()
  setMaxWidth(width)
  getMinWidth()
  setMinWidth(width)
  getWidth()
  setWidth(width)


Collection Class: (Inheritance: TObject)

properties
  Count: integer

methods
  clear(collection)
  getCount(collection)
  delete(collection, index)


ListColumns class : (Inheritance: Collection->Object)
properties
  Columns[]: Array to access a column
  [] = Columns[]

methods
  add(): Returns a new ListColumn object
  getColumn(index): Returns a ListColum object;
  setColumn(index, listcolumns): Sets a ListColum object (not recomended, use add instead)

ListItem Class : (Inheritance: TObject)
properties
  Caption: boolean - The text of this listitem
  Checked: boolean - Determines if the checkbox is checked (if it has a checkbox)
  SubItems: Strings - The Strings object that hold the subitems
  Selected: boolean - Returns true if selected
  Index: integer - The index in the Items object of the owner of this listitem (readonly)
  Owner: ListItems - The ListItems object that owns this ListItem (readonly)

methods
  delete()
  getCaption() : Returns the first columns string of the listitem
  setCaption(string) : Sets the first column string of the listitem
  getChecked() : Returns true if the listitem is checked
  setChecked(boolean): Sets the checkbox of the listbox to the given state
  getSubItems(): Returns a Strings object


ListItems class : (Inheritance: TObject)
properties
  Count : Integer - The number of ListItems this object holds
  Item[]: ListItem[] - Array to access each ListItem object
  [] = Item[]
methods
  clear()
  getCount()
  getItem(integer) : Return the listitem object at the given index
  add(): Returns a new ListItem object



Listview Class : (Inheritance: WinControl->Control->Component->Object)
createListView(owner): Creates a ListView class object which belongs to the given owner. Owner can be any object inherited from WinControl

properties
  Columns: ListColumns - The Listcolumns object of the listview (Readonly)
  Items: ListItems - The ListItems objects of the listview
  ItemIndex: integer - The currently selected index in the Items object 
  Canvas: Canvas - The canvas object used to render the listview  (Readonly)

methods
  clear()
  getColumns() : Returns a ListColumns object
  getItems(): Returns a ListItems object
  getItemIndex()
  setItemIndex(index)
  getCanvas()


Timer Class : (Inheritance: Component->object)
createTimer(owner, enabled OPT): 
  Creates a timer object. If enabled is not given it will be enabled by default (will start as soon as an onTimer event has been assigned)
  Owner may be nil, but you will be responsible for destroying it instead of being the responsibility of the owner object)

properties
  Interval: integer - The number of milliseconds (1000=1 second) between executions
  Enabled: boolean
  OnTimer: function - The function to call when the timer triggers

methods
  getInterval()
  setInterval(interval) : Sets the speed on how often the timer should trigger. In milliseconds (1000=1 second)
  getOnTimer()
  setOnTimer(function)
  getEnabled()
  setEnabled()boolean)

CustomControl class (CustomControl->WinControl->Control->Component->Object)
properties
  Canvas : The canvas object for drawing on the control/. Readonly
methods
  getCanvas() : Returns the Canvas object for the given object that has inherited from customControl


Canvas Class : (Inheritance: CustomCanvas->Object)
properties
  Brush: Brush - The brush object
  Pen: Pen - The pen object
  Font: Font - The font object
  Width: integer - Width of the canvas
  Height: integer - Height of the canvas  


methods
  getBrush(): Returns the brush object of this canvas
  getPen(): Returns the pen object of this canvas
  getFont(): Returns the font object of this canvas
  getWidth()
  getHeight()
  getPenPosition()
  setPenPosition(x,y)
  line(sourcex, sourcey, destinationx, destinationy)
  lineTo(destinationx, destinationy)
  rect(x1,y1,x2,y2)
  fillRect(x1,y1,x2,y2)
  textOut(x,y, text)
  getTextWidth(text)
  getTextHeight(text)
  getPixel(x,y)
  setPixel(x,y,color)
  floodFill(x,y)
  ellipse(x1,y1,x2,y2)
  gradientFill(x1,y1,x2,y2, startcolor, stopcolor, direction) : Gradient fills a rectangle. Direction can be 0 or 1. 0=Vertical 1=Horizontal
  copyRect(dest_x1,dest_y1,dest_x2,dest_y2, sourceCanvas, source_x1,source_y1,source_x2,source_y2)
  draw(x,y, graphic) : Draw the image of a specific Graphic class

Pen Class : (Inheritance: CustomPen->CanvasHelper->Object)
properties
  Color: Integer - The color of the pen
  Width: integer - Thickness of the pen
methods
  getColor()
  setColor(color)
  getWidth()
  setWidth(width)


Brush Class : (Inheritance: CustomBrush->CanvasHelper->Object)
properties
  Color : Integer
methods  
  getColor()
  setColor()

Font Class : (Inheritance: CustomFont->CanvasHelper->Object)
createFont(): Returns a font object
font_getName(font): Gets the fontname of the font
font_setName(font): Sets the fontname of the font
font_getSize(font): Gets the size of the font
font_setSize(font): Sets the size of the font
font_getColor(font): Gets the color of the font
font_setColor(font): Sets the color of the font


Graphic Class : (Inheritance: Object) : Abstract class
graphic_getWidth(graphic): Gets the current width in pixels of this graphics object
graphic_setWidth(graphic, width): Sets thw width in pixels
graphic_getHeight(graphic)
graphic_setHeight(graphic, height)

RasterImage class: (Inheritance: Graphic->Object) : Base class for some graphical controls
rasterimage_getCanvas(RasterImage): Returns the Canvas object for this image
rasterimage_getPixelFormat((rasterimage):  Returns the current pixelformat
rasterimage_setPixelFormat(rasterimage, pixelformat):  Sets the pixelformat for this image. Will clear the current image if it had one. Supported pixelformats: pf1bit, pf4bit, pf8bit, pf15bit, pf16bit, pf24bit, pf32bit (recommended)

rasterimage_setTransparent(rasterimage,state):  Will set the image to support transparency or not
rasterimage_getTransparent(rasterimage):  Returns true if the image supports transparency
rasterimage_setTransparentColor(rasterimage, color): Sets the color that will be rendered as transparent when drawn
rasterimage_getTransparentColor(rasterimage):  Returns the color set to be transparent


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




GenericHotkey Class : (Inheritance:  Object)
createHotkey(function, key, ...) : returns an initialized GenericHotkey class object. Maximum of 5 keys
generichotkey_setKeys(hotkey, key, ....)
generichotkey_onHotkey(hotkey, function)


OpenDialog Class: (Inheritance: FileDialog->CommonDialog->Component->Object)
openDialog_execute(openDialog): Shows the dialog and returns the string to the selected file

SaveDialog Class: (Inheritance: OpenDialog->FileDialog->CommonDialog->Component->Object)

 
MemoryStream Class (Inheritance: Stream->Object)
FileStream Class (Inheritance: HandleStream->Stream->Object)


TableFile class (Inheritance: Object)
tablefile_saveToFile(tablefile, filename)
tablefile_getData(tablefile, filename) : Gets a MemoryStream object 


findTableFile(filename): Returns the TableFile class object for the saved file

xmplayer_playXM(filename, OPTIONAL noloop)
xmplayer_playXM(tablefile, OPTIONAL noloop)
xmplayer_playXM(Stream, OPTIONAL noloop)
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


memoryrecord_string_getSize(te): 
memoryrecord_string_setSize(te, integer): 
memoryrecord_string_getUnicode(te): 
memoryrecord_string_setUnicode(te, boolean): 
memoryrecord_binary_getStartbit(te): 
memoryrecord_binary_setStartbit(te, integer): 
memoryrecord_binary_getSize(te): 
memoryrecord_binary_setSize(te, integer): 
memoryrecord_aob_getSize(te): 
memoryrecord_aob_setSize(te, integer): 


memoryrecord_isSelected(te):  Returns true or false depending on if it's currently selected or not
memoryrecord_delete(te) : It's unknown what this function does, all that is known is that after using this command other memrec routines with this table entry value don't work anymore...


Addresslist Class:
addresslist_getCount(addresslist)
addresslist_getMemoryRecord(addresslist, index)
addresslist_getMemoryRecordByDescription(addresslist, description): getTableEntry(descriptionname): returns a tableEntry pointer for use with memrec functions
addresslist_getMemoryRecordByID(addresslist, ID)

addresslist_createMemoryRecord(addresslist) : createTableEntry: creates an generic cheat table entry and add it to the list. Returns a tableentry pointer you can use with memrec routines

addresslist_getSelectedRecords(Addresslist):  Returns a table of all the selected records

addresslist_doDescriptionChange(addresslist) : Will show the gui window to change the description of the selected entry
addresslist_doAddressChange(addresslist) : Will show the gui window to change the address of the selected entry
addresslist_doTypeChange(addresslist) : Will show the gui window to change the type of the selected entries
addresslist_doValueChange(addresslist) : Will show the gui window to change the value of the selected entries

addresslist_getSelectedRecord(addresslist) : Gets the main selected memoryrecord
addresslist_setSelectedRecord(addresslist, memrec) : Sets the currently selected memoryrecord. This will unselect all other entries





registerSymbol(symbolname, address, OPTIONAL donotsave): Registers a userdefined symbol. If donotsave is true this symbol will not get saved when the table is saved
unregisterSymbol(symbolname)

getNameFromAddress(address): Returns the given address as a string. Registered symbolname, modulename+offset, or just a hexadecimal string depending on what address
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
    vtGrouped
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



Structure class related functions:
getStructureCount(): Returns the number of Global structures. (Global structures are the visible structures)
getStructure(index): Returns the Structure object at the given index
createStructure(name): Returns an empty structure object (Not yet added to the Global list. Call structure_addToGlobalStructureList manually)


structure class: (Inheritance: Object)
Properties:
  Name: String - The name of the structure
  Size: Integer - The number of bytes between the last element and the start. ReadOnly
  Count: Integer - Number of elements in the structure. ReadOnly
  Element[]: structureElement - Returns the structure element at the given index. Readonly
Methods:
  getName(): Returns the name
  setName(name): Sets the name
  getElement(index): Returns a structureElement object (Changing offsets can change the index)
  getElementByOffset(offset): Returns a structureElement object where the specified offset is at least the requested offset
  addElement(): Adds a new blank structureElement and returns it
  autoGuess(baseaddresstoguessfrom, offset, size)

  beginUpdate(): Call this when you want to make multiple updates to a structure. It will speed up the update process
  endUpdate(): Call this when done
  addToGlobalStructureList(): Add this to the list of structures for the user to select from. (Global structures will get saved to the table)
  removeFromGlobalStructureList(): Remove from the list of structures. 


StructureElement class: (Inheritance: Object)
Properties:
  Owner: structure - The structure this element belongs to. Readonly
  Offset: integer - The offset of this element
  Name: string - The name of this element
  Vartype: integer - The variable type of this element
  ChildStruct: structure - If not nil this element if a pointer to the structure defined here
  ChildStructStart: integer - The number of bytes inside the provided childstruct. (E.g: It might point to offset 10 of a certain structure)
  Bytesize: integer - The number of bytes of this element. Readonly for basic types, writable for types that require a defined length like strings and array of bytes

Methods:
  getOwnerStructure(): Returns the structure this element belongs to
  getOffset(): Returns the offset of this element
  setOffset(offset): Sets the offset of this element
  getName(): Returns the name of this element
  setName(name): Sets the name of this element (tip: Leave blank if you only want to set the name of the variable)
  getVartype(): Returns the variable type of this element (check Variable types in defines.lua)
  setVartype(vartype)
  getChildStruct()
  setChildStruct(structure)
  getChildStructStart()
  setChildStructStart(offset)
  getBytesize(): Gets the bytesize of the element. Usually returns the size of the type, except for string and aob
  setBytesize(size): sets the bytesize for types that are affected (string, aob)





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

d3dhook_initializeHook(textureandcommandlistsize OPTIONAL, hookmessages OPTIONAL):
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

d3dhook_enableConsole(virtualkey): Adds a (lua)console to the specific game. The given key will bring it up (0xc0=tilde)


d3dhook_onClick(function):
  Registers a function to be called when clicked on an sprite (excluding the mouse)
  function definition: function d3dclick(d3dhook_spite, x,y)
    x and y are coordinates in the sprite object. If sprites overlap the highest zorder sprite will be given. It does NOT care if a transparent part is clicked or not
  
  Note: This can cause a slowdown in the game if there are a lot of sprites and you press the left button a lot

d3dhook_onKey(function)
  function(vkey, char) : boolean  . Return false if you do not wish this key event to pass down to the game
  



D3DHook_Texture Class (Inheritance: Object)
This class controls the texture in memory. Without a sprite to use it, it won't show


d3dhook_createTexture(filename) : Returns a d3dhook_texture object
d3dhook_createTexture(picture, transparentColor OPTIONAL): Returns a d3dhook_texture object
  if the picture is not a transparent image the transparentcolor parameter can be used to make one of it's colors transparent

d3dhook_texture_getHeight(d3dhook_texture)
d3dhook_texture_getWidth(d3dhook_texture)
d3dhook_texture_loadTextureByPicture(d3dhook_texture, picture)



D3DHook_FontMap Class (Inheritance: D3DHook_Texture->Object)
A fontmap is a texture that contains extra data regarding the characters. This class is used by the textcontainer
Current implementation only supports 96 characters (character 32 to 127)

d3dhook_createFontmap(font): Returns a d3dhook_fontmap object
d3dhook_fontmap_changeFont(d3dhook_fontmap, font): Changes the fontmap to the selected font
d3dhook_fontmap_getTextWidth(d3dhook_fontmap, string): Returns the width of the given string in pixels


D3DHook_RenderObject Class (Inheritance: Object)
The renderobject is the abstract class used to control in what manner objects are rendered.
The sprite and TextContainer classed inherit from this


d3dhook_renderobject_getX(d3dhook_renderobject): Gets the x coordinate of the object. Floating point
d3dhook_renderobject_setX(d3dhook_renderobject, x): Returns the x coordinate of the object
d3dhook_renderobject_getY(d3dhook_renderobject): Sets the y coordinate of the object
d3dhook_renderobject_setY(d3dhook_renderobject, y): Returns the y coordinate of the object

d3dhook_renderobject_getAlphablend(d3dhook_renderobject): Returns the current alphablend value. 1.0 is fully visible and 0.0=invisible
d3dhook_renderobject_setAlphablend(d3dhook_renderobject, x): Sets the alphablend value.
d3dhook_renderobject_getVisible(d3dhook_renderobject)
d3dhook_renderobject_setVisible(d3dhook_renderobject, x)
d3dhook_renderobject_getZOrder(d3dhook_renderobject)
d3dhook_renderobject_setZOrder(d3dhook_renderobject, x)


D3DHook_Sprite Class (Inheritance: D3DHook_RenderObject->Object)
A d3dhook_sprite class is a visible texture on the screen.

d3dhook_createSprite(d3dhook_texture): returns a d3dhook_sprite object
d3dhook_sprite_getWidth(d3dhook_sprite)
d3dhook_sprite_setWidth(d3dhook_sprite, width)
d3dhook_sprite_getHeight(d3dhook_sprite)
d3dhook_sprite_setHeight(d3dhook_sprite, height)
d3dhook_sprite_getTexture(d3dhook_sprite): Returns a d3dhook_texture object
d3dhook_sprite_setTexture(d3dhook_sprite, d3dhook_texture): Sets the texture to render with this sprite (width and height will get reset)


D3Dhook_TextContainer Class (Inheritance: D3DHook_RenderObject->Object)
A d3dhook_sprite class draws a piece of text on the screen based on the used fontmap.
While you could use a texture with the text, updating a texture in memory is slow. So if you wish to do a lot of text updates, use a textcontainer


d3dhook_createTextContainer(d3dhook_fontmap, x, y, text): Returns a d3dhook_textContainer object
d3dhook_textcontainer_getFontMap(d3dhook_textcontainer)
d3dhook_textcontainer_setFontMap(d3dhook_textcontainer, d3dhook_fontmap)
d3dhook_textcontainer_getText(d3dhook_textcontainer)
d3dhook_textcontainer_setText(d3dhook_textcontainer, string)
--]]

