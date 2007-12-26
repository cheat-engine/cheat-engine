@echo off
command /C echo changing to dos-16 file structure
set C_DEFINES=/DCETC

set copycmd=/Y
copy sources.cetc sources

build -cZ
copy .\obj%BUILD_ALT_DIR%\i386\*.sys ..
