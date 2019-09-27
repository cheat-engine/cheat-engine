LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
LOCAL_DISABLE_FATAL_LINKER_WARNINGS := true
LOCAL_MODULE    := ceserver-extension
LOCAL_SRC_FILES := ../../server.c ../../speedhack.c
LOCAL_LDLIBS := -llog  -lz
include $(BUILD_SHARED_LIBRARY)
