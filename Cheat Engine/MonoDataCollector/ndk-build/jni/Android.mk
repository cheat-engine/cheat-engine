LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_CPPFLAGS += -fexceptions -U_FORTIFY_SOURCE

LOCAL_C_INCLUDES += -I$(LOCAL_PATH)/../../MonoDataCollector -I$(LOCAL_PATH)/../../../Common

LOCAL_DISABLE_FATAL_LINKER_WARNINGS := true

ifeq ($(TARGET_ARCH_ABI),armeabi-v7a)
    PREFIX := arm
else ifeq ($(TARGET_ARCH_ABI),arm64-v8a)
    PREFIX := aarch64
else ifeq ($(TARGET_ARCH_ABI),x86)
    PREFIX := x86
else
    PREFIX := x86_64
endif

LOCAL_MODULE := MonoDataCollector-$(PREFIX)

LOCAL_SRC_FILES :=  ../../../Common/Pipe.cpp \
                    ../../MonoDataCollector/CMemStream.cpp \
                    ../../MonoDataCollector/linuxport.cpp \
                    ../../MonoDataCollector/MonoDataCollector.cpp \
                    ../../MonoDataCollector/PipeServer.cpp

LOCAL_LDLIBS := -llog -lz
include $(BUILD_SHARED_LIBRARY)
