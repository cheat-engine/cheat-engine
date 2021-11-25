################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../api.c \
../ceserver.c \
../ceservertest.c \
../context.c \
../extensionfunctions.c \
../extensionloader.c \
../native-api.c \
../porthelp.c \
../symbols.c \
../threads.c 

OBJS += \
./api.o \
./ceserver.o \
./ceservertest.o \
./context.o \
./extensionfunctions.o \
./extensionloader.o \
./native-api.o \
./porthelp.o \
./symbols.o \
./threads.o 

C_DEPS += \
./api.d \
./ceserver.d \
./ceservertest.d \
./context.d \
./extensionfunctions.d \
./extensionloader.d \
./native-api.d \
./porthelp.d \
./symbols.d \
./threads.d 


# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.c
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C Compiler'
	gcc -D_FILE_OFFSET_BITS=64 -D_LARGEFILE64_SOURCE -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


