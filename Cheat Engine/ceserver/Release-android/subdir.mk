################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../api.c \
../ceserver.c \
../ceservertest.c \
../extensionfunctions.c \
../extensionloader.c \
../porthelp.c \
../symbols.c \
../threads.c 

OBJS += \
./api.o \
./ceserver.o \
./ceservertest.o \
./extensionfunctions.o \
./extensionloader.o \
./porthelp.o \
./symbols.o \
./threads.o 

C_DEPS += \
./api.d \
./ceserver.d \
./ceservertest.d \
./extensionfunctions.d \
./extensionloader.d \
./porthelp.d \
./symbols.d \
./threads.d 


# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cross GCC Compiler'
	arm-linux-androideabi-gcc -DHAS_LINUX_USER_H -O3 -Wall -c -fmessage-length=0 -fPIE -pie -fPIC -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


