################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../Binary/binary_struct.f90 

OBJS += \
./Binary/binary_struct.o 


# Each subdirectory must supply rules for building sources it contributes
Binary/%.o: ../Binary/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

Binary/binary_struct.o: ../Binary/binary_struct.f90


