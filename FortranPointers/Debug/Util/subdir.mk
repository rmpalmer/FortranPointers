################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
F90_SRCS += \
../Util/timer_class.f90 

OBJS += \
./Util/timer_class.o 


# Each subdirectory must supply rules for building sources it contributes
Util/%.o: ../Util/%.f90
	@echo 'Building file: $<'
	@echo 'Invoking: GNU Fortran Compiler'
	gfortran -funderscoring -O0 -g -Wall -c -fmessage-length=0 -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '

Util/timer_class.o: ../Util/timer_class.f90


