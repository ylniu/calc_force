
@set pwd=%cd%

cd ..\program

@make

@cd %pwd%

..\program\main_force input.txt
