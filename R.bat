@echo off

echo Current substitutions in effect:
subst

set drive=%~n0

set /p drive_n=Press Enter to set drive to %drive% or type a letter.

set drive="%drive%:"
set drive_n="%drive_n%:"

if not %drive_n% == ":" (
	set drive=%drive_n%
)

echo deleting "%drive%" substitutions
subst %drive% /d >nul

rem new substitutions:
rem %drive% is where the data is
subst %drive% "%CD%"

echo .
echo new substitutions:
subst
echo .
pause

