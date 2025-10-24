@echo off
title JavaScript-Style Calculator v2.0
color 0B
echo.
echo ===============================================
echo    🚀 JavaScript-Style Calculator v2.0
echo    Multi-line Code Editor with Real Syntax!
echo ===============================================
echo.
echo Supported Features:
echo   ✓ Multi-line code blocks: { ... }
echo   ✓ Variable declarations: let x = 10
echo   ✓ Constants: const pi = 3.14159
echo   ✓ Assignments: result = x + y
echo   ✓ Integer and decimal numbers
echo   ✓ Full arithmetic: +, -, *, /
echo.
echo ===============================================
echo Starting calculator...
echo ===============================================
echo.

REM Build the project if needed and run
cd /d "%~dp0"
stack exec js-add

echo.
echo ===============================================
echo Calculator session ended.
echo Press any key to close this window...
echo ===============================================
pause >nul