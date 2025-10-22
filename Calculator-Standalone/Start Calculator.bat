@echo off
title Enhanced Calculator - Double-click to start!
color 0A
echo.
echo ===============================================
echo    Enhanced Calculator v1.0
echo    Supports: +, -, *, /, decimals, variables
echo ===============================================
echo.
echo Starting calculator...
echo.
"%~dp0Calculator.exe"
echo.
echo ===============================================
echo Calculator session ended.
echo Press any key to close this window...
echo ===============================================
pause >nul